# TODO Add comments throughout to explain what each section is returning and to which section of the report it relates to

import dimcli
from dimcli.utils import *
import numpy as np
import os
import pandas as pd
import tomli
import json

# * Project parameters
with open ('bibliometric_report_params.toml', mode = 'rb') as f:
    CONFIG = tomli.load(f)
PROJECT_NAME: str = CONFIG['project']['name']
RESEARCH_UNIT: str = CONFIG['unit']['research_unit']
MIN_YEAR: int = CONFIG['years']['minimum']
MAX_YEAR: int = CONFIG['years']['maximum']
REFERENCE_YEAR: int = CONFIG['years']['reference']
LIMIT: int = CONFIG['search']['limit']
SKIP: int = CONFIG['search']['skip']

# * Housekeeping
HOME_DIR: str = os.getcwd()
DATA_DIR: str = os.path.join(HOME_DIR, 'data')
CITING_PUBLICATIONS: str = os.path.join(DATA_DIR, 'citing_publications')
if not os.path.isdir(CITING_PUBLICATIONS):
    os.mkdir(CITING_PUBLICATIONS)
    print('created folder : ', CITING_PUBLICATIONS)
else:
    print(CITING_PUBLICATIONS, ': folder already exists.')

GRIDID = 'grid.6268.a'

# * Functions
def format_categories(df: pd.DataFrame, output: str, name: str) -> pd.DataFrame:
    
    category = 'category_' + name
    output = output + '_id'

    df_output = (
         df
        .filter([output, category])
        .explode(category)
    )
    df_output = pd.json_normalize(df_output[category]).set_index(df_output[output])
    df_output = (
       df_output
       .drop(columns=['id'])
       .reset_index()
       .rename(columns={'name': name})
)
    return df_output

# * Log into Dimensions using dsl.ini file
dimcli.login()
dsl = dimcli.Dsl()

# * Load staff list
df_staff_list = pd.read_csv(os.path.join(DATA_DIR, 'faculty.csv'))
df_staff_list = df_staff_list[df_staff_list['level_2_long_desc'] == RESEARCH_UNIT]
df_staff_list = df_staff_list[df_staff_list['researcher_id'].notnull()]

# * Researchers
print('Collecting data on researchers')
df_researchers = dsl.query(f"""
                  search researchers
                    where id in {json.dumps(list(df_staff_list['researcher_id']))}
                    return researchers[current_research_org+first_publication_year+orcid_id+first_name+last_name+id+obsolete]
                    limit {LIMIT} skip {SKIP}
                """).as_dataframe().rename(columns = {'id' : 'researcher_id'})

df_researchers = (
    df_researchers
    .filter(['first_name', 'last_name', 'first_publication_year', 'current_research_org.name', 'researcher_id', 'obsolete', 'orcid_id'])
    .assign(first_publication_year = lambda df: df['first_publication_year'].astype(int),
            academic_age = REFERENCE_YEAR - df_researchers.first_publication_year,
            full_name = lambda df: df[['first_name', 'last_name']].apply(' '.join, axis = 1) 
    )
    .set_index('researcher_id')
    .reset_index()
)

df_researchers.to_csv(os.path.join(DATA_DIR, "".join([PROJECT_NAME, "_researchers.csv"])), index = False)

dict_researchers = dict(zip(df_researchers.researcher_id, df_researchers.full_name))

# * Publications: details
# TODO Chnage the way data is collected to use publications, author, and affiliations data frames
# ! Do not use dsl.query_iterative here because it drops duplicates needed for accurate publications counts per researcher
# TODO Add fields to get the data required for author positions
print('Collecting data on publications')
df_publications_details = dsl.query(f"""search publications 
                                where (researchers.id in {json.dumps(list(df_researchers['researcher_id']))} and 
                                year in [{MIN_YEAR}:{MAX_YEAR}] and research_orgs = "grid.6268.a") 
        return publications[id+researchers+doi+year+type+open_access+field_citation_ratio+times_cited+source_title+publisher]
        limit {LIMIT} skip {SKIP}
        """).as_dataframe()

df_publications_details = df_publications_details.explode('researchers')

df_publications_researchers = (pd.json_normalize(df_publications_details['researchers'])
                     .set_index(df_publications_details['id'])
                     .rename(columns={'id':'researcher_id'})
                     .reset_index()                     
)

df_publications_details = df_publications_details.merge(df_publications_researchers, on = 'id')

df_publications_details = (
    df_publications_details
    .rename(columns = {'id': 'publication_id', 'source_title.title': 'source_title'})
    .filter(['publication_id', 'doi', 'open_access', 'times_cited', 'type', 'year', 'source_title', 'field_citation_ratio', 'researcher_id', 'publisher'])
    .pipe(lambda df: df[df['type'] != 'preprint'])
    .explode('open_access')
    .pipe(lambda df: df[df['open_access'] != 'oa_all'])
    .assign(
        year = lambda df: df['year'].astype(int),
        times_cited = lambda df: df['times_cited'].astype(int),
        full_name = lambda df: df['researcher_id'].map(dict_researchers)
    )
    .set_index('researcher_id')
    .reset_index()
    .dropna(subset=['full_name'])
    .drop_duplicates()
)

df_publications_details.to_csv(os.path.join(DATA_DIR, "".join([PROJECT_NAME, "_publications_details.csv"])), index = False)

# TODO What other dictionaries should I export for networks
dict_output_type = dict(zip(df_publications_details.publication_id, df_publications_details.type))
dict_output_year = dict(zip(df_publications_details.publication_id, df_publications_details.year))

# * Split the publications data into chunks (2^9)
split = int(np.ceil(df_publications_details.shape[0]/512))
df_publications_details_split = np.array_split(df_publications_details, split)

# * Authors: positions and corresponding
df_publications = pd.DataFrame()
df_authors = pd.DataFrame()
df_affiliations = pd.DataFrame()
for i in range(len(df_publications_details_split)):
    pubs = df_publications_details_split[i]['publication_id'].drop_duplicates()
    results = dsl.query(f"""search publications
           where id in {json.dumps(list(pubs))}
           return publications[id+open_access+type+year+authors+authors_count]
           limit {LIMIT} skip {SKIP}"""
    )
    
    df_pubs_temp = results.as_dataframe()
    df_authors_temp = results.as_dataframe_authors()
    df_affiliations_temp = results.as_dataframe_authors_affiliations()

    df_publications = pd.concat([df_publications, df_pubs_temp])
    df_authors = pd.concat([df_authors, df_authors_temp])
    df_affiliations = pd.concat([df_affiliations, df_affiliations_temp])

# TODO This needs tidying up into a single step
df_publications = df_publications[df_publications['type'] == 'article']
df_publications = df_publications.explode('open_access')
df_publications = df_publications[df_publications['open_access'] != 'oa_all']

df_autpub = pd.merge(
    left=df_authors,
    right=df_publications,
    left_on='pub_id',
    right_on='id',
    how='inner' # left
)

# TODO This needs tidying up into a single step
df_autpub = df_autpub.drop(columns = ['authors', 'affiliations'])
df_autpub["author_name"] = df_autpub["last_name"] + [", "] + df_autpub["first_name"]
df_autpub['author_number'] = df_autpub.groupby(['pub_id']).cumcount()+1;

# ! Untested
# df_autpub = (
#     df_autpub
#     .drop(columns = ['authors', 'affiliations'])
#     .assign(
#         full_name = lambda df: df["last_name"] + [", "] + df["first_name"],
#         author_number = lambda df: df.groupby(['pub_id']).cumcount()+1
#     )
# )

df_autpub['AuthorCategory'] = np.where(
     df_autpub['author_number']==1, 'first_author',
         np.where(
            df_autpub['author_number']==df_autpub['authors_count'],'last_author',''
         )
)

# Limit to Univeristy of Bradford authors only
brad_authors = df_autpub[df_autpub['raw_affiliation'].astype(str).str.contains('University of Bradford|Bradford University', case=False)]
brad_authors = brad_authors.drop(columns=['id', 'raw_affiliation', 'first_name', 'last_name', 'orcid'])
# corresponding data type is string and should be bool
brad_authors['corresponding'] = brad_authors['corresponding'].astype(bool)

brad_authors.to_csv(os.path.join(DATA_DIR, "".join([PROJECT_NAME, "_author_positions.csv"])), index = False)

df_affiliations = df_affiliations.filter(['aff_id', 'aff_name', 'pub_id'])

# * Publications: categories
df_publications_categories = pd.DataFrame()
for i in range(len(df_publications_details_split)):
    pubs = df_publications_details_split[i]['publication_id'].drop_duplicates()
    results = dsl.query(f"""search publications
           where id in {json.dumps(list(pubs))}
           return publications[id+category_uoa+mesh_terms+category_sdg+category_for_2020+category_rcdc+category_hra+category_hrcs_hc+category_hrcs_rac+category_icrp_cso]
           limit {LIMIT} skip {SKIP}"""
    ).as_dataframe()
    df_publications_categories = pd.concat([df_publications_categories, results])

df_publications_categories = df_publications_categories.rename(columns={'id':'publication_id'})

# UoA
df_publications_categories_uoa = format_categories(df_publications_categories, 'publication', 'uoa')
df_publications_categories_uoa = df_publications_categories_uoa.assign(type = df_publications_categories_uoa['publication_id'].map(dict_output_type))
df_publications_categories_uoa.to_csv(os.path.join(DATA_DIR, "".join([PROJECT_NAME, "_publications_uoa.csv"])), index = False)

# FoR 2020
df_publications_categories_for_2020 = format_categories(df_publications_categories, 'publication', 'for_2020')
df_publications_categories_for_2020 = df_publications_categories_for_2020.assign(type = df_publications_categories_for_2020['publication_id'].map(dict_output_type))
df_publications_categories_for_2020[['for_2020_code','for_2020']] = df_publications_categories_for_2020['for_2020'].str.split(pat=' ', n=1, expand=True)
df_publications_categories_for_2020.to_csv(os.path.join(DATA_DIR, "".join([PROJECT_NAME, "_publications_for_2020.csv"])), index = False)

# MeSH 
df_publications_categories_mesh = (
    df_publications_categories
    .filter(['publication_id', 'mesh_terms'])
    .explode('mesh_terms')
    .assign(type = lambda df: df['publication_id'].map(dict_output_type))
)
df_publications_categories_mesh.to_csv(os.path.join(DATA_DIR, "".join([PROJECT_NAME, "_publications_mesh.csv"])), index = False)

# SGD
df_publications_categories_sdg = format_categories(df_publications_categories, 'publication', 'sdg')
df_publications_categories_sdg = df_publications_categories_sdg.assign(type = df_publications_categories_sdg['publication_id'].map(dict_output_type))
df_publications_categories_sdg[['sdg_code','sdg']] = df_publications_categories_sdg['sdg'].str.split(pat=' ', n=1, expand=True)
df_publications_categories_sdg.to_csv(os.path.join(DATA_DIR, "".join([PROJECT_NAME, "_publications_sdg.csv"])), index = False)

# RCDC
df_publications_categories_rcdc = format_categories(df_publications_categories, 'publication', 'rcdc')
df_publications_categories_rcdc = df_publications_categories_rcdc.assign(type = df_publications_categories_rcdc['publication_id'].map(dict_output_type))
df_publications_categories_rcdc.to_csv(os.path.join(DATA_DIR, "".join([PROJECT_NAME, "_publications_rcdc.csv"])), index = False)

# HRCS HC
df_publications_categories_hrcs_hc = format_categories(df_publications_categories, 'publication', 'hrcs_hc')
df_publications_categories_hrcs_hc = df_publications_categories_hrcs_hc.assign(type = df_publications_categories_hrcs_hc['publication_id'].map(dict_output_type))
df_publications_categories_hrcs_hc.to_csv(os.path.join(DATA_DIR, "".join([PROJECT_NAME, "_publications_hrcs_hc.csv"])), index = False)
    
# HRCS RAC
df_publications_categories_hrcs_rac = format_categories(df_publications_categories, 'publication', 'hrcs_rac')
df_publications_categories_hrcs_rac = df_publications_categories_hrcs_rac.assign(type = df_publications_categories_hrcs_rac['publication_id'].map(dict_output_type))
df_publications_categories_hrcs_rac.to_csv(os.path.join(DATA_DIR, "".join([PROJECT_NAME, "_publications_hrcs_rac.csv"])), index = False)

# ICRP CSO
df_publications_categories_icrp_cso = format_categories(df_publications_categories, 'publication', 'icrp_cso')
df_publications_categories_icrp_cso = df_publications_categories_icrp_cso.assign(type = df_publications_categories_icrp_cso['publication_id'].map(dict_output_type))
df_publications_categories_icrp_cso.to_csv(os.path.join(DATA_DIR, "".join([PROJECT_NAME, "_publications_icrp_cso.csv"])), index = False)

# * Collaborating research organisations 
print('Collecting data on research organisations')
res_publications_organisations = pd.DataFrame()
for i in range(len(df_publications_details_split)):
    pubs = df_publications_details_split[i]['publication_id'].drop_duplicates()
    results = dsl.query(f"""search publications
           where id in {json.dumps(list(pubs))}
           return publications[id+research_orgs]
           limit {LIMIT} skip {SKIP}"""
    ).as_dataframe()
    res_publications_organisations = pd.concat([res_publications_organisations, results])

df_research_organisations = res_publications_organisations.explode('research_orgs')
df_research_organisations = pd.json_normalize(df_research_organisations['research_orgs']).set_index(df_research_organisations['id'])
df_research_organisations = (
    df_research_organisations
    .filter(['id', 'country_name', 'name', 'types'])
    .explode('types')
    .rename(columns={'id': 'organisation_id', 'name': 'organisation_name', 'types': 'organisation_type'})
    .reset_index()
    .rename(columns = {'id': 'publication_id'})
    .assign(
        type = lambda df: df['publication_id'].map(dict_output_type),
        year = lambda df: df['publication_id'].map(dict_output_year)
    )
)

df_research_organisations = pd.merge(
    left=df_research_organisations,
    right=df_affiliations,
    left_on='publication_id',
    right_on='pub_id',
    how='inner' # left
)

df_research_organisations = (
    df_research_organisations
    .pipe(lambda df: df[df['organisation_name'] == df['aff_name']])
    .filter(['publication_id', 'organisation_id', 'country_name', 'organisation_name', 'organisation_type', 'type', 'year'])
)

df_research_organisations.to_csv(os.path.join(DATA_DIR, "".join([PROJECT_NAME, "_research_organisations.csv"])), index = False)

# * Citing publications
print('Collecting data on citing publications')
df_cit_pubs = pd.DataFrame()
for i in range(len(df_publications_details_split)):
    pubs = df_publications_details_split[i]['publication_id'].drop_duplicates()
    results = dsl.query_iterative(f"""search publications
           where reference_ids in {json.dumps(list(pubs))}
           return publications[id+doi+year+authors+type+journal_title_raw+research_orgs+category_for_2020+publisher]
           """
    ).as_dataframe()
    df_cit_pubs = pd.concat([df_cit_pubs, results])

# Citing publications details
df_cit_pubs_details = (
    df_cit_pubs
    .rename(columns={'id':'publication_id'})
    .filter(['publication_id', 'doi', 'journal_title_raw', 'type', 'year', 'publisher'])
    .pipe(lambda df: df[df['type'] != 'preprint'])
).to_csv(os.path.join(CITING_PUBLICATIONS, "".join([PROJECT_NAME, "_citing_pubs_details.csv"])), index = False)

# Citing organisations details
df_cit_pubs_orgs = df_cit_pubs.explode('research_orgs')
df_cit_pubs_orgs = pd.json_normalize(df_cit_pubs_orgs['research_orgs']).set_index(df_cit_pubs_orgs['id'])
df_cit_pubs_orgs = (
    df_cit_pubs_orgs
    .explode('types')
    .filter(['country_name', 'name', 'types'])
    .reset_index()
    .rename(columns={'id':'publication_id'})
)

# Split up citing publications into smaller chunks for processing
split = int(np.ceil(df_cit_pubs.shape[0]/2048))
df_cit_pubs_split = np.array_split(df_cit_pubs, split)

df_output = pd.DataFrame()
for i in range(len(df_cit_pubs_split)):
        
    # Citing organisations details
    df_cit_pubs_orgs = df_cit_pubs.explode('research_orgs')
    df_cit_pubs_orgs = pd.json_normalize(df_cit_pubs_orgs['research_orgs']).set_index(df_cit_pubs_orgs['id'])
    df_cit_pubs_orgs = (
        df_cit_pubs_orgs
        .explode('types')
        .filter(['country_name', 'name', 'types'])
        .reset_index()
        .rename(columns={'id':'publication_id', 'country_name':'country'})
    )
       
    df_output = pd.concat([df_output, df_cit_pubs_orgs])
    
df_citing_countries = (
    df_output
    .groupby('country')
    .nunique('publication_id')
    .reset_index()
)
df_citing_countries.to_csv(os.path.join(CITING_PUBLICATIONS, "".join([PROJECT_NAME, "_citing_countries.csv"])), index = False)

df_citing_orgs = (
     df_output
    .groupby('name')
    .nunique('publication_id')
    .reset_index()
)
df_citing_orgs.to_csv(os.path.join(CITING_PUBLICATIONS, "".join([PROJECT_NAME, "_citing_organisations.csv"])), index = False)

df_citing_orgs_types = (
    df_output
    .groupby('types')
    .nunique('publication_id')
    .reset_index()
)
df_citing_orgs_types.to_csv(os.path.join(CITING_PUBLICATIONS, "".join([PROJECT_NAME, "_citing_org_types.csv"])), index = False)

print('Data collection completed.')