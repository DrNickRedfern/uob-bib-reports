import dimcli
from dimcli.utils import *
import os
import pandas as pd
import tomli

# * Project parameters
with open ('bibliometric_report_params.toml', mode = 'rb') as f:
    CONFIG = tomli.load(f)
PROJECT_NAME: str = CONFIG['project']['name']
MIN_YEAR: int = CONFIG['years']['minimum']
MAX_YEAR: int = CONFIG['years']['maximum']
CURRENT_YEAR: int = CONFIG['years']['current']
LIMIT: int = CONFIG['search']['limit']
SKIP: int = CONFIG['search']['skip']

# * Housekeeping
HOME_DIR = os.getcwd()
DATA_DIR = os.path.join(HOME_DIR, 'data')
CITING_PUBLICATIONS = os.path.join(DATA_DIR, 'citing_publications')
if not os.path.isdir(CITING_PUBLICATIONS):
    os.mkdir(CITING_PUBLICATIONS)
    print('created folder : ', CITING_PUBLICATIONS)
else:
    print(CITING_PUBLICATIONS, ': folder already exists.')
GRANTS_DIR = os.path.join(DATA_DIR, 'grants')
if not os.path.isdir(GRANTS_DIR):
    os.mkdir(GRANTS_DIR)
    print('created folder : ', GRANTS_DIR)
else:
    print(GRANTS_DIR, ': folder already exists.')

# * Log into Dimensions using dsl.ini file
dimcli.login()
dsl = dimcli.Dsl()

# * Load staff list
df_staff_list = pd.read_csv(os.path.join(DATA_DIR, 'staff_list_demo.csv'))
staff_ids = df_staff_list['researcher_id'].tolist()

# * Researchers
print('Collecting data on researchers')
df_researchers = pd.DataFrame()
query = """search researchers 
        where id = "{}" 
        return researchers[current_research_org+first_publication_year+orcid_id+first_name+last_name] 
        LIMIT {} SKIP {}"""

for i in staff_ids:
    researchers_results = dsl.query(query.format(i, LIMIT, SKIP)).as_dataframe()
    researchers_results['researcher_id'] = i
    df_researchers = pd.concat([df_researchers, researchers_results])

df_researchers = (
    df_researchers
    .filter(['first_name', 'last_name', 'first_publication_year', 'current_research_org.name', 'researcher_id'])
    .assign(first_publication_year = lambda df: df['first_publication_year'].astype(int),
            academic_age = CURRENT_YEAR - df_researchers.first_publication_year,
            full_name = lambda df: df[['first_name', 'last_name']].apply(' '.join, axis = 1) 
    )
    .set_index('researcher_id')
    .reset_index()
)

df_researchers.to_csv(os.path.join(DATA_DIR, "".join([PROJECT_NAME, "_researchers.csv"])), index = False)

dict_researchers = dict(zip(df_researchers.researcher_id, df_researchers.full_name))

# * Publications: details
# ! Do not use dsl.query_iterative here because it drops duplicates needed for accurate publications counts per researcher
print('Collecting data on publications')
df_publications_details = pd.DataFrame()
query = """search publications where (researchers.id = "{}" and year in [{}:{}] and research_orgs = "grid.6268.a") 
        return publications[id+doi+year+type+open_access+field_citation_ratio+times_cited+source_title+publisher] 
        LIMIT {} SKIP {}"""

for i in staff_ids:
    publication_results = dsl.query(query.format(i, MIN_YEAR, MAX_YEAR, LIMIT, SKIP)).as_dataframe()
    publication_results['researcher_id'] = i
    df_publications_details = pd.concat([df_publications_details, publication_results])

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
)

df_publications_details.to_csv(os.path.join(DATA_DIR, "".join([PROJECT_NAME, "_publications_details.csv"])), index = False)

dict_output_type = dict(zip(df_publications_details.publication_id, df_publications_details.type))
dict_output_year = dict(zip(df_publications_details.publication_id, df_publications_details.year))

# * Export the dois as a list to paste into JYUCITE
dois = list(df_publications_details['doi'])
with open(os.path.join(DATA_DIR, "".join([PROJECT_NAME, "_dois_list.txt"])), 'w') as file:
    file.write(', '.join(dois))

# * Publications: categories
# ! Use dsl.query_iterative here to save time - don't need duplicates when using metrics for publications
df_publications_categories = dsl.query_iterative(f"""
                 search publications
                    where id in {json.dumps(list(df_publications_details['publication_id']))}
                    return publications[id+category_uoa+mesh_terms+category_sdg+category_for_2020+category_rcdc]
""").as_dataframe().rename(columns={'id': 'publication_id'})

# Units of assessment
df_publications_categories_uoa = df_publications_categories.explode(['category_uoa'])
df_publications_categories_uoa = pd.json_normalize(df_publications_categories_uoa['category_uoa']).set_index(df_publications_categories_uoa['publication_id'])
df_publications_categories_uoa = (
    df_publications_categories_uoa
    .drop(columns=['id'])
    .reset_index()
    .rename(columns={'name': 'unit_of_assessment'})
    .assign(type = lambda df: df['publication_id'].map(dict_output_type))
)

df_publications_categories_uoa.to_csv(os.path.join(DATA_DIR, "".join([PROJECT_NAME, "_publications_uoa.csv"])), index = False)

# FoR 2020
df_publications_categories_for_2020 = (
    df_publications_categories
    .filter(['publication_id', 'category_for_2020'])
    .explode('category_for_2020')
)
df_publications_categories_for_2020 = pd.json_normalize(df_publications_categories_for_2020['category_for_2020']).set_index(df_publications_categories_for_2020['publication_id'])
df_publications_categories_for_2020 = (
    df_publications_categories_for_2020
    .drop(columns=['id'])
    .reset_index()
    .rename(columns={'name': 'for_2020'})
    .assign(type = lambda df: df['publication_id'].map(dict_output_type))
)
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

# Sustainable Development Goals
df_publications_categories_sdg = (
    df_publications_categories
    .filter(['publication_id', 'category_sdg'])
    .explode('category_sdg')
)
df_publications_categories_sdg = pd.json_normalize(df_publications_categories_sdg['category_sdg']).set_index(df_publications_categories_sdg['publication_id'])
df_publications_categories_sdg = (
    df_publications_categories_sdg
    .drop(columns=['id'])
    .reset_index()
    .rename(columns={'name': 'sdg'})
    .assign(type = lambda df: df['publication_id'].map(dict_output_type))
)
df_publications_categories_sdg[['sdg_code','sdg']] = df_publications_categories_sdg['sdg'].str.split(pat=' ', n=1, expand=True)

df_publications_categories_sdg.to_csv(os.path.join(DATA_DIR, "".join([PROJECT_NAME, "_publications_sdg.csv"])), index = False)

# RCDC
df_publications_categories_rcdc = (
    df_publications_categories
    .filter(['publication_id', 'category_rcdc'])
    .explode('category_rcdc')
)
df_publications_categories_rcdc = pd.json_normalize(df_publications_categories_rcdc['category_rcdc']).set_index(df_publications_categories_rcdc['publication_id'])
df_publications_categories_rcdc = (
    df_publications_categories_rcdc
    .drop(columns=['id'])
    .reset_index()
    .rename(columns={'name': 'RCDC'})
    .assign(type = lambda df: df['publication_id'].map(dict_output_type))
)

df_publications_categories_rcdc.to_csv(os.path.join(DATA_DIR, "".join([PROJECT_NAME, "_publications_rcdc.csv"])), index = False)

# * Collaborating research organisations 
print('Collecting data on research organisations')
res_publications_organisations = dsl.query_iterative(f"""
                    search publications
                    where id in {json.dumps(list(df_publications_details['publication_id']))}
                    return publications[id+research_orgs]""")  

df_research_organisations = pd.json_normalize(res_publications_organisations.publications).explode('research_orgs')
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

df_research_organisations.to_csv(os.path.join(DATA_DIR, "".join([PROJECT_NAME, "_research_organisations.csv"])), index = False)

# * Citing publications
print('Collecting data on citing publications')
query = """search publications 
            where reference_ids in {} 
            return publications[id+doi+year+authors+type+journal_title_raw+research_orgs+category_for_2020+publisher]
            LIMIT {} SKIP {}"""
df_cit_pubs = dsl.query(query.format(json.dumps(list(df_publications_details['publication_id'])), LIMIT, SKIP)).as_dataframe()
# Save the raw result for sanity
df_cit_pubs.to_csv(os.path.join(CITING_PUBLICATIONS, "".join([PROJECT_NAME, "_citing_pubs_raw.csv"])), index = False)

df_cit_pubs_details = (
    df_cit_pubs
    .filter(['id', 'doi', 'journal_title_raw', 'type', 'year'])
    .pipe(lambda df: df[df['type'] != 'preprint'])
).to_csv(os.path.join(CITING_PUBLICATIONS, "".join([PROJECT_NAME, "_citing_pubs_details.csv"])), index = False)

df_cit_pubs_orgs = df_cit_pubs.explode('research_orgs')
df_cit_pubs_orgs = pd.json_normalize(df_cit_pubs_orgs['research_orgs']).set_index(df_cit_pubs_orgs['id'])
df_cit_pubs_orgs = (
    df_cit_pubs_orgs
    .explode('types')
    .filter(['country_name', 'name', 'types'])
    .reset_index()
)

df_cit_pubs_orgs.to_csv(os.path.join(CITING_PUBLICATIONS, "".join([PROJECT_NAME, "_citing_pubs_orgs.csv"])), index = False)

df_cit_pubs_authors = (
    df_cit_pubs
    .explode('authors')
    .filter(['id', 'authors'])
    .set_index('id')
)
df_cit_pubs_authors = pd.json_normalize(df_cit_pubs_authors['authors'])
df_cit_pubs_authors = (
    df_cit_pubs_authors
    .explode('affiliations')
    .filter(['affiliations', 'first_name', 'last_name', 'researcher_id'])
    .assign(full_name = lambda df: df.first_name.str.cat(df.last_name, sep = '_'))
)
df_cit_pubs_authors_aff = pd.json_normalize(df_cit_pubs_authors['affiliations']).set_index(df_cit_pubs_authors['full_name'])
df_cit_pubs_authors_aff = (
    df_cit_pubs_authors_aff
    .reset_index()
    .filter(['full_name', 'country', 'name'])
)
df_cit_pubs_authors = df_cit_pubs_authors.drop(columns = 'affiliations')
df_cit_pubs_authors = pd.merge(df_cit_pubs_authors, df_cit_pubs_authors_aff, on = 'full_name')
df_cit_pubs_authors.to_csv(os.path.join(CITING_PUBLICATIONS, "".join([PROJECT_NAME, "_citing_pubs_authors.csv"])), index = False)

df_cit_pubs_orgs = df_cit_pubs_orgs.filter(['id', 'name', 'types'])
df_output = pd.merge(df_cit_pubs_authors, df_cit_pubs_orgs, on = 'name')
df_output = df_output.drop_duplicates()
df_output.to_csv(os.path.join(CITING_PUBLICATIONS, "".join([PROJECT_NAME, "_citing_pubs_comp_2.csv"])), index = False)

# * Grants
query = """search grants where (researchers in {} and active_year in [{}:{}] and research_orgs = "grid.6268.a")
return grants[id+title+active_year+end_date+start_year+research_orgs+funder_org_name+funder_org_countries+funding_gbp+project_numbers+investigators+researchers] 
limit {} skip {}"""
df_grants = dsl.query(query.format(json.dumps(list(df_researchers['researcher_id'])), MIN_YEAR, MAX_YEAR, LIMIT, SKIP)).as_dataframe()

# Summary of grants
df_grants_summary = (
    df_grants
    .rename(columns = {'id' : 'grant_id'})
    .filter(['grant_id', 'title', 'active_year', 'end_date', 'funder_org_countries', 'funder_org_name', 'funding_gbp', 'start_year', 'project_numbers'])
)

df_grants_project_numbers = df_grants_summary.explode('project_numbers')
df_grants_project_numbers = pd.json_normalize(df_grants_project_numbers['project_numbers']).set_index(df_grants_project_numbers['grant_id'])
df_grants_project_numbers = (
    df_grants_project_numbers
    .reset_index()
    .drop(columns = ['label'])
)
df_funder_org_countries = df_grants_summary.explode('funder_org_countries')
df_funder_org_countries = pd.json_normalize(df_funder_org_countries['funder_org_countries']).set_index(df_funder_org_countries['grant_id'])
df_funder_org_countries = df_funder_org_countries.reset_index().drop(columns=['id']).rename(columns = {'name' : 'funder_org_countries'})
df_grants_summary = df_grants_summary.drop(columns = ['project_numbers', 'funder_org_countries'])
df_grants_summary = pd.merge(df_grants_summary, df_grants_project_numbers, on = 'grant_id')
# df_grants_summary = df_grants_summary.drop(columns = ['funder_org_countries'])
df_grants_summary = pd.merge(df_grants_summary, df_funder_org_countries, on = 'grant_id')
df_grants_summary.to_csv(os.path.join(GRANTS_DIR, "".join([PROJECT_NAME, "_grants_details.csv"])), index = False)

# Investigators
df_grants_investigators = (
    df_grants
    .explode('investigators')
    .filter(['id', 'investigators'])
    .rename(columns = {'id' : 'grant_id'})
)
df_grants_investigators = pd.json_normalize(df_grants_investigators['investigators']).set_index(df_grants_investigators['grant_id'])
df_grants_investigators = df_grants_investigators.explode('affiliations')
df_grants_investigators = (
    df_grants_investigators
    .rename(columns={'id': 'researcher_id'})
    .reset_index()
)
df_investigators_affiliations = pd.json_normalize(df_grants_investigators['affiliations']).set_index(df_grants_investigators['researcher_id'])
df_investigators_affiliations = (
    df_investigators_affiliations
    .reset_index()
    .filter(['researcher_id', 'name'])
)
df_grants_investigators = df_grants_investigators.drop(columns=['affiliations'])
df_grants_investigators = pd.merge(df_grants_investigators, df_investigators_affiliations, on = 'researcher_id')
df_grants_investigators = (
    df_grants_investigators
    .drop_duplicates()
    .drop(columns=['middle_name'])
    .assign(full_name = lambda df: df[['first_name', 'last_name']].apply(' '.join, axis = 1))
)
df_grants_investigators.to_csv(os.path.join(GRANTS_DIR, "".join([PROJECT_NAME, "_grants_investigators.csv"])), index = False)

# Organisations
df_grants_orgs = (
    df_grants
    .explode('research_orgs')
    .filter(['id', 'research_orgs'])
    .rename(columns = {'id' : 'grant_id'})
)
df_grants_orgs = pd.json_normalize(df_grants_orgs['research_orgs']).set_index(df_grants_orgs['grant_id'])
df_grants_orgs = (
    df_grants_orgs
    .explode('types')
    .reset_index()
    .filter(['grant_id', 'country_name', 'name', 'types'])
)
df_grants_orgs.to_csv(os.path.join(GRANTS_DIR, "".join([PROJECT_NAME, "_grants_organisations.csv"])), index = False)

# Researchers
df_grants_researchers = (
    df_grants
    .explode('researchers')
    .filter(['id', 'researchers'])
    .rename(columns = {'id' : 'grant_id'})
)
df_grants_researchers = pd.json_normalize(df_grants_researchers['researchers']).set_index(df_grants_researchers['grant_id'])
df_grants_researchers = (
    df_grants_researchers
    .drop(columns=['orcid_id'])
    .reset_index()
    .rename(columns={'id' : 'researcher_id'})
    .explode('research_orgs')
    .pipe(lambda df: df[df['research_orgs'] == 'grid.6268.a'])
)
df_grants_researchers.to_csv(os.path.join(GRANTS_DIR, "".join([PROJECT_NAME, "_grants_researchers.csv"])), index = False)

# Categories
df_grants_categories = dsl.query_iterative(f"""
                 search grants
                    where id in {json.dumps(list(df_grants_summary['grant_id']))}
                    return grants[id+category_uoa+category_hra+category_hrcs_hc+category_hrcs_rac+category_icrp_cso+category_sdg+category_for_2020+category_rcdc]
""").as_dataframe().rename(columns={'id': 'grant_id'})

# Units of assessment
df_grants_categories_uoa = df_grants_categories.explode(['category_uoa'])
df_grants_categories_uoa = pd.json_normalize(df_grants_categories_uoa['category_uoa']).set_index(df_grants_categories_uoa['grant_id'])
df_grants_categories_uoa = (
    df_grants_categories_uoa
    .drop(columns=['id'])
    .reset_index()
    .rename(columns={'name': 'unit_of_assessment'})
)
df_grants_categories_uoa.to_csv(os.path.join(GRANTS_DIR, "".join([PROJECT_NAME, "_grants_categories_uoa.csv"])), index = False)

# FoR 2020
df_grants_categories_for_2020 = (
    df_grants_categories
    .filter(['grant_id', 'category_for_2020'])
    .explode('category_for_2020')
)
df_grants_categories_for_2020 = pd.json_normalize(df_grants_categories_for_2020['category_for_2020']).set_index(df_grants_categories_for_2020['grant_id'])
df_grants_categories_for_2020 = (
    df_grants_categories_for_2020
    .drop(columns=['id'])
    .reset_index()
    .rename(columns={'name': 'for_2020'})
)
df_grants_categories_for_2020[['for_2020_code','for_2020']] = df_grants_categories_for_2020['for_2020'].str.split(pat=' ', n=1, expand=True)
df_grants_categories_for_2020.to_csv(os.path.join(GRANTS_DIR, "".join([PROJECT_NAME, "_grants_categories_for_2020.csv"])), index = False)

# RCDC
df_grants_categories_rcdc = (
    df_grants_categories
    .filter(['grant_id', 'category_rcdc'])
    .explode('category_rcdc')
)
df_grants_categories_rcdc = pd.json_normalize(df_grants_categories_rcdc['category_rcdc']).set_index(df_grants_categories_rcdc['grant_id'])
df_grants_categories_rcdc = (
    df_grants_categories_rcdc
    .drop(columns=['id'])
    .reset_index()
    .rename(columns={'name': 'RCDC'})
)
df_grants_categories_rcdc.to_csv(os.path.join(GRANTS_DIR, "".join([PROJECT_NAME, "_grants_categories_rcdc.csv"])), index = False)

# SDG
df_grants_categories_sdg = (
    df_grants_categories
    .filter(['grant_id', 'category_sdg'])
    .explode('category_sdg')
)
df_grants_categories_sdg = pd.json_normalize(df_grants_categories_sdg['category_sdg']).set_index(df_grants_categories_sdg['grant_id'])
df_grants_categories_sdg = (
    df_grants_categories_sdg
    .drop(columns=['id'])
    .reset_index()
    .rename(columns={'name': 'sdg'})
)
df_grants_categories_sdg[['sdg_code','sdg']] = df_grants_categories_sdg['sdg'].str.split(pat=' ', n=1, expand=True)
df_grants_categories_sdg.to_csv(os.path.join(GRANTS_DIR, "".join([PROJECT_NAME, "_grants_categories_sdg.csv"])), index = False)

def health_categories(df: pd.DataFrame, category: str) -> pd.DataFrame:
    df_output = (
        df
        .filter(['grant_id', category])
        .explode(category)
    )
    df_output = pd.json_normalize(df_output[category]).set_index(df_output['grant_id'])
    df_output = df_output.rename(columns={'name' : category.split('_', 1)[1]}).reset_index()
    return df_output

# HRA
df_grants_categories_hra = health_categories(df_grants_categories, 'category_hra')
df_grants_categories_hra.to_csv(os.path.join(GRANTS_DIR, "".join([PROJECT_NAME, "_grants_categories_hra.csv"])), index = False)

# HRCS HC
df_grants_categories_hrcs_hc = health_categories(df_grants_categories, 'category_hrcs_hc')
df_grants_categories_hrcs_hc.to_csv(os.path.join(GRANTS_DIR, "".join([PROJECT_NAME, "_grants_categories_hrcs_hc.csv"])), index = False)

# HRCS RAC
df_grants_categories_hrcs_rac = health_categories(df_grants_categories, 'category_hrcs_rac')
df_grants_categories_hrcs_rac.to_csv(os.path.join(GRANTS_DIR, "".join([PROJECT_NAME, "_grants_categories_hrcs_rac.csv"])), index = False)