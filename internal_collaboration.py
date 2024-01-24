import dimcli
from dimcli.utils import * 
import json
import pandas as pd
import tomli

dimcli.login()
dsl = dimcli.Dsl()

with open ('bibliometric_report_params.toml', mode = 'rb') as f:
    CONFIG = tomli.load(f)
PROJECT_NAME: str = CONFIG['project']['name']
RESEARCH_UNIT: str = CONFIG['unit']['research_unit']
MIN_YEAR: int = CONFIG['years']['minimum']
MAX_YEAR: int = CONFIG['years']['maximum']

HOME_DIR: str = os.getcwd()
DATA_DIR: str = os.path.join(HOME_DIR, 'data')

GRIDID = 'grid.6268.a'
  
df_staff_list = pd.read_csv(os.path.join(DATA_DIR, 'faculty.csv'))
df_staff_list = df_staff_list[df_staff_list['level_2_long_desc'] == RESEARCH_UNIT]

publications = dsl.query_iterative(f"""search publications
                                          where (researchers.id in {json.dumps(list(df_staff_list['researcher_id']))} and 
                                          year in [{MIN_YEAR}:{MAX_YEAR}])
                                        return publications[id+authors+category_for_2020]
                                   """)

pub_df = publications.as_dataframe()
aff_df = publications.as_dataframe_authors_affiliations()
auth_df = publications.as_dataframe_authors()

internal_authors = aff_df[aff_df['aff_id'] == GRIDID]
internal_authors[internal_authors['researcher_id'] != '']

internal_authorsf = internal_authors[internal_authors['researcher_id'] !=''][['pub_id', 'researcher_id']]
internal_authorsf.merge(internal_authorsf, on='pub_id')

edges = internal_authorsf.merge(internal_authorsf, on='pub_id').groupby(['researcher_id_x', 'researcher_id_y']).count()
edges.reset_index(inplace = True)

auth_details = auth_df.groupby('researcher_id').agg({'pub_id':'count', 'first_name':'max', 'last_name':'max'})
# TODO Assign researcher details to id by mapping a dictionary
auth_my_df = (
    auth_details.reset_index()
    .assign(full_name = lambda df: df[['first_name', 'last_name']].apply(' '.join, axis = 1)
)
)

dict_author_id = dict(zip(auth_my_df.researcher_id, auth_my_df.full_name))

my_edges_df = (
    edges
    .assign(
         researcher_x = lambda df: df['researcher_id_x'].map(dict_author_id),
         researcher_y = lambda df: df['researcher_id_y'].map(dict_author_id)
    )
)

edges = my_edges_df

new_cols = [col for col in edges.columns if col != 'pub_id'] + ['pub_id']
edges = edges[new_cols]

edges = edges[edges['researcher_id_x'] != edges['researcher_id_y']]

# edges.to_csv("edges_fohs.csv", index = False)
edges.to_csv(os.path.join(DATA_DIR, "".join([PROJECT_NAME, "_edges.csv"])), index = False)