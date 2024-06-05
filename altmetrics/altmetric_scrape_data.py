'''
This script extracts data from an HTML file containing information about research 
publications and their altmetric scores.

For each publication, it extracts the altmetric score and the individual metric 
values from the HTML.

It converts the extracted data into a pandas DataFrame, with each row representing 
a publication and columns for the DOI, altmetric score, and individual metric values.

The resulting DataFrame is saved as a CSV file in the "data" directory, using a 
filename that includes the project name specified in the configuration file.
'''
from bs4 import BeautifulSoup
import pandas as pd
import tomli

import os

HOME_DIR: str = os.getcwd()
ALTMETRICS_DIR: str = os.path.join(os.getcwd(), "altmetrics")
DATA_DIR: str = os.path.join(HOME_DIR, 'data')

with open (os.path.join(HOME_DIR, 'bibliometric_report_params.toml'), mode = 'rb') as f:
    CONFIG = tomli.load(f)
PROJECT_NAME: str = CONFIG['project']['name']

# Convert a list to a dictionary
def Convert(a) -> dict:
    it = iter(a)
    res_dct = dict(zip(it, it))
    return res_dct

os.chdir(ALTMETRICS_DIR)

with open(r"altmetric_badges_generated.html", "r") as f:
    soup = BeautifulSoup(f, "html.parser")
 
# Collect everything needed for generating outputs
images = soup.findAll('img')
result = soup.find_all(lambda tag: tag.name == 'div' and tag.get('class') == ['altmetric-embed'])

df_results: pd.DataFrame = pd.DataFrame()

for i in range(len(images)):
    # Grab the altmetric score
    score = images[i].attrs['alt'].split()[6]
    # Grab the doi
    doi = result[i].attrs['data-doi']
    # Grab the individual scores
    text = result[i].get_text()
    text = text.split('\n')
    text = [x.strip() for x in text]
    text = [x for x in text if x]
    text = [x for x in text if "details" not in x]
    text = [x.replace('(','').replace(')','') for x in text]

    text_store: list = []
    for t in text:
        text_format = t.rsplit(' ', 1)
        text_store.append(text_format[0])
        text_store.append(int(text_format[1]))
    
    # Convert and format to a dataframe
    text_dict: dict = Convert(text_store)
    df = pd.DataFrame.from_dict(text_dict, orient = 'index', columns = ['n'])
    df = (
        df
        .reset_index()
        .rename(columns = {'index': 'altmetric'})
        .assign(doi = doi)
        .set_index('doi')
        .reset_index()
        .pivot(index = "doi", columns = "altmetric", values = "n")
        .assign(Altmetric = score)
        .reset_index()
        .rename_axis(None, axis = 1)
        )
    df_results = pd.concat([df_results, df]).fillna('NA')

col = df_results.pop("Altmetric")
df_results.insert(1, col.name, col)

df_results.to_csv(os.path.join(DATA_DIR, "".join([PROJECT_NAME, "_altmetric_data.csv"])), index = False)