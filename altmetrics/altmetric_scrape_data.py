from bs4 import BeautifulSoup
import os
import pandas as pd
import tomli

HOME_DIR = os.path.abspath(os.path.join(os.getcwd(), os.pardir))
DATA_DIR = os.path.join(HOME_DIR, 'data')

with open (os.path.join(HOME_DIR, 'bibliometric_report_params.toml'), mode = 'rb') as f:
    CONFIG = tomli.load(f)
PROJECT_NAME: str = CONFIG['project']['name']

# Convert a list to a dictionary
def Convert(a):
    it = iter(a)
    res_dct = dict(zip(it, it))
    return res_dct

with open(r"altmetric_badges_generated.html", "r") as f:
    soup = BeautifulSoup(f, "html.parser")

# Collect everything needed for generating outputs
images = soup.findAll('img')
result = soup.find_all(lambda tag: tag.name == 'div' and tag.get('class') == ['altmetric-embed'])

df_results = pd.DataFrame()

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

    text_store = []
    for t in text:
        text_format = t.rsplit(' ', 1)
        text_store.append(text_format[0])
        text_store.append(int(text_format[1]))
    
    # Convert and format to a dataframe
    text_dict = Convert(text_store)
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

data_directory = os.getcwd() + '\\data'
df_results.to_csv(os.path.join(DATA_DIR, "".join([PROJECT_NAME, "_altmetric_data.csv"])), index = False)