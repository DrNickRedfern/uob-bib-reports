'''
The code generates an HTML file that contains Altmetric badges for a list of 
Digital Object Identifiers (DOIs) associated with publications.

Altmetric badges are visual representations of the online attention and 
engagement received by scholarly publications. These badges provide a quick 
overview of the impact and reach of a publication based on various metrics, 
such as mentions on social media, news outlets, and other online sources.

The code follows a straightforward logic flow, reading the necessary input data, 
transforming it into a list of DOIs, and then constructing an HTML file with 
embedded Altmetric badges for each DOI.

The output of this code is an HTML file named altmetric_badges.html, which 
contains the necessary HTML structure and embedded Altmetric badges for each 
DOI in the input CSV file.
'''
import os
import pandas as pd
import tomli

# * Load the dois we want to produce badges for
HOME_DIR: str = os.getcwd()
ALTMETRICS_DIR: str = os.path.join(HOME_DIR, "altmetrics")
DATA_DIR: str = os.path.join(HOME_DIR, 'data')

with open(os.path.join(HOME_DIR, 'bibliometric_report_params.toml'), mode='rb') as f:
    CONFIG = tomli.load(f)
PROJECT_NAME: str = CONFIG['project']['name']

publications: str = os.path.join(DATA_DIR, "".join([PROJECT_NAME, "_publications_details.csv"]))
df: pd.DataFrame = pd.read_csv(publications)
df = (df
     .filter(['doi'])
     .drop_duplicates()
     .astype(str)
)
dois: list[str] = df['doi'].to_list()

os.chdir(ALTMETRICS_DIR)

# * Construct the html file
with open('altmetric_html_top.html', 'r') as file:
    html_string: str = file.read()

for doi in dois:
    text: str = ''.join(['\n<div data-condensed="true" data-badge-details="right" data-badge-type="donut" data-doi="', doi, '" data-hide-no-mentions="true" class="altmetric-embed"></div>'])
    html_string = html_string + text

html_string = html_string + '\n</body>\n</html>'

with open('altmetric_badges.html', 'w') as file:
        file.write(html_string)