import os
import pandas as pd
import tomli

# * Load the dois we want to produce badges for
HOME_DIR = os.getcwd()
ALTMETRICS_DIR = os.path.join(os.getcwd(), "altmetrics")
DATA_DIR = os.path.join(HOME_DIR, 'data')

with open (os.path.join(HOME_DIR, 'bibliometric_report_params.toml'), mode = 'rb') as f:
    CONFIG = tomli.load(f)
PROJECT_NAME: str = CONFIG['project']['name']

publications = os.path.join(DATA_DIR, "".join([PROJECT_NAME, "_publications_details.csv"]))
df = pd.read_csv(publications)
df = (
     df
     .filter(['doi'])
     .drop_duplicates()
     .astype(str)
)
dois = df['doi'].to_list()

os.chdir(ALTMETRICS_DIR)

# * Construct the html file
with open('altmetric_html_top.html', 'r') as f:
    html_string = f.read()

doi_html = []
for doi in dois:
    text = ''.join(['\n<div data-condensed="true" data-badge-details="right" data-badge-type="donut" data-doi="', doi, '" data-hide-no-mentions="true" class="altmetric-embed"></div>'])
    html_string = html_string + text

html_string = html_string + '\n</body>\n</html>'

with open(r'altmetric_badges.html', 'w') as f:
        f.write(html_string)