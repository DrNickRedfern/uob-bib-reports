import pandas as pd

# * Load the dois we want to produce badges for
# dois = ['10.1163/22134913-bja10046', '10.2190/em.32.2.g']
df = pd.read_csv('faculty_of_health_sciences_publications_details.csv')
dois = df['doi'].to_list()

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