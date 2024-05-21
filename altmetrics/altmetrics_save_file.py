'''
The output of this code is an HTML file named "altmetric_badges_generated.html" 
saved in the "Z:\university_of_bradford\rais_reports\bibliometric_reports\development\altmetrics" directory. 
This file contains the HTML source code of the web page located at the URL 
"Z:\university_of_bradford\rais_reports\bibliometric_reports\development\altmetrics\altmetric_badges.html".

The code follows a straightforward logic flow: it initializes the Edge web browser, 
navigates to the desired URL, retrieves the HTML source code, saves it to a file, 
and then closes the browser.
'''
# TODO sort out file paths relative to the project
# TODO update git
from selenium import webdriver

import os

ALTMETRICS_DIR = os.path.join(os.getcwd(), 'altmetrics')

edge_driver = webdriver.Edge()
edge_driver.implicitly_wait(0.5)
edge_driver.get(os.path.join(ALTMETRICS_DIR, 'altmetric_badges.html'))
with open (os.path.join(ALTMETRICS_DIR, 'altmetric_badges_generated.html', 'w', encoding='utf-8')) as file:
    file.write(edge_driver.page_source)
edge_driver.quit()