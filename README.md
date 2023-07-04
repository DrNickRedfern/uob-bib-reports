# RaIS Bibliometric reports

This README file describes the workflow for generating bibliometric reports using data from Dimensions and Altmetrics.

---

## Folder structure

The folder structure for the project is displayed below.

If any files are missing or in the wrong location the workflow described below will **not** function correctly.

---

## Workflow

**Please note that all paths are relative to the project folder. The project folder is the folder containing the `.rproj` file.**

### Data preparation

Dimensions can be searched based on researcher ids (e.g., Dimensions id, ORCiD) or output ids (Dimensions id, DOI, grant number, etc.). It is assumed that most reports will be produced for a defined set of researchers. If the result set is to be generated for a defined set of publications then the workflow will need to be adapted to take this into account, though the analysis will only be minimally affected.

**1. Format a list of researcher ids with the research unit and name of each researcher and their disambiguated Dimensions researcher id and save as a `csv` file.**

> Researchers without a disambiguated id may also be included in a report by re-searching across a set of outputs produced by the initial search for outputs using the University of Bradford grid number; however, there will not be any individual data for researchers identified int his way and so it will not be possible to calculate relevant statistics for them (e.g., academic age).

### Create the project folder and files

### Set parameters

**2. Edit the `bibliometric_report_params.toml` file to set the required parameters searching the Dimensions database and producing the reports.**

Parameters

- `project`
  - `name`: `string` used to format file names associated with the project
- `unit`
  - `research_unit`: `string` used to display the name of the research unit under analysis in the final report
- `years`
  - `minimum`: `int` the lower bound for the Dimensions query - also used in the final report
  - `maximum`: `int` the upper bound for the Dimensions query - also used in the final report
  - `reference`: `int` the reference year for calculating academic age - almost always the current year
- `look`
  - `palette`: `string` set the palette to be used in the report - will be some value from Rcolorbrewer's list of colourblind safe palettes: YlOrRd, YlGnBu, RdYlBu, PuBuGn
- `search`
  - `limit`: `int` parameter for Dimensions query
  - `skip`: `int` parameter for Dimensions query

### Get data from Dimensions

**3. Run the `dimensions_get_data.py` file to download data from the Dimensions database.**

### Get Altmetric data

Navigate to the `altmetric` folder in the project folder.

**4. Run the `compile_altmetric_html_file.py` file.**

**5. Open the `html` file and save as `generated_altmetric_badges.html`.**

**6. Run the `altmetric_scrape_data.py` file.**

**7. Run the `altmetric_data.R` file**.

### Get Co-citation Percentile Rank data

Navigate to the `co-citation percentile rank` folder in the project folder.

**8. Open the file `dois.txt` and copy the text to the clipboard.**

**9. Navigate to the [JYUCite](https://oscsolutions.cc.jyu.fi/jyucite/) website. Paste the dois into the search box and click `GET!`. This search will take several minutes to complete. Once finished, click `Download as CSV` to save the results as a `csv` file to the project data directory. Change the file name to comply with the standard formatting of file names for the project - NB: this step is required for reading the data when generating the report.**

### Generate the bibliography

Navigate to the `bibliography` folder in the project folder.

**10. Run the `generate_bibliography.R` file to download bibliography information from Crossref. Copy the `.bib` file to the main project directory, so that it is in the same folder as the `.rproj` file.**

> Not all items may be discoverable this way and so it may be necessary to manually add bibtex entries to the `bibliography.bib` file. If there is a large number of files that need to be added, this can be done more efficiently by loading the `bibliography.bib` file into Zotero and adding new entries using the `import` wizard.

### Generate the report

Open RStudio and open the project for the report.
