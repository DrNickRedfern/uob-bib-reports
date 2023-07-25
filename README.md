# RaIS Bibliometric reports

This README file describes the workflow for generating bibliometric reports using data from Dimensions and Altmetrics.

---

## Folder structure

The folder structure for the project is displayed below.

If any files are missing or in the wrong location the workflow described below will **not** function correctly.

./
│   .gitignore
│   .Rhistory
│   apa_mod_nr.csl
│   dimensions_get_data.py
│   eisvogel.latex
│   LICENSE
│   move_files.ps1
│   rais_bibliometric_report.Rmd
│   rais_bibliometric_report.Rproj
│   README.md
│   test.ipynb
│   tree.log
│   university_of_bradford_white_logo.png
│
├───.Rproj.user
│   ├───91F64AE8
│   │   │   rmd-outputs
│   │   │   saved_source_markers
│   │   │
│   │   ├───bibliography-index
│   │   ├───ctx
│   │   ├───explorer-cache
│   │   ├───pcs
│   │   │       files-pane.pper
│   │   │       source-pane.pper
│   │   │       windowlayoutstate.pper
│   │   │       workbench-pane.pper
│   │   │
│   │   ├───presentation
│   │   ├───profiles-cache
│   │   ├───sources
│   │   │   ├───per
│   │   │   │   ├───t
│   │   │   │   │       42B743EE
│   │   │   │   │       42B743EE-contents
│   │   │   │   │
│   │   │   │   └───u
│   │   │   └───prop
│   │   │           E5DA42A4
│   │   │           INDEX
│   │   │
│   │   ├───tutorial
│   │   └───viewer-cache
│   └───shared
│       └───notebooks
│               patch-chunk-names
│               paths
│
├───altmetrics
│       altmetric_data.R
│       altmetric_html_top.html
│       altmetric_scrape_data.py
│       compile_altmetric_html_file.py
│
├───bibliography
│       generate_bibliography.R
│
└───source_files
        my_theme.R
        numbers2words.R

---

## Workflow

**Please note that all paths are relative to the project folder. The project folder is the folder containing the `.rproj` file.**

### Data preparation

Dimensions can be searched based on researcher ids (e.g., Dimensions id, ORCiD) or output ids (Dimensions id, DOI, grant number, etc.). It is assumed that most reports will be produced for a defined set of researchers. If the result set is to be generated for a defined set of publications then the workflow will need to be adapted to take this into account, though the analysis will only be minimally affected.

**1. Format a list of researcher ids with the research unit and name of each researcher and their disambiguated Dimensions researcher id and save as a `csv` file. This file must be located in the `data` folder.** There must  be only one researcher_id per row; researchers with mutliple ids must have one row per id.

> Researchers without a disambiguated id may also be included in a report by re-searching across a set of outputs produced by the initial search for outputs using the University of Bradford grid number; however, there will not be any individual data for researchers identified in this way and so it will not be possible to calculate relevant statistics for them (e.g., academic age).

### Create the project folder and files

### Set parameters

**2. The config file - `bibliometric_report_params.toml` - is automatically generated when creating the project. The file should be checked at this stage to ensure that the correct parameters have been set for searching the Dimensions database and producing the reports.**

Parameters

- `project`
  - `name`: `string` used to format file names associated with the project
  - `date`: `int` the date on which the data was downloaded from Dimensions
- `unit`
  - `research_unit`: `string` used to display the name of the research unit under analysis in the final report
- `years`
  - `minimum`: `int` the lower bound for the Dimensions query - also used in the final report
  - `maximum`: `int` the upper bound for the Dimensions query - also used in the final report
  - `reference`: `int` the reference year for calculating academic age - almost always the current year
- `look`
  - `palette`: `string` set the palette to be used in the report - will be some value from Rcolorbrewer's list of colourblind safe palettes: YlOrRd, YlGnBu, PuBu, PuBuGn
- `search`
  - `limit`: `int` parameter for Dimensions queries
  - `skip`: `int` parameter for Dimensions queries

### Get data from Dimensions

**3. Run the `dimensions_get_data.py` file to download data from the Dimensions database.**

### Get Altmetric data

Navigate to the `altmetric` folder in the project folder.

**4. Run the `compile_altmetric_html_file.py` file.**

**5. Open the `altmetric_badges.html` file and save as `generated_altmetric_badges.html`.**

**6. Run the `altmetric_scrape_data.py` file.**

**7. Run the `altmetric_data.R` file**.

### Generate the bibliography

Open RStudio and load the project. Navigate to the `bibliography` folder in the project folder in the `files` tab.

**8. Run the `generate_bibliography.R` file to download bibliography information from Crossref.

> Not all items may be discoverable this way and so it may be necessary to manually add bibtex entries to the `bibliography.bib` file. If there is a large number of files that need to be added, this can be done more efficiently by loading the `bibliography.bib` file into **Zotero** and adding new entries using the `import` wizard.

### Generate the report

Open RStudio and open the project for the report. Open the file `*_bibliometric_report.Rmd`.

**9. Click `knit` to generate the report**

> Tidy up the report by adjusting how plots and tables fit onto pages, either by adjusting the scale of a plot on a page of by limiting the data shown.

### Tidy up the project folder

Generating the report will produce a pdf file (the report), the image files for the report, a LaTeX log file.

**10. Open Windows Terminal from the project folder and run `move_files.ps1` to create the `report` folder and move the generated report files to this folder.**

## Archiving

Once a report is completed and no longer required to be accessed, the project folder can be zipped and archived.
