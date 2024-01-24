# RaIS Bibliometric reports

This README file describes the workflow for generating standard bibliometric reports using data from Dimensions and Altmetrics.

> [!NOTE]  
> This README file describes the process for generating version 1.0.x of the standard bibliometric reports. Please check the version number and ensure you are reading the correct README file before proceeding.

---

## Folder structure

The folder structure for the project is displayed in the `tree.txt` file.

> [!WARNING]  
> If any files are missing or in the wrong location the workflow described below will not function correctly.

---

## Workflow

**Please note that all paths are relative to the project folder. The project folder is the folder containing the `rproj` file.**

- Relative paths in R scripts are implemented using the [here](https://here.r-lib.org) package.

### Data preparation

Dimensions can be searched based on researcher ids (e.g., Dimensions id, ORCiD) or output ids (Dimensions id, DOI, grant number, etc.). It is assumed that most reports will be produced for a defined set of researchers. If the result set is to be generated for a defined set of publications then the workflow will need to be adapted to take this into account, though the analysis will only be minimally affected.

**1. A formatted list of researcher ids with the research unit and name of each researcher and their disambiguated Dimensions researcher id is saved as `faculty.csv` file in the `data` folder of the template project. This file will be automatically copied to the project's `data` folder when creating the project.** This file must be updated prior to creating a project. There must  be only one researcher_id per row; researchers with mutliple ids must have one row per id. When generating the report, results for researchers with mutliple ids will be collated under the id with the greatest academic age.

> [!NOTE]  
> Researchers without a disambiguated id may also be included in a report by re-searching across a set of outputs produced by the initial search for outputs using the University of Bradford grid number; however, there will not be any individual data for researchers identified in this way and so it will not be possible to calculate relevant statistics for them (e.g., academic age). This is not implemented at the moment because it creates more problems than it solves.

### Create the project folder and files

To create the project open the folder bibliometric_reports and execute the `bibliometric_report.ps1` file. This will create the project folder, copy all the required files to the project folder and generate the config file containing the paramters for the report.

#### Set parameters

**2. The config file - `bibliometric_report_params.toml` - is automatically generated when creating the project.** The file should be checked to ensure that the correct parameters have been set for searching the Dimensions database and producing the reports.

> [!IMPORTANT]
> To create the `bibliometric_report_params.toml` file you will need to have PSToml installed. See the GitHub repo for [PSToml](https://github.com/jborean93/PSToml) for full installation instructions.

##### Parameters

- `project`
  - `name`: `string` used to format file names associated with the project.
  - `date`: `int` the date on which the data was downloaded from Dimensions.
- `unit`
  - `research_unit`: `string` used to display the name of the research unit under analysis in the final report - this must match one of the names in [Table 1](#table-1-values-for-creating-projects-for-standard-bibliometric-reports) below. NB: this inlcudes matching the case of the name of the research unit.
- `years`
  - `minimum`: `int` the lower bound for the Dimensions query; also used in the final report.
  - `maximum`: `int` the upper bound for the Dimensions query; also used in the final report.
  - `reference`: `int` the reference year for calculating academic age - almost always the current year.
- `look`
  - `palette`: `string` set the palette to be used in the report. This will be some value from [Rcolorbrewer's](https://earlglynn.github.io/RNotes/package/RColorBrewer/index.html) list of colourblind safe palettes - see [Table 1](#table-1-values-for-creating-projects-for-standard-bibliometric-reports) for which palette to use for which faculty.
- `search`
  - `limit`: `int` parameter for Dimensions queries.
  - `skip`: `int` parameter for Dimensions queries.

#### Table 1: values for creating projects for standard bibliometric reports

| Research unit                                              | Palette |
|:-----------------------------------------------------------|:--------|
| Faculty of Engineering and Digital Technologies            | PuBuGn  |
| Faculty of Health Studies                                  | YlGnBu  |
| Faculty of Life Sciences                                   | PuBu    |
| Faculty of Management, Law and Social Sciences             | YlOrRd  |

> [!NOTE]
> User selection of a palette will be replaced in the next version of the report with automatic selection on rendering.

### Get data from Dimensions

> [!IMPORTANT]
> This workflow assumes that users will log into Dimensions via a `dsl.ini` file that is stored in the project folder. See the Dimensions guide to using dimcli on [how to create a configuration file](https://digital-science.github.io/dimcli/getting-started.html#creating-a-configuration-file-manually) for easy API access.

**3. Run the `dimensions_get_data.py` file to download data from the Dimensions database.** This file will collect data from Dimensions and save it to the required `csv` files.

### Get data for creating internal collaboration networks

**4. Run the `internal_collaboration.py` file**. This will generate a `csv` file that contains data for visualising internal networks among researchers.

---

### Get Altmetric data

Navigate to the `altmetric` folder in the project folder.

**5. Run the `compile_altmetric_html_file.py` file.** This will generate an `html` file containing altmetric badges for any outputs with dois for which altmetric data is available.

**6. Open the `altmetric_badges.html` file and save as `altmetric_badges_generated.html`.** This step is required to produce an `html` file containing the altmetric data suitable for scraping.

**7. Run the `altmetric_scrape_data.py` file.** This script will scrape altmetric data from the `altmetric_badges_generated.html` file and store the results as a `csv` file.

**8. Open RStudio and load the project: `File > Open Project > [navigate to the project folder and select the rproj file] > Open`. In RStudio, navigate to the `altmetric` folder in the project folder in the `files` tab. Run the `altmetric_data.R` file.** This will add additional altmetric data to the result set.

> [!IMPORTANT]
> Changes to the altmetric API access via the dimensionsR package no longer seems to produce useful data for most outputs. Some of the report functionality in the Altmetrics section has been disabled until an alternative solution is provided.

### Generate the report

**9. In RStudio, open the project for the report (if not already open from Step 8). Open the file `{faculty}_bibliometric_report_{date}.qmd`. Click `Render` to generate the report**. `embed-resources` is set to `true` and so it will not be necessary to share `html` and `css` files when sharing the report. However, if filesizes become too large this will be chnaged and the workflow updated.

### Tidy up the project folder

**11. Open Windows Terminal from the project folder and run `move_files.ps1` to create the `report` folder and move the generated report files to this folder.** This report folder can then be shared with anyone who wants to access the report. To access the report, users will need to download the `{faculty}_bibliometric_report_{date}.html` file and open it in their preferred browser.

## Archiving

Once a report is completed and no longer required to be accessed, the project folder can be zipped and archived in the biblioemtric reports archive folder on SharePoint.
