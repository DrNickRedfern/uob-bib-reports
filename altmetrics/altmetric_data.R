# This code is designed to retrieve and process altmetric data for a set of publications. 
# Altmetric data provides information about the online attention and engagement that 
# scholarly publications receive, such as mentions on social media, news outlets, and other online sources.
#
# The code takes two inputs:
#
# A list of Digital Object Identifiers (DOIs) for the publications of interest, which is read from a 
# CSV file named "{project_name}_publications_details.csv" located in the "data" directory.
# The name of the project, which is read from a configuration file named "bibliometric_report_params.toml".
#
# The main output of the code is a CSV file named "{project_name}_altmetric_details.csv", 
# which contains the altmetric data for the publications of interest, including their DOIs, altmetric IDs, 
# URLs for more details, publication dates, altmetric scores, and various context metrics 
# (e.g., mentions on Twitter, Facebook, news outlets, etc.).

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dimensionsR, glue, here, tidyverse)

project_name <- blogdown::read_toml("bibliometric_report_params.toml")$project$name

# Load file with list of dois
dat <- read_csv(here("data", glue("{project_name}_publications_details.csv")),
                show_col_types = FALSE)
head(dat)

dois <- dat %>%
  filter(type != "preprint") %>%
  select(doi) %>%
  distinct(doi) %>%
  unlist

# Get the altmetric data
res <- altmetric(doi = dois)
write_csv(res, here("data", glue("{project_name}_altmetric_results.csv")))

res <- read_csv(here("data", glue("{project_name}_altmetric_results.csv")), show_col_types = FALSE)
# head(res)

# tidy up the data to keep only what is needed
final_res <- res %>%
  filter(!is.na(altmetric_id)) %>%
  select(doi, altmetric_id, details_url, last_updated, 
         published_on, type, score, contains("context.")) %>%
  # mutate(last_updated = as_date(as_datetime(last_updated)),
  #        published_on = as_date(as_datetime(published_on))) %>%
  rename(raw_score = score) 

names(final_res) <- gsub("\\.", "_", names(final_res))
# head(final_res)

altmetric_details <- read_csv(here("data", glue("{project_name}_altmetric_data.csv")), 
                                   show_col_types = FALSE)
# head(altmetric_details)
  
altmetric_details <- inner_join(altmetric_details, final_res, by = "doi")
#head(altmetric_details)

write_csv(altmetric_details, here("data", glue("{project_name}_altmetric_details.csv")))