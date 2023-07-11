pacman::p_load(glue, here, rcrossref, tidyverse)

project_name <- blogdown::read_toml('bibliometric_report_params.toml')$project$name

publications <- read_csv(here("data", glue("{project_name}_publications_details.csv")))

df_dois <- publications %>%
  select(doi, type) %>%
  filter(type != "preprint") %>%
  filter(!is.na(doi)) %>%
  distinct(doi, type)

refs <- cr_cn(
  df_dois$doi,
  format = "bibtex",
  style = "apa",
  locale = "en-GB",
  raw = FALSE,
  .progress = "text",
  url = NULL,
  cache = TRUE
)

myfile = file("bibliography.bib")

writeLines(unlist(lapply(refs, paste, collapse=" ")), con = myfile)
