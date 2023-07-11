pacman::p_load(here, rcrossref, tidyverse)

publications <- read_csv(here("data", "faculty_of_health_sciences_publications_details.csv"))

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