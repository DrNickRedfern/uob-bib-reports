pacman::p_load(glue, here, rcrossref, tidyverse)

project_name <- blogdown::read_toml('bibliometric_report_params.toml')$project$name

publications <- read_csv(here("data", glue("{project_name}_publications_details.csv")))

df_dois <- publications %>%
  select(doi, type) %>%
  filter(type != "preprint") %>%
  filter(!is.na(doi)) %>%
  distinct(doi, type)

df_dois_x <- df_dois %>% 
  group_by((row_number()-1) %/% (n()/4)) %>%
  group_split()

refs <- list()

for (i in seq_along(df_dois_x)){
  
  Sys.sleep(10)
  
  refs_temp <- cr_cn(
    df_dois_x[[i]]$doi,
    format = "bibtex",
    style = "apa",
    locale = "en-GB",
    raw = FALSE,
    .progress = "text",
    url = NULL,
    cache = TRUE
  )
  
  refs <- c(refs, refs_temp)
  
}

myfile <- file("bibliography.bib")

writeLines(unlist(lapply(refs, paste, collapse=" ")), con = myfile)
