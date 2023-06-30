pacman::p_load(dimensionsR, here, tidyverse)

# Load file with list of dois
dat <- read_csv(here("data", "faculty_of_health_sciences_publications_details.csv"),
                show_col_types = FALSE)
head(dat)

dois <- dat %>%
  filter(type != "preprint") %>%
  select(doi) %>%
  distinct(doi) %>%
  unlist

# Get the altmetric data
res <- altmetric(doi = dois)
write_csv(res, here("data", "fohs_altmetric_results.csv"))

res <- read_csv(here("data", "fohs_altmetric_results.csv"), show_col_types = FALSE)
head(res)

# tidy up the data to keep only what is needed
final_res <- res %>%
  filter(!is.na(altmetric_id)) %>%
  select(doi, altmetric_id, details_url, last_updated, 
         published_on, type, score, contains("context.")) %>%
  mutate(last_updated = as_date(as_datetime(last_updated)),
         published_on = as_date(as_datetime(published_on))) %>%
  rename(raw_score = score) 

names(final_res) <- gsub("\\.", "_", names(final_res))
  
head(final_res)

altmetric_details <- read_csv(here("data", "fohs_altmetric_data.csv"), show_col_types = FALSE)
head(altmetric_details)
  
altmetric_details <- inner_join(altmetric_details, final_res, by = "doi")
head(altmetric_details)

write_csv(altmetric_details, here("data", "fohs_altmetric_details.csv"))



