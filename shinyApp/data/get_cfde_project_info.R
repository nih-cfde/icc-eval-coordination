library(tidyverse)
library(magrittr)
library(gh)
## Get Project Info
raw_core_projects <- fromJSON(gh('GET https://raw.githubusercontent.com/nih-cfde/icc-eval-core/refs/heads/main/data/raw/reporter-projects.json', .token = gitcreds::gitcreds_get()$password)$message)$results %>% as_tibble()
## DT Prep
raw_core_projects %>% 
  unnest(organization) %>% 
  mutate(project_detail_url = glue::glue("<a href='{project_detail_url}' target='_blank'>{project_detail_url}</a>")) %>% 
  select(core_project_num, project_num, project_title, project_detail_url, org_name, terms, pref_terms) %>% 
  write_csv(here::here('shinyApp/data/reporter-projects.csv'))

