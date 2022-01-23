library(rdrop2)
library(dkanr)
library(dplyr)
library(lubridate)

download_cy_daily_stats <- function(){
  
  dkanr_setup(url = 'https://www.data.gov.cy')
  metadata_rs <- retrieve_node(nid ='4844', as = 'list')
  
  covid_daily_stats_df <- ds_search_all(
    resource_id = metadata_rs$uuid, as = 'df', offset = 1
  )
  
  covid_daily_stats_df <- covid_daily_stats_df %>%
    dplyr::mutate(
      across(c(2:15), ~ case_when(. == "" ~ "0", . == ":" ~ NA_character_ , TRUE ~ as.character(.)))
    ) %>% 
    dplyr::mutate(across(c(2:15), as.numeric)) %>%
    dplyr::mutate(date = lubridate::dmy(date))
}

