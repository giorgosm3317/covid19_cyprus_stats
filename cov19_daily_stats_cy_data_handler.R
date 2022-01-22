library(rdrop2)
library(dkanr)
library(dplyr)
library(lubridate)
# library(rstudioapi)

# setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# "C:\Program Files\R\R-4.1.1\bin\Rscript" -e "source('C:/Users/Giorgos/Desktop/Shiny_R/cyprus_antigen_test_tracker/rapid_test_cy_data_handler.R')"

dkanr_setup(url = 'https://www.data.gov.cy')
metadata_rs <- retrieve_node(nid ='4844', as = 'list')

if (!(rdrop2::drop_exists(path = 'cy_daily_cov19_stats'))){
  rdrop2::drop_create(
    path = 'cy_daily_cov19_stats'
  )
}

if (rdrop2::drop_exists(path = 'cy_daily_cov19_stats/cy_daily_cov19_stats_master.rds')){
  
  path_old <- paste0("cy_daily_cov19_stats/cy_daily_cov19_stats_master_", 
                     format(as.Date(Sys.Date()-1), "%d_%m_%Y"), ".rds", sep="")
  
  rdrop2::drop_copy(
    from_path = 'cy_daily_cov19_stats/cy_daily_cov19_stats_master.rds', 
    to_path = path_old
  )
}  

new_data_covid <- ds_search_all(
  resource_id = metadata_rs$uuid, as = 'df', offset = 1
)
  
new_data_covid <- new_data_covid %>%
  dplyr::mutate(
    across(c(2:15), ~ case_when(. == "" ~ "0", . == ":" ~ NA_character_ , TRUE ~ as.character(.)))
    ) %>% 
  dplyr::mutate(across(c(2:15), as.numeric)) %>%
  dplyr::mutate(date = lubridate::dmy(date))

saveRDS(new_data_covid, 'cy_daily_cov19_stats_master.rds')

drop_upload('cy_daily_cov19_stats_master.rds', 
            path = '/cy_daily_cov19_stats', 
            mode = "overwrite")

