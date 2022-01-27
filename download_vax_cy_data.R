library(dkanr)
library(tidyverse)
library(lubridate)
library(zoo)
library(ISOweek)

download_cy_vax_data <- function(){
  
  dkanr_setup(url = 'https://www.data.gov.cy')
  
  metadata_rs <- retrieve_node(nid ='4985', as = 'list')
  
  dataset_covid <- ds_search_all(resource_id = metadata_rs$uuid, as = 'df', offset = 1)
  
  data_covid <- dataset_covid %>%
    dplyr::select(yearweekiso, firstdose, seconddose, doseadditional1, 
                  age_group = targetgroup, denominator, vaccine) %>%
    dplyr::filter(!(age_group %in% c("HCW", "LTCF", "AgeUNK"))) %>%
    dplyr::mutate(age_group = str_replace(age_group, "Age", "")) %>%
    dplyr::mutate(age_group = str_replace(age_group, "_", "-")) %>%
    dplyr::mutate(
      yearweekiso =  ISOweek2date(paste(yearweekiso, '-4', sep='')), 
      firstdose = as.numeric(firstdose), 
      seconddose = as.numeric(seconddose),
      doseadditional1 = as.numeric(doseadditional1), 
      age_group = as.factor(age_group), 
      denominator = as.numeric(denominator), 
      vaccine = as.factor(vaccine)
    ) %>%
    dplyr::mutate(age_group = fct_relevel(age_group, "5-9")) %>%
    dplyr::mutate(age_group = fct_rev(age_group))
  
  data_covid <- data_covid %>% 
    pivot_wider(names_from = vaccine, 
                values_from = c(firstdose, seconddose, doseadditional1), 
                values_fill = 0)
}

get_cy_vax_data_brief<- function(){
  
  cy_vax_data_brief <- download_cy_vax_data()
  
  cy_vax_data_brief <- cy_vax_data_brief %>%
    mutate(age_group = case_when(
      age_group == "ALL" ~ "ALL", 
      age_group %in% c("15-17", "10-14", "5-9") ~ "5-17", 
      age_group %in% c("50-59", "25-49", "18-24") ~ "18-60", 
      age_group %in% c("80+", "70-79", "60-69") ~ "60+", 
      TRUE ~ NA_character_
    )) %>%
    mutate(
      age_group = as.factor(age_group), 
      age_group = fct_relevel(age_group, "ALL", "60+", "18-60", "5-17")
    ) %>%
    group_by(yearweekiso, age_group) %>%
    relocate(yearweekiso, age_group) %>%
    mutate(
      across(denominator:last_col(), ~ sum(.x))
    ) %>%
    distinct() %>%
    ungroup()
  
  cy_vax_data_brief <- cy_vax_data_brief %>%
    filter(age_group != "ALL") %>%
    group_by(yearweekiso) %>%
    summarise(age_group = "ALL", across(firstdose_COM:last_col(), sum)) %>%
    bind_cols(., cy_vax_data_brief %>% filter(age_group == "ALL") %>% select(denominator)) %>%
    relocate(denominator, .after = age_group) %>% 
    bind_rows(., cy_vax_data_brief %>% filter(age_group != "ALL")) %>%
    arrange(yearweekiso)
  
  cy_vax_data_brief <- cy_vax_data_brief %>%
    dplyr::group_by(age_group) %>%
    dplyr::mutate(
      fully_vacc = 
        cumsum(seconddose_COM + seconddose_MOD + seconddose_AZ + firstdose_JANSS) / denominator, 
      booster_vacc = cumsum(doseadditional1_COM + doseadditional1_MOD + doseadditional1_AZ + 
                              doseadditional1_JANSS) / denominator
    ) %>%
    ungroup()
}

get_cy_vax_data_brief() %>% View()

data1 <- get_cy_vax_data_brief()

glimpse(data1)


data_covid3 <- data_covid2 %>%
  dplyr::group_by(age_group) %>%
  dplyr::mutate(
    fully_vacc = 
      cumsum(seconddose_COM + seconddose_MOD + seconddose_AZ + firstdose_JANSS) / denominator, 
    booster_vacc = cumsum(doseadditional1_COM + doseadditional1_MOD + doseadditional1_AZ + 
                       doseadditional1_JANSS) / denominator
  ) %>%
  ungroup()

 %>% View()

# ggplot(data = data_covid3, aes(x=yearweekiso)) + 
#   geom_line(aes(y = fully_vaccinated, color = age_group), size = 1.25) + 
#   scale_colour_manual(values = rainbow(10)) # nlevels(data_covid3$age_group)
#   scale_color_viridis_d()
# 
# 
#   # scale_color_colorblind()
# 
# scale_y_continuous(labels = scales::comma)
#   
# 
# data_covid3 %>% head(3) %>%
#   complete(yearweekiso = seq.Date(min(yearweekiso), max(yearweekiso), by="day")) %>%
#   fill(everything())
# 
# ggplot(data = data_covid3, aes(x=yearweekiso)) + 
#   geom_line(aes(y = fully_vaccinated)) + 
#   geom_line(aes(y = fully_vaccinated_COM))
#   scale_y_continuous(labels = scales::comma)
