library(dkanr)
library(tidyverse)
library(lubridate)
library(zoo)
library(ISOweek)

dkanr_setup(url = 'https://www.data.gov.cy')

metadata_rs <- retrieve_node(nid ='4985', as = 'list')

dataset_covid <- ds_search_all(resource_id = metadata_rs$uuid, as = 'df', offset = 1)

glimpse(dataset_covid)
 
data_covid <- dataset_covid %>%
  dplyr::select(yearweekiso, firstdose, seconddose, doseadditional1, 
                age_group = targetgroup, denominator, vaccine) %>%
  dplyr::filter(!(age_group %in% c("HCW", "LTCF", "AgeUNK"))) %>%
  dplyr::mutate(age_group = str_replace(age_group, "Age", "")) %>%
  dplyr::mutate(age_group = str_replace(age_group, "_", "-")) %>%
  # dplyr::mutate(
  #   vaccine = case_when(
  #     vaccine == "COM" ~ "Pfizer-BioNTech",
  #     vaccine == "MOD" ~ "Moderna", 
  #     vaccine == "AZ" ~ "AstraZeneca", 
  #     vaccine == "JANSS" ~ "Janssen", 
  #     TRUE ~ "Other"
  #   )
  # ) %>%
  mutate(
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

glimpse(data_covid)

data_covid2 <- data_covid %>% 
  pivot_wider(names_from = vaccine, 
              values_from = c(firstdose, seconddose, doseadditional1), 
              values_fill = 0)

data_covid3 <- data_covid2 %>%
  dplyr::group_by(age_group) %>%
  dplyr::mutate(
    fully_vaccinated = 
      cumsum(seconddose_COM + seconddose_MOD + firstdose_AZ + firstdose_JANSS) / denominator, 
    booster = cumsum(doseadditional1_COM + doseadditional1_MOD + doseadditional1_AZ + 
                       doseadditional1_JANSS) / denominator
  ) %>%
  ungroup()

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
