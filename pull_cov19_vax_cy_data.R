library(purrr)
library(dkanr)
library(tidyverse)
library(shiny)
library(ggplot2)
library(lubridate)
library(plotly)
library(shinyWidgets)
library(zoo)
library(ISOweek)

dkanr_setup(url = 'https://www.data.gov.cy')

metadata_rs <- retrieve_node(nid ='4985', as = 'list')

dataset_covid <- ds_search_all(resource_id = metadata_rs$uuid, as = 'df', offset = 1)

glimpse(dataset_covid)
 
data_covid <- dataset_covid %>%
  dplyr::select(yearweekiso, firstdose, seconddose, doseadditional1, 
                targetgroup, denominator, vaccine) %>%
  dplyr::filter(!(targetgroup %in% c("HCW", "LTCF"))) %>%
  mutate(
    yearweekiso =  ISOweek2date(paste(yearweekiso, '-4', sep='')), 
    firstdose = as.numeric(firstdose), 
    seconddose = as.numeric(seconddose),
    doseadditional1 = as.numeric(doseadditional1), 
    targetgroup = as.factor(targetgroup), 
    denominator = as.numeric(denominator), 
    vaccine = as.factor(vaccine)
  )

glimpse(data_covid)

data_covid2 <- data_covid %>% 
  pivot_wider(names_from = vaccine, 
              values_from = c(firstdose, seconddose, doseadditional1), 
              values_fill = 0)

data_covid3 <- data_covid2 %>%
  dplyr::filter(targetgroup == 'ALL') %>%
  dplyr::mutate(
    fully_vaccinated = cumsum(seconddose_COM + seconddose_MOD + firstdose_AZ + firstdose_JANSS), 
    fully_vaccinated_COM = cumsum(seconddose_COM)
  )

data_covid3 %>% head(3) %>%
  complete(yearweekiso = seq.Date(min(yearweekiso), max(yearweekiso), by="day")) %>%
  fill(everything())

ggplot(data = data_covid3, aes(x=yearweekiso)) + 
  geom_line(aes(y = fully_vaccinated)) + 
  geom_line(aes(y = fully_vaccinated_COM))
  scale_y_continuous(labels = scales::comma)
