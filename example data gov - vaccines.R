library(purrr)
library(dkanr)
library(dplyr)
library(shiny)
library(ggplot2)
library(lubridate)
library(plotly)
library(shinyWidgets)
library(zoo)
library(ISOweek)

dkanr_setup(url = 'https://www.data.gov.cy')

dkanr_settings()

# Get a list of all datasets
resp <- list_nodes_all(filters = c(type = 'dataset'), as = 'df')
# Print the first 10 datasets
resp %>%
  select(nid, title, uri) %>%
  arrange(title) %>%
  head(n = 10)

metadata <- retrieve_node(nid ='4984', as = 'list')
metadata

names(metadata)[1:30]

get_resource_nids(metadata)

metadata_rs <- retrieve_node(nid ='4985', as = 'list')
metadata_rs
ds_is_available(metadata_rs)

dataset_covid <- ds_search_all(resource_id = metadata_rs$uuid, as = 'df', offset = 1)

glimpse(dataset_covid)
 
data_covid <- dataset_covid %>%
  dplyr::select(yearweekiso, firstdose, seconddose, doseadditional1, targetgroup, denominator, vaccine) %>%
  mutate(
    yearweekiso =  ISOweek2date(paste(yearweekiso, '-4', sep='')), 
    firstdose = as.numeric(firstdose), 
    seconddose = as.numeric(seconddose),
    doseadditional1 = as.numeric(doseadditional1), 
    targetgroup = as.factor(targetgroup), 
    denominator = as.numeric(denominator), 
    vaccine = as.factor(vaccine)
    # yearweekiso = ISOweek2date(paste(!!rlang::sym(yearweekiso), '-4'))
    # area = as.factor(area),
    # people_tested = as.numeric(people_tested), 
    # positive_cases = as.numeric(positive_cases),
    # positive = as.numeric(sub("%", "", positive))/100
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

# -------------------------------------------------------------------------------------------------


ui <- fluidPage(
  sidebarPanel(
    selectInput('area', 'Select Area', choices = levels(data_covid$area), 
                multiple = TRUE, selected = 'Total'), 
    dateRangeInput('date_range', 'Date range', 
                   start = max(data_covid$date) -7, end = max(data_covid$date), 
                   format = "dd-mm-yyyy", min = min(data_covid$date), max = max(data_covid$date), 
                   ), 
    checkboxGroupButtons("roll_avg", 'Rolling average:', choices = c("3", "5", "7", "15", "21"), 
    justified = TRUE)
    
  ), 
  mainPanel(
    # tabsetPanel(
    #   tabPanel('Plot', tableOutput('table_covid')), 
    #   tabPanel('Table', DataTableOutput('table_covid'))
    # )
    plotly::plotlyOutput('plot'),
    # plotOutput('plot')
    # textOutput('textroll')
    dataTableOutput('table_covid')
    
  )
)

server <- function(input, output){
  
  filtered_data <- reactive({
    if(is.null(input$roll_avg)){
      data_covid %>%
        filter(area %in% input$area, date >= input$date_range[1], date <= input$date_range[2]) %>%
        select(date, area, positive_cases)
    }
    else {
      data_covid %>%
        filter(area %in% input$area, date >= input$date_range[1] - (max(as.numeric(input$roll_avg))/2-0.5),
               date <= input$date_range[2] + (max(as.numeric(input$roll_avg))/2-0.5)) %>%
        select(date, area, positive_cases)
    }
    
  })
  
  filtered_data_roll <- reactive({
    final_data <- filtered_data()
    if(!is.null(input$roll_avg)){
      # final_data <- final_data%>%
      #   dplyr::mutate(positive_cases = positive_cases + as.numeric(input$roll_avg))
      
      for (val in input$roll_avg) {
        final_data_temp <- filtered_data() %>%
          mutate(area = paste(area, "_", val, "da", sep = "")) %>%
          dplyr::group_by(area) %>%
          mutate(positive_cases = zoo::rollmean(positive_cases,
                                                  k = as.numeric(val), fill = NA)) %>%
          dplyr::ungroup() %>%
          select(date, area, positive_cases)

        final_data <- rbind(final_data, final_data_temp)
      }
    }
    final_data <- final_data  %>%
      dplyr::filter(date >= input$date_range[1], date <= input$date_range[2])
  })
  
  output$table_covid <- renderDataTable({
    filtered_data_roll()
    # filtered_data()
  })
  
  output$plot <- plotly::renderPlotly(
    # output$plot <- renderPlot(
    filtered_data_roll() %>%
      ggplot(aes(x=date, y = positive_cases)) + geom_line(aes(colour = area))
  )

  
}

shinyApp(ui, server)


table5 <- data_covid %>%
  filter(month(date) == 10, year(date) == 2021) %>%
  filter(area %in% c('Lefkosia', 'Lemesos')) %>%
  dplyr::group_by(area) %>%
  dplyr::mutate(three_day =zoo::rollmean(positive_cases, k = 3, fill = NA)) %>%
  dplyr::ungroup()

table6 <- data_covid %>%
  filter(month(date) == 10, year(date) == 2021) %>%
  filter(area == 'Lefkosia') %>%
  dplyr::mutate(three_day =zoo::rollmean(positive_cases, k = 3, fill = NA))

         