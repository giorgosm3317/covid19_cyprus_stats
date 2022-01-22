#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rdrop2)
library(purrr)
library(dkanr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(shinyWidgets)
library(zoo)
library(tidyselect)
library(tidyr)

rdrop2::drop_auth(rdstoken = "token.rds")

create_roll_avg <- function(dataset_in, roll_avg_d, var_displayed){
    
    var_displayed = as.character(var_displayed)
    col_order <- names(dataset_in)
    new_col = paste(var_displayed, roll_avg_d, 'da', sep='_')
    
    dataset_out <- dataset_in %>%
        mutate(!!(new_col) := zoo::rollmean(!!rlang::sym(var_displayed),
                                        k = as.numeric(roll_avg_d), fill = NA), 
               .after = !!(var_displayed))
}

if (!exists('data_covid_cy')){
    rdrop2::drop_download(
        path = 'cy_daily_cov19_stats/cy_daily_cov19_stats_master.rds', 
        local_path = 'cy_daily_cov19_stats_master.rds', 
        overwrite = TRUE
    )
    data_covid_cy <- readRDS('cy_daily_cov19_stats_master.rds')
} 

ui <- fluidPage(
    titlePanel('Cyprus COVID-19 Daily Stats'),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId =  'variables_inc', 
                label = 'Show:', 
                choices = c('Daily new cases' = "daily_new_cases", 'Daily deaths' = "daily_deaths", 
                            'Hospitalised cases' = "hospitalised_cases", 'Severe cases' = "severe_cases", 
                            'Cases in ICUs' = "cases_in_icus", 'Incubated cases' = "incubated_cases", 
                            'PCR daily tests' = "pcr_daily_tests_performed", 
                            'Rapid Antigen daily tests' = 'ra_daily_tests_performed', 
                            'Total Daily Tests' = "total_daily_tests_performed", 
                            'Total cases' = "total_cases", 'Total Deaths' = "total_deaths", 
                            'Total PCR tests' = "total_pcr_tests", 
                            'Total Rapid Antigen Tests' = "total_ra_tests", 'Total Tests' = "total_tests"),
                # choices = names(data_covid_cy %>% dplyr::select(-date, -notes, -entry_id)), 
                multiple = TRUE, 
                selected = "daily_new_cases"
            ), 
            
            dateRangeInput('date_range', 'Date range:',
                           start = max(data_covid_cy$date) -7, end = max(data_covid_cy$date),
                           format = "dd-mm-yyyy", 
                           min = min(data_covid_cy$date), max = max(data_covid_cy$date)
            ), 
            
            conditionalPanel(
                condition = "input.variables_inc.includes('daily_new_cases')",
                checkboxGroupButtons("roll_avg_daily_new_cases", 'Rolling average - Daily new cases:',
                                         choices = c("1", "3", "5", "7", "15", "21"), justified = TRUE)
            ),
            conditionalPanel(
                condition = "input.variables_inc.includes('daily_deaths')",
                checkboxGroupButtons("roll_avg_daily_deaths", 'Rolling average - Daily deaths:',
                                     choices = c("1", "3", "5", "7", "15", "21"), justified = TRUE)
            ), 
            conditionalPanel(
                condition = "input.variables_inc.includes('hospitalised_cases')",
                checkboxGroupButtons("roll_avg_hospitalised_cases", 'Rolling average - Hospitalised cases:',
                                     choices = c("1", "3", "5", "7", "15", "21"), justified = TRUE)
            ),
            conditionalPanel(
                condition = "input.variables_inc.includes('severe_cases')",
                checkboxGroupButtons("roll_avg_severe_cases", 'Rolling average - Severe cases:',
                                     choices = c("1", "3", "5", "7", "15", "21"), justified = TRUE)
            ),
            conditionalPanel(
                condition = "input.variables_inc.includes('cases_in_icus')",
                checkboxGroupButtons("roll_avg_cases_in_icus", 'Rolling average - Cases in ICUs:',
                                     choices = c("1", "3", "5", "7", "15", "21"), justified = TRUE)
            ),
            conditionalPanel(
                condition = "input.variables_inc.includes('incubated_cases')",
                checkboxGroupButtons("roll_avg_incubated_cases", 'Rolling average - Incubated cases:',
                                     choices = c("1", "3", "5", "7", "15", "21"), justified = TRUE)
            ), 
        ), 
        
        mainPanel(
            tabsetPanel(
                tabPanel('Plot', plotly::plotlyOutput('plot')), 
                tabPanel('Table', dataTableOutput('table_covid')), 
                tabPanel('About', uiOutput('about'))
            )
        )
    )
)    
    
server <- function(input, output){
    
    filtered_data <- reactive({
        roll_avg <- c(input$roll_avg_daily_new_cases, input$roll_avg_daily_deaths, 
                           input$roll_avg_hospitalised_cases, input$roll_avg_severe_cases, 
                           input$roll_avg_cases_in_icus, input$roll_avg_incubated_cases)
        
        output_data <- data_covid_cy %>%
            dplyr::select(date, !!!rlang::syms(input$variables_inc))
        
        if(is.null(roll_avg)){
            output_data <- output_data %>%
                filter(date >= input$date_range[1], date <= input$date_range[2])
        }
        else {
            max_roll_avg <- max(as.numeric(roll_avg))
            output_data <- output_data %>%
                filter(date >= input$date_range[1] - ((max_roll_avg)/2-0.5),
                       date <= input$date_range[2] + ((max_roll_avg)/2-0.5))
        }
        output_data
    })
    
    filtered_data_roll <- reactive({
        final_data <- filtered_data()
        
        for (variable in input$variables_inc){
            if (variable %in% c("daily_new_cases", "daily_deaths", "hospitalised_cases", 
                                "severe_cases", "cases_in_icus", "incubated_cases")){
                if (!is.null(input[[paste('roll_avg_', variable, sep="")]])){
                    for (num_days in input[[paste('roll_avg_', variable, sep="")]]){
                        if (num_days != 1) {
                            final_data <- create_roll_avg(final_data, num_days, variable)
                        }
                    }
                }
            }
        }
        final_data <- final_data  %>%
            dplyr::filter(date >= input$date_range[1], date <= input$date_range[2])
    })
    

    output$about <- renderUI({
        tagList(
            fluidRow(
                column(6, 'Authored by ', tags$a(href="mailto:giorgosm33@hotmail.com", "Georgios M"))
            ), 
            fluidRow(
                column(6, 'Source: ',
                       (tags$a(href="https://www.data.gov.cy/?language=en", 
                               "Cyprus National Open Data Portal")))
            )
        ) 
    })
    
    output$table_covid <- renderDataTable({
        
        validate(
            need(input$variables_inc, 'Select at least one variable!'), 
            need(input$date_range[1] < input$date_range[2], 'Select a valid date range!')
        )
        
        final_data <- filtered_data_roll()
        
        final_data <- final_data %>%
            tidyr::pivot_longer(!date, names_to = 'variable', values_to = 'value')
        
        for (var in input$variables_inc) {
            
            var <- as.character(var)
            
            if (var %in% c("daily_new_cases", "daily_deaths", "hospitalised_cases", 
                                "severe_cases", "cases_in_icus", "incubated_cases")){
                if (!is.null(input[[paste('roll_avg_', var, sep="")]])){
                    if (!('1' %in% input[[paste('roll_avg_', var, sep="")]])){
                        final_data <- final_data %>%
                            dplyr::filter(variable != var)
                    }
                }
            }
        }
        
        final_data
    })
    
    output$plot <- plotly::renderPlotly({
        
        validate(
            need(input$variables_inc, 'Select at least one variable!'), 
            need(input$date_range[1] < input$date_range[2], 'Select a valid date range!')
        )
        
        final_data <- filtered_data_roll()
        
        final_data <- final_data %>%
            tidyr::pivot_longer(!date, names_to = 'variable', values_to = 'value')
        
        for (var in input$variables_inc) {
            
            var <- as.character(var)
            
            if (var %in% c("daily_new_cases", "daily_deaths", "hospitalised_cases", 
                           "severe_cases", "cases_in_icus", "incubated_cases")){
                if (!is.null(input[[paste('roll_avg_', var, sep="")]])){
                    if (!('1' %in% input[[paste('roll_avg_', var, sep="")]])){
                        final_data <- final_data %>%
                            dplyr::filter(variable != var)
                    }
                }
            }
        }
        
        ggplot(data = final_data, aes(x=date, y = value, color = variable)) + 
            geom_line() + 
            theme(legend.title=element_blank()) + 
            labs(x = "Date", y = "Value") + 
            scale_x_date(date_labels =  "%b %Y") 
    })
}

shinyApp(ui, server)
