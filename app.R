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
library(tidyverse)
library(dkanr)
library(ggplot2)
library(lubridate)
library(plotly)
library(shinyWidgets)
library(zoo)
library(tidyselect)
library(here)
library(scales)
library(patchwork)

source(here("download_cy_daily_stats.R"))

covid_daily_stats_df <- download_cy_daily_stats()

source(here("download_vax_cy_data.R"))

vax_cy_df <- get_cy_vax_data_brief()

# rdrop2::drop_auth(rdstoken = "token.rds")

create_roll_avg <- function(dataset_in, roll_avg_d, var_displayed){
    
    var_displayed = as.character(var_displayed)
    col_order <- names(dataset_in)
    new_col = paste(var_displayed, roll_avg_d, 'da', sep='_')
    
    dataset_out <- dataset_in %>%
        mutate(!!(new_col) := zoo::rollmean(!!rlang::sym(var_displayed),
                                        k = as.numeric(roll_avg_d), fill = NA), 
               .after = !!(var_displayed))
}

# if (!exists('covid_daily_stats_df')){
#     rdrop2::drop_download(
#         path = 'cy_daily_cov19_stats/cy_daily_cov19_stats_master.rds', 
#         local_path = 'cy_daily_cov19_stats_master.rds', 
#         overwrite = TRUE
#     )
#     covid_daily_stats_df <- readRDS('cy_daily_cov19_stats_master.rds')
# } 



ui <- fluidPage(
    titlePanel('Cyprus COVID-19 Daily Stats'),
    sidebarLayout(
        sidebarPanel(
          prettyCheckboxGroup(
            inputId = "select_data",
            label = "Display information about:", 
            choices = c("The spread of COVID-19" = "spread_covid",
                        "The COVID-19 vaccinations" = "vaccinations"),
            icon = icon("check-square"), 
            status = "primary",
            outline = TRUE,
            animation = "pulse", 
            selected = "spread_covid"
          ),
          
          dateRangeInput('date_range', 'Date range:',
                         start = max(covid_daily_stats_df$date) -7, end = max(covid_daily_stats_df$date),
                         format = "dd-mm-yyyy", 
                         min = min(covid_daily_stats_df$date), max = max(covid_daily_stats_df$date)
          ),
          
          conditionalPanel(
            
            condition = "input.select_data.includes('spread_covid')",
            
            selectInput(
              inputId = 'variables_inc', 
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
              # choices = names(covid_daily_stats_df %>% dplyr::select(-date, -notes, -entry_id)), 
              multiple = TRUE, 
              selected = "daily_new_cases"
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
            )
          ),
          
          conditionalPanel(
            
            condition = "input.select_data.includes('vaccinations')",
            
            selectInput(
              inputId = 'vax_age_group', 
              label = 'Age group for vaccinations:', 
              choices = c('All' = "ALL",
                          '5-17' = "5-17", 
                          '18-60' = "18-60",
                          '60+' = "60+"),
              multiple = TRUE, 
              selected = "ALL"
            ), 
            
            selectInput(
              inputId = 'vax_status', 
              label = 'Vaccination status:', 
              choices = c('Fully vaccinated' = "fully_vacc",
                          'Booster dose' = "booster_vacc"),
              multiple = TRUE, 
              selected = "fully_vacc"
            )
          )
          
        ), 
        
        mainPanel(
            tabsetPanel(
                tabPanel('Graph', plotly::plotlyOutput('plot')),
                # tabPanel('Plot', plotOutput('plot')), 
                # tabPanel('Plot2', plotly::plotlyOutput('plot2')),
                # tabPanel('Table', dataTableOutput('table_covid')), 
                tabPanel('About', uiOutput('about'))
            )
        )
    )
)    
    
server <- function(input, output){
  
  input_validation <- reactive({
    
    validate(
      need(input$select_data, "Select information to be displayed!"),
      need(input$date_range[1] < input$date_range[2], 'Select a valid date range!')
    )
    
    if ("spread_covid" %in% input$select_data){
      validate(
        need(input$variables_inc, 'Select at least one option to be displayed!') 
      )
    }
    
    if ("vaccinations" %in% input$select_data){
      validate(
        need(input$vax_age_group, 'Select at least 1 option to be displayed!'), 
        need(input$vax_status, 'Select at least 1ne option to be displayed!')  
      )
    }
    
  })
    
  filtered_data <- reactive({
      roll_avg <- c(input$roll_avg_daily_new_cases, input$roll_avg_daily_deaths, 
                         input$roll_avg_hospitalised_cases, input$roll_avg_severe_cases, 
                         input$roll_avg_cases_in_icus, input$roll_avg_incubated_cases)
      
      output_data <- covid_daily_stats_df %>%
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
    
  #-----------------------------------------------------------------------------------------
  
  output$plot <- plotly::renderPlotly({
  # output$plot <- renderPlot({
    
    input_validation()
      
      # validate(
      #     need(input$variables_inc, 'Select at least one variable!'), 
      #     need(input$date_range[1] < input$date_range[2], 'Select a valid date range!')
      # )
      
    if ("spread_covid" %in% input$select_data){
      
      final_daily_stats <- filtered_data_roll()
      
      final_daily_stats <- final_daily_stats %>%
        tidyr::pivot_longer(!date, names_to = 'variable', values_to = 'value')

      for (var in input$variables_inc) {
        
        var <- as.character(var)
        
        if (var %in% c("daily_new_cases", "daily_deaths", "hospitalised_cases", 
                       "severe_cases", "cases_in_icus", "incubated_cases")){
          if (!is.null(input[[paste('roll_avg_', var, sep="")]])){
            if (!('1' %in% input[[paste('roll_avg_', var, sep="")]])){
              final_daily_stats <- final_daily_stats %>%
                dplyr::filter(variable != var)
            }
          }
        }
      }
      
      plot_daily_stats <- ggplot(data = final_daily_stats, 
                                 aes(x=date, y = value, colour = variable)) +
          geom_line() +
          theme(legend.title=element_blank()) +
          labs(
            x = "Date", 
            y = "", 
            colour = "", 
            caption = "Cyprus National Open Data Portal"
          ) +
          scale_y_continuous(limits = c(0, NA))
      
      plot_daily_stats
      
    }
    
    if ("vaccinations" %in% input$select_data){
      
      final_vax_data <- vax_cy_df %>%
        dplyr::filter(yearweekiso >= input$date_range[1], yearweekiso <= input$date_range[2]) %>%
        pivot_longer(
          cols = -c(yearweekiso, age_group), 
          names_to = "vax_status", 
          values_to = "vax_percentage"
        ) %>%
        dplyr::mutate(vax_percentage = round(vax_percentage*100, 2))
      
      final_vax_data <- final_vax_data %>%
        dplyr::filter(age_group %in% input$vax_age_group) %>%
        dplyr::filter(vax_status %in% input$vax_status)
      
      plot_vax_data <- ggplot(data = final_vax_data, 
             aes(x = yearweekiso, y = vax_percentage, color = age_group, linetype = vax_status)) + 
        geom_line() + 
        labs(
          x = "Date", 
          y = "Percentage", 
          colour = "Age group",
          linetype = "Vaccination status",
          caption = "Cyprus National Open Data Portal"
        ) +
        scale_y_continuous(
          limits = c(0, 100),
          labels = scales::label_percent(scale = 1, accuracy = 1)
        )

    }
    
    if(("spread_covid" %in% input$select_data) & !("vaccinations" %in% input$select_data)){
      final_plot <- plot_daily_stats 
    } else if (!("spread_covid" %in% input$select_data) & ("vaccinations" %in% input$select_data)) {
      final_plot <- plot_vax_data
    } else {
      final_plot <- subplot(plot_vax_data, plot_daily_stats, 
                            nrows = 2, margin = c(0.02,0.02,0.1,0.02), 
                            shareX = TRUE, titleX = TRUE)
    }
    
    final_plot 
  })
    
  #----------------------------------------------------------------------------------------
    output$plot2 <- plotly::renderPlotly({
      
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
        scale_x_date(date_labels =  "%b %Y") + 
        scale_y_continuous(limits = c(0, NA))
    })
    
    #----------------------------------------------------------------------------------------
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
    
}

shinyApp(ui, server)
