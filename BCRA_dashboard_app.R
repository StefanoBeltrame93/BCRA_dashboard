#BCRA Dashboard for Monetary Policy Control

rm(list = ls())

#API BCRA 
library(tidyverse)
library(httr)
library(jsonlite)
library(shiny)
library(lubridate)
library(plotly)
library(scales)
library(zoo)
library(shinythemes)

#importing functions
source(stringr::str_c(getwd(), "/functions/BCRA_api.R")) #api bcra data download function 


# Define UI for application
ui <- fluidPage(
  theme = shinythemes::shinytheme("cosmo"),
  tabsetPanel(
    type = "tabs",
    tabPanel(title = "Graficos",
             br(),
             titlePanel("BCRA Monetary Policy Performance Dashboard"),
             br(),
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "MPInstrument",
                             label = "Instrumento de Politica Monetaria a Visualizar",
                             choices = c("Base Monetaria" = "base",
                                         "Logaritmo Base Monetaria" = "log_bm",
                                         "Base Monetaria en USD Corrientes" = "bm_usd",
                                         "Reservas Internacionales" = "rrii",
                                         "Variación diaria RRII" = "delta1_rrii",
                                         "Indice MERVAL en USD" = "merval_usd")),
                 dateRangeInput(
                   inputId = "dateRange",
                   start = "1996-01-01",
                   end = as.character(Sys.Date()),
                   label = "Seleccionar el rango de fechas",
                   min = "1996-01-01",
                   weekstart = 1)
               ),
               
               mainPanel(
                 titlePanel(title = "Principales Indicadores de la Politica Monetaria"),
                 plotlyOutput(outputId = "graphics"),
                 br(),
                 titlePanel("Key Monetary Indicators - Últimos Datos"),
                 tableOutput(outputId = "kmi_last")
               )
             )
    )
  )
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Generamos un nuevo token. Valido hasta 2022-09-18
  acces_token <- c("eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJleHAiOjE2NjM0OTI3MDEsInR5cGUiOiJleHRlcm5hbCIsInVzZXIiOiJzdGVmYW5vYmVsdHJhbWUxMkBnbWFpbC5jb20ifQ.0-MqI4-TxscsDrNC0NRKr-Pkx_iGu4QOt2Sd3yeLQogHjTyOOb5dt48A6Yt-JAV6lyz35eWltm99nGFEz9CSSQ")
  
  BCRA_data_base <- bcra_data_extraction(token = acces_token) #ETL process for later use
  
  #actualDates <- seq()
  actual_dates <- seq(from = as.Date("1996-01-01"), to = Sys.Date(), by = "day")
  actual_dates <- as_tibble(actual_dates)
  
  BCRA_data_base <- 
    actual_dates %>%
    dplyr::rename(fecha = value) %>%
    dplyr::left_join(BCRA_data_base, by = "fecha") %>% #usamos el data frame que generamos con la funcion
    tidyr::fill(BM, .direction = c("down")) %>% 
    tidyr::fill(c(BM, RRII, log_bm, delta1_RRII, bm_usd, delta1_bm_usd, merval_usd, delta1_merval_usd), .direction = c("up")) %>% 
    dplyr::select(fecha, BM, RRII, log_bm, delta1_RRII, bm_usd, delta1_bm_usd, merval_usd, delta1_merval_usd)
  
  
  BCRA_historical_monthly_data <- 
    BCRA_data_base %>% 
    dplyr::filter(fecha == lubridate::floor_date(fecha, unit = "month"))
  
  BCRA_last_data <- 
    BCRA_historical_monthly_data %>% 
    dplyr::filter(fecha == max(fecha))
  
  reactive_data_date_filter <- reactive({ #generamos un reactive expresion para poder usar el "input data range"
    dplyr::filter(BCRA_data_base, dplyr::between(fecha, input$dateRange[1], input$dateRange[2]))
  })
  
  reactive_plots <- reactive({
    if("base" %in% input$MPInstrument) return(ggplotly(ggplot(data = reactive_data_date_filter())+ #usamos el reactive expresion
                                                         geom_line(mapping = aes(x = fecha, y = BM))+
                                                         labs(x = "fecha", y = "Base Monetaria")+
                                                         theme()))
    
    if("log_bm" %in% input$MPInstrument) return(ggplotly(ggplot(data = reactive_data_date_filter())+ #usamos el reactive expresion
                                                           geom_line(mapping = aes(x = fecha, y = log_bm))+
                                                           labs(x = "fecha", y = "Logaritmo Base Monetaria")+
                                                           theme()))
    if("bm_usd" %in% input$MPInstrument) return(ggplotly(ggplot(data = reactive_data_date_filter())+ #usamos el reactive expresion
                                                           geom_line(mapping = aes(x = fecha, y = bm_usd))+
                                                           labs(x = "fecha", y = "Base Monetaria en USD Corrientes")+
                                                           theme()))
    
    if("merval_usd" %in% input$MPInstrument) return(ggplotly(ggplot(data = reactive_data_date_filter())+ #usamos el reactive expresion
                                                               geom_line(mapping = aes(x = fecha, y = merval_usd))+
                                                               labs(x = "fecha", y = "Merval en USD")+
                                                               theme()))
    
    if("rrii" %in% input$MPInstrument) return(ggplotly(ggplot(data = reactive_data_date_filter()) + #usamos el reactive expresion
                                                         geom_line(mapping = aes(x = fecha, y = RRII)) +
                                                         labs(x = "fecha", y = "RR II") +
                                                         theme()))
    
    if("delta1_rrii" %in% input$MPInstrument) return(ggplotly(ggplot(data = reactive_data_date_filter())+
                                                                geom_line(mapping = aes(x = fecha, y = delta1_RRII)) +
                                                                labs(x = "fecha", y = "Variacion Diaria RRII") +
                                                                theme()))
    
  })
  
  output$graphics <- renderPlotly({
    graf = reactive_plots()
    print(graf)
  })
  #output$graf_RRII <- reactive(renderPlot({graf2}))
  #output$graf_usd_convert <- reactive(renderPlot({graf3}))
  #output$graf_varacum_RRII <- reactive(renderPlot({graf4}))
  
  output$kmi_last <- 
    renderTable({
      BCRA_last_data}, width = "100%", res = 96)
  
}

#https://stackoverflow.com/questions/60253265/update-stock-line-graph-based-on-daterangeinput-user-selection-shiny-app

#https://stackoverflow.com/questions/49388206/reactive-daterangeinput-for-shiny

# Run the application 
shinyApp(ui = ui, server = server)
