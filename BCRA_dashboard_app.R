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


# Define UI for application
ui <- fluidPage(
  theme = shinythemes::shinytheme("cosmo"),
  tabsetPanel(
    type = "tabs",
    tabPanel(title = "Graficos",
             br(),
             titlePanel("BCRA - Politica Monetaria"),
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
                  label = "seleccionar el rango de fechas",
                  min = "1996-01-01",
                  #max = as.character(max(BCRA$fecha)),
                  weekstart = 1)
              ),
              
              mainPanel(
                titlePanel(title = "Principales Indicadores"),
                plotlyOutput(outputId = "graphics")
                  )
              )
            )
    )
    
  )
  

                
                




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  bcra_api <- function(path){
    url = modify_url(url = "https://api.estadisticasbcra.com", path = path)
    GET(url, add_headers(Authorization = "bearer eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJleHAiOjE2MzAwNzcxOTEsInR5cGUiOiJleHRlcm5hbCIsInVzZXIiOiJzdGVmYW5vYmVsdHJhbWUxMkBnbWFpbC5jb20ifQ.RmpQ--4wQ3YDo5LnbAQKx7cg9yeEH1PFwcJC5pWgbexUykrRotXH58Hu_OSPGQfZnszJrjyMnkav5c3Nqi3hGg")
    )
  }
  
  bm_resp <-  bcra_api(path = "/base") #Base Monetaria 
  rrii_resp <- bcra_api(path = "/reservas") #Reservas Int
  usd_resp <- bcra_api(path = "/usd") #Evol Tipo de Cambio
  bm_usd_resp <- bcra_api(path = "/base_usd") #Base Monetaria % TC
  bm_rrii_resp <- bcra_api(path = "/base_div_res") #base % RRII
  merval_usd_resp <- bcra_api(path = "/merval_usd") #Merval % USD

    
  #Base Monetaria Data
  base_monetaria_data <- 
    dplyr::as_tibble(jsonlite::fromJSON(content(bm_resp, "text", encoding = "UTF-8"))) %>% 
      dplyr::rename(fecha = d, BM = v) %>% 
      dplyr::mutate(fecha = lubridate::as_date(fecha),
                    log_bm = log(BM),
                    delta_bm = BM - lag(BM, n = 1L),
                    daily_var_bm = delta_bm / BM)

  #RRII Data
  rrii_data <- 
    fromJSON(content(rrii_resp, "text", encoding = NULL)) %>%
      dplyr::as_tibble() %>% 
      dplyr::rename(fecha = d, RRII = v) %>%
      dplyr::mutate(lag1_RRII = lag(x = RRII, n = 1L),
                    delta1_RRII = RRII - lag1_RRII) %>%
      tidyr::replace_na(list(lag1_RRII = 0, delta1_RRII = 0)) %>%  #Me pone un 0 en el primer valor
      dplyr::mutate(cumsum_RRII = cumsum(delta1_RRII),
                    dailyvar_RRII = delta1_RRII / RRII,
                    fecha = lubridate::as_date(fecha))
  
  
  # BM div USD 
   bm_usd_data <-
    jsonlite::fromJSON(content(bm_usd_resp, "text", encoding = "UTF-8")) %>% 
    dplyr::as_tibble() %>% 
    dplyr::rename(fecha = d, bm_usd = v) %>% 
    dplyr::mutate(lag1_bm_usd = lag(x = bm_usd, n = 1L),
                  delta1_bm_usd = bm_usd -lag1_bm_usd) %>% 
    tidyr::replace_na(list(lag1_bm_usd = 0, delta1_bm_usd = 0)) %>% 
    dplyr::mutate(cumsum_bm_usd = cumsum(delta1_bm_usd),
                  dailyvar_bm_usd = delta1_bm_usd / bm_usd,
                  fecha = lubridate::as_date(fecha))
   
  #Merval in USD 
  merval_usd_data <- 
    jsonlite::fromJSON(content(merval_usd_resp, "text", encoding = "UTF-8")) %>% 
    dplyr::as_tibble() %>% 
    dplyr::rename(fecha = d, merval_usd = v) %>% 
    dplyr::mutate(lag1_merval_usd = lag(x = merval_usd, n = 1L),
                  delta1_merval_usd = merval_usd - lag1_merval_usd) %>% 
    tidyr::replace_na(list(lag1_merval_usd = 0, delta1_merval_usd = 0)) %>% 
    dplyr::mutate(cumsum_merval_usd = cumsum(delta1_merval_usd),
                  dailyvar_merval_usd = delta1_merval_usd / merval_usd,
                  fecha = lubridate::as_date(fecha))
   
  
    
  
  
  BCRA_data_base <- 
    base_monetaria_data %>% 
    left_join(rrii_data, by = "fecha") %>% 
    left_join(bm_usd_data, by = "fecha") %>% 
    left_join(merval_usd_data, by = "fecha")
    
  #actualDates <- seq()
  actual_dates <- seq(from = as.Date("1996-01-01"), to = Sys.Date(), by = "day")
  actual_dates <- as_tibble(actual_dates)
  
  BCRA_data_base <- 
    actual_dates %>%
      dplyr::rename(fecha = value) %>%
      dplyr::left_join(BCRA_data_base, by = "fecha") %>% 
      tidyr::fill(BM, .direction = c("down")) %>% 
      tidyr::fill(c(BM, RRII, log_bm, delta1_RRII, bm_usd, delta1_bm_usd, merval_usd, delta1_merval_usd), .direction = c("up")) %>% 
      dplyr::select(fecha, BM, RRII, log_bm, delta1_RRII, bm_usd, delta1_bm_usd, merval_usd, delta1_merval_usd)

    
  
  reactive_data_date_filter <- reactive({ #generamos un reactive expresion para poder usar el "input data range"
    dplyr::filter(BCRA_data_base, dplyr::between(fecha, input$dateRange[1], input$dateRange[2]))
  })
  
  colPlotTest <- reactive({
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
    graf = colPlotTest()
    print(graf)
  })
  #output$graf_RRII <- reactive(renderPlot({graf2}))
  #output$graf_usd_convert <- reactive(renderPlot({graf3}))
  #output$graf_varacum_RRII <- reactive(renderPlot({graf4}))
}

#https://stackoverflow.com/questions/60253265/update-stock-line-graph-based-on-daterangeinput-user-selection-shiny-app

#https://stackoverflow.com/questions/49388206/reactive-daterangeinput-for-shiny

# Run the application 
shinyApp(ui = ui, server = server)
