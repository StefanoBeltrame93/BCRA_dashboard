library(tidyverse)
library(httr)
library(jsonlite)
library(shiny)
library(lubridate)
library(plotly)
library(scales)
library(zoo)
library(shinythemes)

acces_token <- c("eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJleHAiOjE2NjM1MTE3MTYsInR5cGUiOiJleHRlcm5hbCIsInVzZXIiOiJzdGVmYW5vYmVsdHJhbWUxMkBnbWFpbC5jb20ifQ.mrM0ox2paudKGjTgoxrFDh4nSIqcPzQ3yoV_pGNYl7h2Tdx7FdiG_gdvBg9qD1nyPu28Ho3uVBRGwzQKJqDSpw")

bcra_api <- function(path){
  url = modify_url(url = "https://api.estadisticasbcra.com", path = path)
  GET(url, add_headers(Authorization = stringr::str_c("bearer", " ", acces_token))
  )
}

bm_resp <-  bcra_api(path = "/base") #Base Monetaria 
rrii_resp <- bcra_api(path = "/reservas") #Reservas Int
usd_resp <- bcra_api(path = "/usd") #Evol Tipo de Cambio
bm_usd_resp <- bcra_api(path = "/base_usd") #Base Monetaria % TC
bm_rrii_resp <- bcra_api(path = "/base_div_res") #base % RRII
merval_usd_resp <- bcra_api(path = "/merval_usd") #Merval % USD


fromJSON(content(bm_resp, "text", encoding = "UTF-8"))
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


BCRA_historical_monthly_data <- 
  BCRA_data_base %>% 
  dplyr::filter(fecha == lubridate::floor_date(fecha, unit = "month"))

BCRA_last_data <- 
  BCRA_historical_monthly_data %>% 
  dplyr::filter(fecha == max(fecha))
