library(rvest)

library(tidyverse)
urlml <- 'https://listado.mercadolibre.com.ve/inmuebles/apartamentos/alquiler/distrito-capital/caracas---chacao-sur/campo-alegre/_Desde_49_NoIndex_True'
links_ml <- read_html(urlml)%>%
  html_nodes('p')%>%
  html_attr('href')%>%
  unique()

