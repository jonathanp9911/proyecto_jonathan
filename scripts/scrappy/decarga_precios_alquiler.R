library(tidyverse)
library(rvest)
library(httr2)







sess1 <- read_html_live(url_municipios_zonas[1])
# sess1$session$close()
# rm(sess1)
# gc() #
sess1$view() 


sess1$click(".old-user-button")

url_login <- sess1$session$Runtime$evaluate("window.location.href")$result$value

print(url_login)

sess1 <- read_html_live(url_login)
sess1$view() 


sess1$type("input[data-testid='user_id']", "jonathanpv9911@gmail.com")
### requiere acepta captcha manualmente en el session View


url_validacion <- sess1$session$Runtime$evaluate("window.location.href")$result$value
sess1 <- read_html_live(url_validacion)
sess1$view() 
url_confirmacion <- sess1$session$Runtime$evaluate("window.location.href")$result$value
sess1 <- read_html_live(url_confirmacion)


sess1$click("button[aria-labelledby='code_validation-content']")


url_codigoconfirmacion <- sess1$session$Runtime$evaluate("window.location.href")$result$value
sess1 <- read_html_live(url_codigoconfirmacion)
sess1$view() 

codigo_correo <- '624154'
sess1$type("input[aria-label='Dígito 1']", codigo_correo)
sess1$click("span[data-andes-button-content='true']")# hacer click en enviar codigo

# ejecutar si lo anterior da error
selector <- "span[data-andes-button-content='true']"
script_js <- sprintf("document.querySelector(\"%s\").click();", selector)
sess1$session$Runtime$evaluate(script_js)


#### opcional reeviar codigo
sess1$click("span.validation-form__button--send-email")

# #### obtener cookies
# cookies_raw <- sess1$session$Network$getCookies()$cookies
# 
# # 2. Convertir esa lista compleja en un dataframe (tibble) ordenado
# df_cookies <- map_dfr(cookies_raw, as_tibble)
# 
# # 3. Ver las cookies extraídas (especialmente las del dominio de Mercado Libre)
# df_cookies |> 
#   select(name, value, domain, httpOnly, secure, session) |> 
#   print()

####inciar descargas
urlgeneral_ccs <- 'https://listado.mercadolibre.com.ve/inmuebles/apartamentos/alquiler/distrito-capital/'

municipios_raw <-  c("Caracas - Baruta (central)",
                     "Caracas - Baruta (este)",
                     "Caracas - Baruta (norte)",
                     "Caracas - Baruta (sur)",
                     "Caracas - Baruta (sureste)",
                     "Caracas - Chacao (norte)",
                     "Caracas - Chacao (sur)",
                     "Caracas - El Hatillo (norte)",
                     "Caracas - El Hatillo (sur)",
                     "Caracas - Libertador (centro)",
                     "Caracas - Libertador (noreste)",
                     "Caracas - Libertador (oeste)",
                     "Caracas - Libertador (sur)",
                     "Caracas - Libertador (sureste)",
                     "Caracas - Libertador (suroeste)",
                     "Caracas - Sucre (centro)",
                     "Caracas - Sucre (este)",
                     "Caracas - Sucre (noreste)",
                     "Caracas - Sucre (norte)",
                     "Caracas - Sucre (sur)")#

municipios_clean <- municipios_raw%>%
  tolower()|>
  str_replace_all(' - ','---')|>
  str_replace_all(' \\(| ','-')|>
  str_remove_all('\\)')

url_municipios <- paste0('https://listado.mercadolibre.com.ve/inmuebles/apartamentos/alquiler/distrito-capital/',
                         municipios_clean,
                         '/')

url_municipios_zonas <- paste0(url_municipios,
                               "_FiltersAvailableSidebar?filter=neighborhood")


# comprobacion de que navegacion funciona
sess1 <- read_html_live(url_municipios_zonas[i])
sess1$view() 



datos_zonas_raw <- sess1%>%
  html_nodes('span')%>%
  html_text2()%>%
  unique()%>%
  .[.!='']

df_zonas_caracas <- tibble(municipios=character(),
                           url_municipio= character(),
                           zona_raw= character(),
                           zona_clean= character(),
                           url_zona= character(),
                           indice_vuelta_municipio=integer())


for( i in 1:length(url_municipios_zonas)){
  print(i)
  datos_zonas_raw <- NULL
  datos_zonas_clean <- NULL
  df_info_temp <- NULL
  
  sess1 <- read_html_live(url_municipios_zonas[i])
  
  
  
  datos_zonas_raw <- sess1%>%
    html_nodes('span')%>%
    html_text2()%>%
    unique()%>%
    .[.!='']
  
  datos_zonas_clean <-datos_zonas_raw %>%
    tolower()%>%
    chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU",.)%>%
    str_replace_all(' ','-')%>%
    str_remove_all('\\.|,')
  
  url_zonas <- paste0(url_municipios[i],
                      datos_zonas_clean,
                      '/')
  
  if(!is.null(datos_zonas_clean)&length(datos_zonas_clean)>1){
    
    df_info_temp <- tibble(municipios=municipios_raw[i],
             url_municipio= url_municipios_zonas[i],
             zona_raw= datos_zonas_raw,
             zona_clean= datos_zonas_clean,
             url_zona= url_zonas,
             indice_vuelta_municipio=i)
             
             
    df_zonas_caracas <- bind_rows(df_zonas_caracas,
                                  df_info_temp
                                  )
    print(nrow(df_zonas_caracas))
                                  
    
  }else{
    print(paste('revisar descarga',i ))
  }

  Sys.sleep(3)
}





saveRDS(df_zonas_caracas,'df_zonas_caracas.rds')
##########################

url_zona1 <- df_zonas_caracas$url_zona[1]


sess1 <- read_html_live(url_zona1)

sess1$view() 
'<div class="poly-component__attributes-list"><ul class="poly-attributes_list" style="--separator-content:&quot;|&quot;;gap:2px"><li class="poly-attributes_list__item poly-attributes_list__separator">2 habitaciones</li><li class="poly-attributes_list__item poly-attributes_list__separator">1 baño</li><li class="poly-attributes_list__item poly-attributes_list__separator">80 m² cubiertos</li></ul></div>'
temp <- sess1 |> 
  html_elements(".ui-search-layout__item") |> 
  lapply(function(nodo) {
    list(
      # Usamos tryCatch por si algún elemento falta y no romper el loop
      titulo = tryCatch(nodo |> html_element(".poly-component__title") |> html_text2(), error = function(e) NA),
      precio = tryCatch(nodo |> html_element(".poly-price__current") |> html_text2(), error = function(e) NA),
      datos= tryCatch(nodo |> html_element(".poly-attributes_list") |> html_text2(), error = function(e) NA),
      location= tryCatch(nodo |> html_element(".poly-component__location") |> html_text2(), error = function(e) NA)
    )
  }) |> 
  dplyr::bind_rows() #poly-component__attributes-list
View(temp)
# print(datos)
#

sess1 <- read_html_live('https://listado.mercadolibre.com.ve/inmuebles/apartamentos/alquiler/distrito-capital/caracas---baruta-central/clnas-de-valle-arriba/_NoIndex_True#applied_filter_id%3Dneighborhood%26applied_filter_name%3DBarrios%26applied_filter_order%3D3%26applied_value_id%3DTUxWQkNMTjYwNjQ1Nw%26applied_value_name%3DClnas.+de+Valle+Arriba%26applied_value_order%3D2%26applied_value_results%3D438%26is_custom%3Dfalse')
sess1 |> 
  html_elements(".poly-component__title")%>%
  html_attr('href')

cantidad_publicaciones <- sess1 |>
  html_elements('.ui-search-search-result__quantity-results')%>%
  html_text()%>%
  str_extract("\\d+")%>%
  as.numeric()
j=1

# df_zonas_caracas_info_completa <- tibble(municipios=character(),
#                                          url_municipio= character(),
#                                          zona_raw= character(),
#                                          zona_clean= character(),
#                                          url_zona= character(),
#                                          indice_vuelta_municipio=integer(),
#                                          cantidad_publicaciones_zona= integer(),
#                                          cantidad_vueltas_zona_sub= integer(),
#                                          titulo=character(),
#                                          precio=character(),
#                                          datos=character(),
#                                          location=character(),
#                                          url_publicacion= character(),
#                                          indice_vuelta_zona= integer(),
#                                          indice_vuelta_zona_sub=integer()
#                                          )

# j=22
for( j in 124:nrow(df_zonas_caracas)){
  print(paste('visitando',df_zonas_caracas$zona_raw[j],'vuelta',j))
  
  sess1 <- read_html_live(df_zonas_caracas$url_zona[j])
  # sess1$view() 
  cantidad_publicaciones <- sess1 |>
    html_elements('.ui-search-search-result__quantity-results')%>%
    html_text()%>%
    str_extract("\\d+")%>%
    as.numeric()
  
  cdad_vueltas <- ceiling(cantidad_publicaciones/49)
  
#   
  if(cdad_vueltas>0){
    # url_info_zonas <- df_zonas_caracas$url_zona[j]
    
    df_info_obtenida <- NULL
    
    df_info_obtenida <- sess1 |> 
      html_elements(".ui-search-layout__item") |> 
      lapply(function(nodo) {
        list(
          # Usamos tryCatch por si algún elemento falta y no romper el loop
          titulo = tryCatch(nodo |> html_element(".poly-component__title") |> html_text2(), error = function(e) NA),
          precio = tryCatch(nodo |> html_element(".poly-price__current") |> html_text2(), error = function(e) NA),
          datos= tryCatch(nodo |> html_element(".poly-attributes_list") |> html_text2(), error = function(e) NA),
          location= tryCatch(nodo |> html_element(".poly-component__location") |> html_text2(), error = function(e) NA)
        )
      }) |> 
      dplyr::bind_rows() #poly-component__attributes-list

    url_visitar_aptos <- sess1 |> 
      html_elements(".poly-component__title")%>%
      html_attr('href')
    
    df_zonas_caracas_info_completa_temp <- NULL
    if(!is.null(df_info_obtenida)&nrow(df_info_obtenida)>0){
      df_zonas_caracas_info_completa_temp <- tibble(municipios= df_zonas_caracas$municipios[j],
                                                    url_municipio= df_zonas_caracas$url_municipio [j],
                                                    zona_raw= df_zonas_caracas$zona_raw [j],
                                                    zona_clean= df_zonas_caracas$zona_clean [j],
                                                    url_zona= df_zonas_caracas$zona_clean [j],
                                                    indice_vuelta_municipio= df_zonas_caracas$indice_vuelta_municipio [j],
                                                    cantidad_publicaciones_zona= cantidad_publicaciones,
                                                    cantidad_vueltas_zona_sub= cdad_vueltas,
                                                    titulo= df_info_obtenida$titulo,
                                                    precio= df_info_obtenida$precio,
                                                    datos= df_info_obtenida$datos,
                                                    location= df_info_obtenida$location,
                                                    url_publicacion= url_visitar_aptos,
                                                    indice_vuelta_zona= j,
                                                    indice_vuelta_zona_sub=1
      )
      if(nrow(df_zonas_caracas_info_completa_temp)>0){
        df_zonas_caracas_info_completa <- bind_rows(df_zonas_caracas_info_completa,
                                                    df_zonas_caracas_info_completa_temp)
      }
      
    }
  
    
    if(cdad_vueltas>1){
      
      for(k in 1:cdad_vueltas){
        Sys.sleep(10)
        print(paste('visitando, subvuelta',df_zonas_caracas$zona_raw[j],'vuelta',j,'.subvuelta',k,'de',cdad_vueltas))
        valor_intermedio <- ((k-1)*48)+1
        url_info_zonas <- paste0(df_zonas_caracas$url_zona[j],
                                 '_Desde_',valor_intermedio,'_NoIndex_True')
        
        sess1 <- read_html_live(url_info_zonas)
        # sess1$view() 
        df_info_obtenida <- NULL
        
        df_info_obtenida <- sess1 |> 
          html_elements(".ui-search-layout__item") |> 
          lapply(function(nodo) {
            list(
              # Usamos tryCatch por si algún elemento falta y no romper el loop
              titulo = tryCatch(nodo |> html_element(".poly-component__title") |> html_text2(), error = function(e) NA),
              precio = tryCatch(nodo |> html_element(".poly-price__current") |> html_text2(), error = function(e) NA),
              datos= tryCatch(nodo |> html_element(".poly-attributes_list") |> html_text2(), error = function(e) NA),
              location= tryCatch(nodo |> html_element(".poly-component__location") |> html_text2(), error = function(e) NA)
            )
          }) |> 
          dplyr::bind_rows() #poly-component__attributes-list
        
        url_visitar_aptos <- sess1 |> 
          html_elements(".poly-component__title")%>%
          html_attr('href')
        
        df_zonas_caracas_info_completa_temp <- NULL
        if(!is.null(df_info_obtenida)&nrow(df_info_obtenida)>0){
          df_zonas_caracas_info_completa_temp <- tibble(municipios= df_zonas_caracas$municipios[j],
                                                        url_municipio= df_zonas_caracas$url_municipio [j],
                                                        zona_raw= df_zonas_caracas$zona_raw [j],
                                                        zona_clean= df_zonas_caracas$zona_clean [j],
                                                        url_zona= df_zonas_caracas$zona_clean [j],
                                                        indice_vuelta_municipio= df_zonas_caracas$indice_vuelta_municipio [j],
                                                        cantidad_publicaciones_zona= cantidad_publicaciones,
                                                        cantidad_vueltas_zona_sub= cdad_vueltas,
                                                        titulo= df_info_obtenida$titulo,
                                                        precio= df_info_obtenida$precio,
                                                        datos= df_info_obtenida$datos,
                                                        location= df_info_obtenida$location,
                                                        url_publicacion= url_visitar_aptos,
                                                        indice_vuelta_zona= j,
                                                        indice_vuelta_zona_sub=k
          )
          if(nrow(df_zonas_caracas_info_completa_temp)>0){
            df_zonas_caracas_info_completa <- bind_rows(df_zonas_caracas_info_completa,
                                                        df_zonas_caracas_info_completa_temp)
          }
        }
      }
    }
    print(paste('cdad datos:',nrow(df_zonas_caracas_info_completa)))
    Sys.sleep(8)
  }
}


# si se van a descargar los datos incluir un gc() cada cierta cantidad de vueltas
# View(df_zonas_caracas)

df_zonas_caracas_info_completa_final <- df_zonas_caracas_info_completa%>%
  distinct()

nrow(df_zonas_caracas_info_completa_final)


saveRDS(df_zonas_caracas_info_completa_final,'df_zonas_caracas_info_completa.rds')
write_csv(df_zonas_caracas_info_completa_final,'df_zonas_caracas_info_completa_final_20260425.csv')
# df_zonas_caracas$url_zona
# 
# 'https://listado.mercadolibre.com.ve/inmuebles/apartamentos/alquiler/distrito-capital/_Desde_49_NoIndex_True'
# 'https://listado.mercadolibre.com.ve/inmuebles/apartamentos/alquiler/distrito-capital/caracas---baruta-central/cerro-verde/_Desde_49_NoIndex_True'
# # n el navegador
# sess1$session$close()


# 2. (Opcional) Elimina el objeto de R para limpiar la memoria
rm(sess1)
gc() #
