library(jsonlite)
library(httr2)

# necesita ejecutar previamente script descarga_precios_alquiler 
# de la lina 1 a la 57 iniciando sesi'on usuario

urlml <- "https://listado.mercadolibre.com.ve/inmuebles/apartamentos/alquiler/distrito-capital/caracas---el-hatillo-norte/el-encantado-del-hatillo-o-la-boyera/_NoIndex_True"

# 2. Definimos la cookie por separado para legibilidad
mis_cookies <- df_cookies$value[16]
urlml <- "https://listado.mercadolibre.com.ve/inmuebles/apartamentos/alquiler/distrito-capital/caracas---el-hatillo-norte/el-encantado-del-hatillo-o-la-boyera/_NoIndex_True"

# 3. Construimos el request
req <- request(urlml) |> 
  req_headers(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language" = "es-ES,es;q=0.8,en-US;q=0.5,en;q=0.3",
    # "Accept-Encoding" = "gzip, deflate, br, zstd",  <-- ESTA LÍNEA SE ELIMINA
    "Sec-GPC" = "1",
    "Connection" = "keep-alive",
    "Upgrade-Insecure-Requests" = "1",
    "Sec-Fetch-Dest" = "document",
    "Sec-Fetch-Mode" = "navigate",
    "Sec-Fetch-Site" = "none",
    "Sec-Fetch-User" = "?1",
    "Priority" = "u=0, i",
    "Cookie" = mis_cookies
  ) |> 
  req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:145.0) Gecko/20100101 Firefox/145.0")

resp <- req |> 
  req_perform()

resp$status_code

html_info <- resp |> 
  resp_body_html()

# Verificamos que cargó bien
# print(html_info)

html_info%>%
  html_nodes('.poly-component__title')%>%
  html_attr('href') 


html_info%>%
  html_nodes('p')%>%
  html_text2()
