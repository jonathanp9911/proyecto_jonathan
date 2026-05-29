library(rvest)
library(jsonlite)
library(httr2)

# library(httr2)


# 1. Definimos la URL completa (Host + Path)
urlml <- "https://listado.mercadolibre.com.ve/inmuebles/apartamentos/alquiler/distrito-capital/caracas---el-hatillo-norte/el-encantado-del-hatillo-o-la-boyera/_NoIndex_True"

# 2. Definimos la cookie por separado para legibilidad
mis_cookies <- "_d2id=139ab701-a11a-496f-8f11-43efa9d6ad1b; _csrf=DKeNIHpI39qE76d-cvwO_jfS; _mldataSessionId=e1c7b09b-6878-4251-a38c-942d79c0027d; _bmstate=efbd580e84a72270756f9a1886c40f1534271b8b7a9d7f1791e7e984448a2cfd%3B2%3Ba4fed9382ce95749a1f5dc77a106342f6b1df9ff0f3c2a2e627e9b219d877968%3B1768963986%3B1c88efc95851a65c8efb3b93339351be5e8fdeafd327a9f1e978755da411638a; _bmc=efbd580e84a72270756f9a1886c40f1534271b8b7a9d7f1791e7e984448a2cfd%3B355; main_domain=; main_attributes=; categories=; last_query=; category=MLV1459; backend_dejavu_info=j%3A%7B%7D; c_Zfwxvk=1; g_state={\"i_l\":1,\"i_ll\":1768960388242,\"i_b\":\"yfJGjeCjU22diLmgQgdTI6+jkTmbKuF9S/sL7DrhDp0\",\"i_e\":{\"enable_itp_optimization\":0},\"i_p\":1768967592153}"

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
print(html_info)

html_info%>%
  html_nodes('.poly-component__title')%>%
  html_attr('href') 

# 5. Ejecutar la petición
# resp <- req |> req_perform()

# 6. Ver el resultado (status 200 significa éxito)
print(resp)


##################
library(rvest)

urlml <- "https://listado.mercadolibre.com.ve/inmuebles/apartamentos/alquiler/distrito-capital/caracas---el-hatillo-norte/el-encantado-del-hatillo-o-la-boyera/_NoIndex_True"

# 1. Cargamos la página en un navegador real (controlado por R)
# Esto abrirá un proceso de Chrome en background
sess <- read_html_live(urlml)

# 2. (Opcional) Si quieres ver lo que el navegador está viendo para depurar:
sess$view() 

# 3. Ahora extraemos los datos usando la sesión 'sess' en lugar del HTML estático
# Nota: MercadoLibre cambia las clases a menudo, verifica si '.ui-search-layout__item' sigue vigente
datos <- sess |> 
  html_elements(".ui-search-layout__item") |> 
  lapply(function(nodo) {
    list(
      # Usamos tryCatch por si algún elemento falta y no romper el loop
      titulo = tryCatch(nodo |> html_element(".poly-component__title") |> html_text2(), error = function(e) NA),
      precio = tryCatch(nodo |> html_element(".poly-price__current") |> html_text2(), error = function(e) NA)
    )
  }) |> 
  dplyr::bind_rows()

print(datos)


sess |> 
  html_elements(".poly-component__title")%>%
  html_attr('href')


# 1. Cierra la conexión con el navegador
sess$session$close()


# 2. (Opcional) Elimina el objeto de R para limpiar la memoria
rm(sess<)
gc() #
