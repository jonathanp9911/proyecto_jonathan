# 
#
#

# Cargar Librerias.---- 

library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
# install.packages("tidytext") #En caso de no tener instalado el paquete tidytext 
# para el procesado de texto desmarcar el codigo anterior. 

#install.packages("wordcloud")
#install.packages("RColorBrewer")  

# 1.Importar Datos.----

df_alq_ccs_raw <- read.csv("df_zonas_caracas_info_completa_final_20260425.csv") 

# 2. Limpieza de datos y creación de un nuevo dataset----

df_alq_ccs_clean <- df_alq_ccs_raw %>%

# 2.1. Limpieza (Extración) de las varibles: precio, numero de  habitación, numero de baños, metros^2 
  mutate(
    precio_usd = as.numeric(str_replace_all(precio, "[^0-9]", "")),
    habitaciones = as.numeric(str_extract(datos, "\\d+(?=\\s*habitaciones)")),
    baños = as.numeric(str_extract(datos, "\\d+(?=\\s*baños)")),
    metros_2 = as.numeric(str_extract(datos, "\\d+(?=\\s*m²)"))
      ) %>% 
  mutate( 
    municipio_limpio = case_when(
      str_detect(municipios, "Baruta") ~ "Baruta",
      str_detect(municipios, "Chacao") ~ "Chacao",
      str_detect(municipios, "Sucre") ~ "Sucre",
      str_detect(municipios, "Hatillo") ~ "El Hatillo",
      str_detect(municipios, "Libertador") ~ "Libertador",
      TRUE ~ municipios # Si no coincide, deja el original
    )
    ) %>%
  select(municipio_limpio, zona_clean, titulo, precio_usd, habitaciones, baños,
         metros_2, url_publicacion)
  
# 3. Limpieza y analisis de la variable titulo----

# 3.1. Conteo de amenidades para evaluar la creación de variables dummy 

 qty_amenidades <- df_alq_ccs_clean %>%
   reframe(
     total_Apt = n(),
     Con_Vigilancia      = sum(str_detect(tolower(titulo), "vigilancia|seguridad|cerrada"), na.rm = TRUE),
     Con_Pozo_O_Agua     = sum(str_detect(tolower(titulo), "pozo|agua constante"), na.rm = TRUE),
     Amoblado_Equipado   = sum(str_detect(tolower(titulo), "amoblado|equipado"), na.rm = TRUE) ) %>%
  mutate(
    # Cálculo del porcentaje de penetración en el mercado
    Pct_Vigilancia      = (Con_Vigilancia / total_Apt) * 100,
    Pct_Pozo            = (Con_Pozo_O_Agua / total_Apt) * 100,
    Pct_Amoblado        = (Amoblado_Equipado / total_Apt) * 100
  ) 
 
 view(qty_amenidades) 
 
 # 3.2. Mineria de texto y analisis de de frecuencia
 
 # 3.2.1. Descargar stop words en español (basicamente conectores)
 
stop_words_esp <- get_stopwords(language = "es") 

 # 3.2.2. Tokenización y conteo de terminos en la variable titulo 

freq_palabras <- df_alq_ccs_clean %>% 
  select(titulo) %>%
  unnest_tokens(output = palabra, input = titulo) %>% # Descompone las frases en palabras individuales 
  anti_join(stop_words_esp, by = c("palabra" = "word")) %>% # Filtra conectores ("de", "con", "para", etc.)
  filter(!palabra %in% c(
    "alquiler", "apartamento", "apto", "alquila", "caracas", "inmuebles", "excelente",
    "baruta", "chacao", "sucre", "hatillo", "libertador", # Municipios
    "colinas", "bello", "monte", "palos", "grandes", "mercedes", "altamira" # Sectores comunes
  )) %>% 
  filter(!str_detect(palabra, "^\\d+$")) %>%
  count(palabra, sort = TRUE)
  
#write_csv(freq_palabras,"freq_palabras.csv")
  
  
# Vector con palabras consideradas ruido----

palabras_inutiles <-  c(
  # Códigos y plataformas
  "mls", "cod", "cód", "rah", "mls26", "at26", "es26", "d26", "flex",
  # Iniciales y basura de texto
  "yf", "yg", "jg", "jr", "k.f", "ng", "ls", "amc", "gc", "sc", "rd", "c.h", 
  "mp", "ijp", "jm", "db", "mb", "rg", "y.t", "pm", "sq", "ld", "mc", "ag", 
  "mjs", "mr", "s", "c", "vi", "ft",
  # Nombres de asesores
  "josmary", "sanjuan", "pastrán", "maría", "rosa", "barbara", 
  "andreina", "castro", "alvarez", "paula", "elena", "yanira", 
  "jessel", "monica", "mónica", "manuel", "belkis", "gonzalez", 
  "carlos", "carla", "christian", "szotyori", "ivanna",
  # Transacción y genéricos
  "alquiler", "alquilo", "venta", "se", "solo", "ofrece", "presenta", 
  "cliente", "clientes", "ubicación", "ubicado", "zona", "urbanización",
  "urbanizacion", "urb", "resd", "edificio", "calle", "piso", "conjunto", "anexo", "
  tipo", "alta", "baja", "grande",
  # Geografía macro y micro
  "caracas", "distrito", "metropolitano",
  "sur", "norte", "santa", "naranjos", "lomas", "valle", "arriba",
  "castellana", "tahona", "eduvigis", "cafetal", "alameda", "chorros",
  "samanes", "boyera", "marin", "sol", "lagunita", "fe", "florida", "escampadero", 
  "solar", "terrazas", "avila", "ávila", "chuao", "san", "club", "sebucan", "sebucán", 
  "lima", "urbina", "caminos", "cigarral", "bernardino", "carmen", "medina", "campo",
  "humboldt", "macaracuay", "marques", "country", "manzanares", "boleita", "chulavista", 
  "rosal", "guaicay", "paraíso", "paraiso", "sabana", "esmeraldas", "california", "cumbres", 
  "curumo", "mesetas", "campitos",
  # Duplicados estructurales
  "m2", "mts", "metros", "habitaciones", "baños", "baño",
  "habitación", "hab", "puestos", "puesto", "2b", "3h", "2h", 
  "1b", "1h", "2p", "1p", "3b"
)
  
# Conteo de palabras sin ruido-----

freq_palabras_fltda <- freq_palabras %>% 
  filter(!palabra %in% palabras_inutiles)

# Codigo para limpiar el Environment----
rm(list = ls())

# Notas----

# 1. Este codigo fue escrito con asistencia parcial de gemini. 
# 2. Pendiente por limpiar datos anomalos, duplicados y faltantes (27-05-2026)
       