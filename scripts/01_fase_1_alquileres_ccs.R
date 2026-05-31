# 
# 
# 
#
#

# 1. Cargar librerias de trabajo----
#
# Inicialmente trabajaremos con las siguientes librerias: 

library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer) 
library(DT)
library(gt)

#En caso de no tener los paquetes instalados debemos usar el siguinte codigo: 
#install.packages("tidyverse")
#install.packages("DT")
#install.packages("gt") 
#install.packages("tidytext")
#install.packages("wordcloud") 
#install.packages("RcolorBrewer")
#install.packages("scales")

# 2. Importar, inspeccionar y ordenar datos----

# 2.1. Importar el conjunto de datos csv a trabajar. 

alq_ccs_raw <- read_csv("datos/df_zonas_caracas_info_completa_final_20260425.csv")

# 2.2. Inspeccionar el conjunto de datos
#        
#     Se busca identificar información basica del CD: determinar su dimensión,
#     nombre de las columnas entre otros. 
# 

# Dimensión del conjunto de datos. 
  dim(alq_ccs_raw) #Este codigo permite visualizar en la consola
                   # el numerdo de filas y columnas de un CD. 
  
# Nombre de las columnas. 
  colnames(alq_ccs_raw) 

# Estructura de los datos. 
   glimpse(alq_ccs_raw) # Esta función sirve para inspeccionar rapidamente el conjunto de datos
                        # muestra el numero de filas y columnas, los nombres de cada columna
                        # (variable), su tipo de dato y un adelanto de sus valore. 
   
# Tabla 1. Estructura del conjunto de datos crudos. 
   
   alq_ccs_raw %>%               
     slice_head(n = 5) %>%   #Esta función le indica a r que solo muestra una muestra de 5 filas dle
     gt()                    # cd

   
# Tabla 2. Tipos de datos.
    
   sapply(alq_ccs_raw, class) %>%
     enframe(name = "Variable", value = "Tipo de dato") %>%
     gt() %>%
     tab_header(
       title = "Tipo de datos de cada variable"
     )
   
   
   
   
# Opcional: Crear un csv pequeño para analizarlo utilizando IA para identificar información 
# Relevante de conjunto de datos. 
   
# preview_datos <- alq_ccs_raw  %>%           # Este codigo crea un nuevo df que contiene solo una    
#                   slice_head(n = 5).        # muestra de solo 5 observaciones del conjunto de datos. 
   
# write_csv(preview_datos,'datos/preview_datos.csv')
   
   
# 2.3. Ordenar conjunto de datos  (Eliminar variables no relevantes, normalizar variable datos)
   
   alq_ccs_clean <- alq_ccs_raw %>%  
     mutate(
       precio_usd = as.numeric(str_replace_all(precio, "[^0-9]", "")),
       habitaciones = as.numeric(str_extract(datos, "\\d+(?=\\s*habitaciones)")),
       baños = as.numeric(str_extract(datos, "\\d+(?=\\s*baños)")),
       metros_2 = as.numeric(str_extract(datos, "\\d+(?=\\s*m²)"))
     ) %>%
     select(titulo, precio_usd, habitaciones, baños, metros_2, municipios, zona_clean, location)
   
   
# 2.3.1. Tabla Resumen del Conjunto de Datos. 
   
# Paso 1. Crear una tibble de texto con las descripciones de las variables. 
   
   descripciones <- tibble( 
     Variable = c("titulo", "precio_usd", "habitaciones", 
                 "baños", "metros_2", "municipios", 
                 "zona_clean", "location"),
     Descripción = c(
       "Título original descriptivo de la oferta en la plataforma",
       "Canon de arrendamiento mensual expresado en dólares (USD)",
       "Número total de habitaciones del inmueble",
       "Número total de baños del inmueble",
       "Área de construcción del inmueble en metros cuadrados",
       "Municipio de la Gran Caracas donde se ubica la propiedad",
       "Urbanización o sector específico normalizado",
       "Ubicación geográfica general extraída del portal"
     )
    )

# Paso 2. Crear la estructura de la tabla. 
   
   sapply(alq_ccs_clean, class) %>%
     enframe(name = "Variable", value = "Clase_R") %>% 
     mutate(
       Tipo = case_when( 
         Clase_R == "numeric" ~ "Cuantitativa",
         Clase_R == "character" ~ "Cualitativa",
         TRUE ~ "Otro"
         )
     ) %>%
     left_join(descripciones, by = 'Variable') %>%
     select(Variable, Descripción, Tipo) %>%
     gt()
   
   view(tabla_resumen) 
   
   
# 2.4. Variable Titulo. 

# Realizaremos un proceso de basico de mineria de texto para extraer la información mas relevante
# de la variable titulo. 
   
# 2.4.1. Lista de palabras mas usadas

# Paso 1: Crear el lista de palabras vacias / stopwords para posteriormente removerlas.
# Esto incluye conectores como "de", "con", "el", "en", que no aportan valor al análisis.

stop_words_es <- get_stopwords(language = "es") 

# Paso 2: Tokenización y conteo de palabras.

freq_words <- alq_ccs_clean %>%
  select(titulo) %>%
  unnest_tokens(output = palabra, input = titulo) %>%
  anti_join(stop_words_es, by = c("palabra" = "word")) %>%
  count(palabra, sort= TRUE)
   
   
# Opcional: Guardar la lista de palabras para que la IA identifique palabras claves.

# write.csv(freq_words,"datos/freq_words.csv")
   
   
   
    
# 2.4. Datos anomalos o OUTLIERS

# Para la detección de valores anomalos usaremos el test de Tukey (METODO DEL RANGO INTERCUARTIL)
# haremos 2 iteraciones sobre el conjunto de datos uno global y uno por municipio. 
   

# 2.4.1. Test Tukey ("Global").
   
# Paso 1: Analisis Visual. 
   
  ggplot(alq_ccs_clean, aes(y = precio_usd)) + 
           geom_boxplot(fill= "steelblue", alpha = 0.7) +
           theme_minimal() 
         
   
summary_stats <- alq_ccs_clean %>%
      select(precio_usd, metros_2) %>%
      summary() %>%
      as.data.frame.matrix()

rownames(summary_stats) <- c("Min", "1st Qu", "Median", 
                               "Mean", "3rd Qu", "Max")

summary_stats_price <- 




# Asegúrate de cargar la librería scales (viene instalada con el tidyverse)
#library(scales)

ggplot(alq_ccs_clean) +
  aes(y = precio_usd) + 
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  theme_bw(base_size = 11) + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  
  # 1. SOLUCIÓN A LOS NÚMEROS: Formateamos el eje Y
  # Esto quita la notación científica y pone el formato de moneda ($1,000)
  scale_y_continuous(labels = label_dollar(big.mark = ".", decimal.mark = ",")) +
  
  # 2. SOLUCIÓN A LA CAJA APLASTADA: Hacemos un "Zoom"
  # Cambia el 5000 por el precio máximo que consideres razonable ver en el gráfico.
  # Esto corta visualmente el gráfico en $5,000 para que la caja se pueda expandir y verse clara.
  coord_cartesian(ylim = c(0, 5000)) +
  
  labs(
    title = "Distribución General de Precios de Alquiler (Zoom aplicado)",
    y = "Precio (USD)",
    x = NULL
  )
   