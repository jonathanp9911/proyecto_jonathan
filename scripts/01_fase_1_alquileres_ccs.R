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
   
   
   


   
  
   


   
   
   
   
   
   