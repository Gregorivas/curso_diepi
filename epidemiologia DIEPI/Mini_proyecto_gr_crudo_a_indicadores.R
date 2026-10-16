#===================================================================================
# Documento Metodológico: Cálculo de Tasas de Incidencia por 100,000 Habitantes
# Proyecto: Análisis Espacial y Temporal de Leptospirosis en República Dominicana
# Autor: Gregorio Rivas
# Objetivo: Transformar una base de datos cruda de casos (linelist) 
            # en una tabla de tasas de incidencia estandarizadas por provincia.

  ## Fundamento Metodológico (La Lógica Epidemiológica). fórmula epidemiológica:
    ## Tasa = (Número de casos / Población en riesgo) × Factor de escala
      ## Numerador: Casos confirmados de Leptospirosis (extraídos del CSV crudo).
      ## Denominador: Población total de la provincia (simulada en este ejercicio, 
       ## pero en la realidad vendría del Censo o ONE).
      ## Factor de escala: 100,000 (1e5). Se usa para que la tasa sea legible 
       ## (ej. "50 casos por cada 100,000 habitantes" en lugar de "0.0005").

      ## Cada línea tiene un comentario (#) explicando qué hace la función 
       ## y por qué es importante metodológicamente.

#==================================================================================
# FASE 1: CONFIGURACIÓN E IMPORTACIÓN DE DATOS: Cargar las librerías necesarias
# ==============================================================================

install.packages(c("tidyverse", "janitor", "lubridate", "stringr", "tidyr", "tibble", "readxl", 
                   "writexl"))
install.packages(ggplot2)
install.packages(readr)

library(tibble)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(lubridate)
library(writexl)

library(ggplot2)
library(readr)

# ==============================================================================
# DATOS REALES: POBLACIÓN POR PROVINCIA (Censo 2022 - ONE)
# ==============================================================================

poblacion_RD_censo_2022 <- tibble(
  provincia <-  c(
    "Azua", "Baoruco", "Barahona", "Dajabón", "Distrito Nacional", 
    "Duarte", "Elías Piña", "El Seibo", "Espaillat", "Hato Mayor", 
    "Hermanas Mirabal", "Independencia", "La Altagracia", "La Romana", 
    "La Vega", "María Trinidad Sánchez", "Monseñor Nouel", "Monte Cristi", 
    "Monte Plata", "Pedernales", "Peravia", "Puerto Plata", "Samaná", 
    "San Cristóbal", "San José de Ocoa", "San Juan", "San Pedro de Macorís", 
    "Sánchez Ramírez", "Santiago", "Santiago Rodríguez", "Santo Domingo", 
    "Valverde"
  ),
  
  # Población total según el Censo 2022 (ONE)
  poblacion = c(255362, 114445, 234744, 75669, 964301, 
    321142, 75672, 96670, 250684, 92788, 
    95600, 66760, 320663, 287284, 
    442756, 145438, 197535, 129891, 
    205280, 41344, 220285, 364697, 122699, 
    676844, 65575, 259554, 351386, 
    175341, 1014661, 69695, 3013022, 
    197126
  )
)

#-----------------------------------------------------------------------------------
# Paso 2: Exportar esta tabla a un archivo CSV (Tu "CSV Crudo")
# Guardar la tabla como un archivo CSV en tu carpeta de datos
# (Asegúrate de tener la carpeta "datose_epi" creada en tu proyecto)

write_csv(poblacion_RD_censo_2022, "C:/Users/Gregorio Rivas/Desktop/epidemiologia DIEPI/datos_epi/poblacion_RD_censo_2022.csv")
write.csv(poblacion, "C:/Users/Gregorio Rivas/Desktop/epidemiologia DIEPI/datos_epi/poblacion_RD_censo_2022.csv", row.names = FALSE)

#--------------------------------------------------------------------------------------

# Verificar que se creó bien (debe tener 32 filas)
View(poblacion_RD_censo_2022)
print(poblacion_RD_censo_2022)
poblacion_RD_censo_2022

#---------------------------------------------------------------------------------

Paso 3: Integrarlo a tu script de Tasas

#----------------------------------------------------------------------------------


# 3. Limpieza de datos para crear el "linelist" (lista de casos) final
linelist <- linelist_crudo %>%
  mutate(
    # str_trim(): Elimina espacios en blanco al inicio o final del texto.
    # ¡CRÍTICO! "Santo Domingo " (con espacio) no es igual a "Santo Domingo" 
    # y rompería la unión con la tabla de población más adelante.
    provincia = str_trim(provincia), 
    
    # as.Date(): Convierte el texto de la fecha en un objeto "Date" real de R.
    # Esto permite hacer operaciones temporales (como extraer la semana epi).
    # Ajusta el format = "%d/%m/%Y" si tus fechas vienen con barras.
    fecha_sintomas = as.Date(fecha_sintomas, format = "%d/%m/%Y")
  )






























# 4. Cargar la tabla de población real (ya creada en el Paso 1)
      # Si prefieres leerla desde el CSV que acabamos de crear, usa esto: (ahora mismo no)
poblacion_RD_censo_2022 <- read_csv("datos/poblacion_RD_censo_2022.csv")














# Serie 'casos_por_semana_provincia'(usando 'linelist' ya limpio)
sem_prov <- linelist %>% count(anio_iso, semana_epi, provincia, name = "casos")


# Agregar tasa acumulada por provincia
tasas <- sem_prov %>%
  group_by(provincia) %>% summarise(casos = sum(casos), .groups="drop") %>%
  left_join(poblacion, by = "provincia") %>%
  mutate(tasa_100k = 1e5 * casos / poblacion) %>%
  arrange(desc(tasa_100k))
tasas







# 2. Unir la población con tu tabla anterior 'casos_por_semana_provincia'
analisis_completo <- casos_por_semana_provincia %>%
  inner_join(poblacion_rd, by = "provincia") %>%
  # 3. Calcular la Tasa de Incidencia por cada 100,000 habitantes
  mutate(tasa_incidencia = (casos / poblacion) * 100000)


