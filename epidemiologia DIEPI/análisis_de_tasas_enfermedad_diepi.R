# ==============================================================================
# ANÁLISIS DE TASAS DE ENFERMEDAD POR PROVINCIA (RD - Censo 2022)
# Autor: Gregorio Rivas
# Fecha: 2026-09-15
# ==============================================================================

# 1. Cargar las librerías necesarias
install.packages(c("tidyverse", "janitor", "lubridate", "readxl"))
install.packages("readr")
install.packages("writexl")

library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)
library(tidyr);
library(writexl)
library(ggplot2)     # crear gráficos profesionales, permite diseñar desde barras simples y 
                     # diagramas de caja (boxplots) hasta curvas epidémicas complejas
                     # controlando de forma de los colores, los ejes y los títulos
library(tibble)      # sirve para crear tablas de datos modernas en R,
                     # columnas tengan tamaños compatibles y muestra resúmenes limpios

# ==============================================================================
# FASE 1: CREAR EL CSV CRUDO A PARTIR DEL EXCEL
# ==============================================================================

# Opción A: Si tienes el archivo Excel en tu carpeta de proyecto
# datos_crudos_excel <- read_excel("M03_Estructuras_Datos.xlsx")

# Opción B: Simulación directa con los datos que me compartiste (para que pruebes ya mismo)
datos_crudos_excel <- tibble(
  ID = paste0("REG-", sprintf("%04d", 1:100)),
  Provincia = c("DN","La Altagracia","Puerto Plata","Santo Domingo","La Vega",
                "La Altagracia","DN","San Cristóbal","Puerto Plata","Santo Domingo",
                "Puerto Plata","La Altagracia","Santo Domingo","Santiago","San Cristóbal",
                "Santiago","Santo Domingo","La Altagracia","Santiago","La Altagracia",
                "San Cristóbal","San Cristóbal","San Cristóbal","Puerto Plata","Santo Domingo",
                "Santo Domingo","Puerto Plata","Puerto Plata","Santo Domingo","Santiago",
                "Puerto Plata","La Vega","San Cristóbal","La Altagracia","San Cristóbal",
                "San Cristóbal","DN","La Vega","Santiago","San Cristóbal","Santiago",
                "La Altagracia","Santo Domingo","Puerto Plata","La Vega","Santo Domingo",
                "San Cristóbal","La Vega","La Vega","La Vega","Puerto Plata","La Altagracia",
                "San Cristóbal","Santiago","Santo Domingo","Santo Domingo","Puerto Plata",
                "Puerto Plata","San Cristóbal","La Vega","Santo Domingo","DN","San Cristóbal",
                "DN","DN","San Cristóbal","DN","Puerto Plata","San Cristóbal","La Altagracia",
                "Santo Domingo","Santiago","San Cristóbal","DN","DN","Santiago","Santiago",
                "Santiago","La Altagracia","Santiago","Santo Domingo","Santo Domingo","Santiago",
                "San Cristóbal","Santiago","La Vega","La Altagracia","Santo Domingo","DN",
                "Santiago","La Vega","La Altagracia","Santo Domingo","DN","Santiago","La Vega",
                "La Altagracia","DN","Puerto Plata","Puerto Plata"),
  Edad = c(33,87,49,70,31,0,55,8,60,39,13,36,73,25,80,34,74,65,82,12,11,45,20,1,50,7,31,40,58,2,20,39,58,
            64,57,89,33,21,49,37,48,52,4,54,75,57,3,30,55,67,83,85,22,0,38,60,26,87,52,13,26,2,84,17,49,69,
            26,15,52,39,68,7,66,20,64,10,47,56,21,83,14,47,7,54,18,59,57,6,36,46,9,55,12,90,52,75,57,11,57,45),
  Sexo = c("M","F","M","F","M","M","F","F","M","M","F","M","M","F","F","M","M","M","F","F","F","M","F","F",
           "M","M","F","M","M","M","F","F","F","M","F","F","M","F","F","F","M","M","F","F","M","F","F","F",
           "M","F","M","F","M","M","M","M","F","F","M","M","F","F","M","F","M","F","F","F","F","F","F","F",
           "F","F","F","M","M","M","F","M","M","F","F","F","F","F","M","F","M","M","M","F","M","F","F","F",
           "M","M","F","F"),
  Inicio_Sintomas =  c("2025-01-03","2025-06-22","2025-06-08","2025-06-08","2025-04-12","2025-05-16",
                        "2025-03-25","2025-01-04","2025-04-14","2025-04-23","2025-03-09","2025-03-24",
                        "2025-06-08","2025-01-07","2025-06-23","2025-02-11","2025-06-08","2025-04-07",
                        "2025-06-16","2025-05-22","2025-01-27","2025-04-04","2025-05-18","2025-04-27",
                        "2025-02-11","2025-02-26","2025-02-17","2025-01-15","2025-05-31","2025-03-04",
                        "2025-02-18","2025-03-02","2025-05-10","2025-04-14","2024-12-27","2025-04-14",
                        "2025-02-27","2025-04-20","2025-02-26","2025-02-13","2025-03-05","2025-03-21",
                        "2025-01-08","2025-03-27","2025-02-01","2025-04-21","2025-06-23","2025-05-06",
                        "2025-01-03","2025-01-04","2025-06-02","2025-05-25","2025-02-22","2025-03-30",
                        "2025-05-26","2025-06-12","2025-01-11","2025-01-22","2025-05-29","2025-05-15",
                        "2025-05-13","2025-05-04","2025-03-17","2025-06-11","2025-04-25","2025-01-12",
                        "2025-02-27","2025-02-19","2025-03-31","2025-05-01","2025-05-29","2025-01-15",
                        "2025-04-07","2025-04-02","2025-04-25","2025-04-16","2025-04-11","2025-01-12",
                        "2025-05-11","2025-03-26","2025-04-30","2025-02-12","2024-12-31","2025-03-02",
                        "2025-01-16","2025-03-29","2025-05-23","2025-02-08","2025-04-28","2025-01-10",
                        "2025-01-24","2025-03-23","2025-03-02","2025-05-20","2025-03-19","2025-05-29",
                        "2025-02-19","2025-03-02","2025-02-07","2025-01-14"),
  Resultado_Lab =  c("Positivo","Positivo","Positivo","Pendiente","Pendiente","Pendiente","Positivo","Negativo",
                    "Positivo","Pendiente","Positivo","Pendiente","Negativo","Pendiente","Pendiente","Pendiente",
                    "Negativo","Positivo","Negativo","Positivo","Pendiente","Negativo","Negativo","Negativo",
                    "Pendiente","Positivo","Pendiente","Positivo","Pendiente","Positivo","Pendiente","Negativo",
                    "Pendiente","Negativo","Positivo","Positivo","Pendiente","Positivo","Negativo","Positivo",
                    "Positivo","Pendiente","Pendiente","Positivo","Pendiente","Pendiente","Negativo","Negativo",
                    "Pendiente","Negativo","Pendiente","Pendiente","Pendiente","Negativo","Positivo","Positivo",
                    "Pendiente","Positivo","Positivo","Positivo","Pendiente","Positivo","Positivo","Negativo",
                    "Pendiente","Pendiente","Positivo","Negativo","Negativo","Positivo","Negativo","Positivo",
                    "Pendiente","Pendiente","Negativo","Negativo","Negativo","Negativo","Pendiente","Pendiente",
                    "Pendiente","Pendiente","Negativo","Positivo","Pendiente","Negativo","Pendiente","Pendiente",
                    "Positivo","Positivo","Pendiente","Negativo","Pendiente","Positivo","Pendiente","Pendiente",
                    "Pendiente","Pendiente","Pendiente","Negativo"),
  Diagnostico =  c("Dengue","Leptospirosis","Dengue","IRA Grave","EDA","IRA Grave","EDA","IRA Grave","IRA Grave",
                  "IRA Grave","Influenza","COVID-19","EDA","EDA","IRA Grave","Leptospirosis","Leptospirosis",
                  "Leptospirosis","IRA Grave","Influenza","IRA Grave","Chikungunya","Dengue","Dengue","EDA",
                  "IRA Grave","IRA Grave","Leptospirosis","COVID-19","COVID-19","EDA","EDA","COVID-19","EDA",
                  "EDA","EDA","Influenza","COVID-19","Leptospirosis","Dengue","EDA","COVID-19","IRA Grave",
                  "COVID-19","COVID-19","COVID-19","EDA","EDA","Dengue","COVID-19","COVID-19","Dengue","Dengue",
                  "Leptospirosis","Chikungunya","EDA","Chikungunya","IRA Grave","Dengue","Influenza","IRA Grave",
                  "COVID-19","IRA Grave","Dengue","EDA","IRA Grave","IRA Grave","COVID-19","IRA Grave","EDA",
                  "Dengue","IRA Grave","Dengue","Influenza","Chikungunya","Influenza","Dengue","COVID-19","Dengue",
                  "Influenza","Dengue","Chikungunya","Leptospirosis","IRA Grave","COVID-19","COVID-19","IRA Grave",
                  "Leptospirosis","Leptospirosis","COVID-19","COVID-19","IRA Grave","COVID-19","COVID-19","COVID-19",
                  "Malaria","EDA","Dengue","Dengue","Chikungunya")
)

# Exportar como CSV crudo (simulando el archivo)
write.csv(datos_crudos_excel, "C:/Users/Gregorio Rivas/Desktop/epidemiologia DIEPI/datos_epi/datos_crudos_excel.csv", row.names = FALSE)
message("✅ CSV crudo creado exitosamente en la carpeta 'datos/_epi'")

# Esto abrirá una ventana flotante para buscar tu archivo con el mouse
datos_crudos_excel <- read_csv(file.choose())



length(Provincia)               # verificar cuántos elementos tiene tu vector de provincias
nrow(datos_crudos_excel)        # verificar cuántas filas tiene tu tabla base
Provincia_corregida <- Provincia[1:100] # Ajustar el vector de provincias exactamente el tamaño de la tabla



# ==============================================================================
# FASE 2: LEER EL CSV CRUDO Y LIMPIAR (CREAR EL LINELIST)
# ==============================================================================

# Leer el CSV que acabamos de crear (así simulamos el flujo de trabajo real)
linelist_crudo <- read_csv("C:/Users/Gregorio Rivas/Desktop/epidemiologia DIEPI/datos_epi/datos_crudos_excel.csv")
linelist_crudo <- read_csv2("C:/Users/Gregorio Rivas/Desktop/epidemiologia DIEPI/datos_epi/datos_crudos_excel.csv")


print(linelist_crudo)
View(linelist_crudo)
View(datos_crudos_excel)  
print(datos_crudos_excel) 


# Limpieza y estandarización
linelist <- linelist_crudo %>%
  mutate(
    # 1. Renombrar y seleccionar columnas clave (id, Edad, sexo, fecha_sintomas, resultado, provincia)
    id = ID,
    edad = Edad,
    sexo = ifelse(Sexo == "M", "Masculino", "Femenino"),
    fecha_sintomas = ymd(Inicio_Sintomas),  # Convertir a fecha real
    resultado = Resultado_Lab,
    provincia_raw = Provincia  # Guardamos la original por si acaso
    
  ) %>%
  # 2. ¡CRÍTICO! Mapear "DN" a "Distrito Nacional" para que coincida con el Censo
  mutate(
    provincia = case_when(
      provincia_raw == "DN" ~ "Distrito Nacional",
      TRUE ~ provincia_raw  # Mantener el resto igual
    )
  ) %>%
  # 3. Crear variables de tiempo para análisis epidemiológico
  mutate(
    semana_epi = epiweek(fecha_sintomas),
    anio_iso = isoyear(fecha_sintomas)
  ) %>%
  # 4. Seleccionar solo las columnas que necesitamos (formato linelist limpio)
  select(id, edad, sexo, fecha_sintomas, resultado, provincia, semana_epi, anio_iso)

# colnames(linelist_crudo) # VEr las columnas principales 


# Verificar que la limpieza funcionó
print("📋 Primeras filas del linelist limpio:")
print(head(linelist, 5))

# ==============================================================================
# FASE 3: DEFINIR DENOMINADORES (POBLACIÓN CENSO 2022)
# ==============================================================================

poblacion_RD_censo_2022 <- tibble(
  provincia = c(
    "Azua", "Baoruco", "Barahona", "Dajabón", "Distrito Nacional", 
    "Duarte", "Elías Piña", "El Seibo", "Espaillat", "Hato Mayor", 
    "Hermanas Mirabal", "Independencia", "La Altagracia", "La Romana", 
    "La Vega", "María Trinidad Sánchez", "Monseñor Nouel", "Monte Cristi", 
    "Monte Plata", "Pedernales", "Peravia", "Puerto Plata", "Samaná", 
    "San Cristóbal", "San José de Ocoa", "San Juan", "San Pedro de Macorís", 
    "Sánchez Ramírez", "Santiago", "Santiago Rodríguez", "Santo Domingo", 
    "Valverde"
  ),
  poblacion = c(
    255362, 114445, 234744, 75669, 964301, 
    321142, 75672, 96670, 250684, 92788, 
    95600, 66760, 320663, 287284, 
    442756, 145438, 197535, 129891, 
    205280, 41344, 220285, 364697, 122699, 
    676844, 65575, 259554, 351386, 
    175341, 1014661, 69695, 3013022, 
    197126
  )
)

# ==============================================================================
# FASE 4: CALCULAR TASAS POR 100,000 HABITANTES
# ==============================================================================

# Serie semanal por provincia (opcional, para análisis temporal)
sem_prov <- linelist %>% 
  count(anio_iso, semana_epi, provincia, name = "casos")

# Tasa acumulada por provincia (el objetivo principal)
tasas <- linelist %>%
  group_by(provincia) %>% 
  summarise(casos = n(), .groups = "drop") %>%  # Contar casos totales por provincia
  left_join(poblacion_RD_censo_2022, by = "provincia") %>%  # Unir con población
  mutate(
    tasa_100k = 1e5 * casos / poblacion  # Fórmula: (casos / población) * 100,000
  ) %>%
  arrange(desc(tasa_100k))  # Ordenar de mayor a menor tasa

# ==============================================================================
# FASE 5: VISUALIZAR RESULTADOS
# ==============================================================================

print(" TABLA DE TASAS POR 100,000 HABITANTES POR PROVINCIA:")
print(tasas)

# Gráfico de barras para visualizar las tasas
ggplot(tasas %>% filter(!is.na(tasa_100k)), aes(x = reorder(provincia, tasa_100k), y = tasa_100k)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +  # Barras horizontales para leer mejor los nombres
  labs(
    title = "Tasa de Casos por 100,000 Habitantes por Provincia (RD)",
    subtitle = "Datos simulados basados en M03_Estructuras_Datos.xlsx",
    x = "Provincia",
    y = "Tasa por 100,000 habitantes"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))