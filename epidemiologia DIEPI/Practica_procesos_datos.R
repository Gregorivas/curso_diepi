
# ============================================
# ANÁLISIS DE CASOS DE LEPTOSPIROSIS (EJERCICIO)
# Autor: Gregorio Rivas
# Fecha: 2026-09-11
# ============================================

# NOTAS IMPORTANTES:   ### Ejecutar la línea actual: Ctrl + Enter/ selecciona y se presciona
                       ### Comentar o descomentar una línea: Ctrl + Shift + C
                       ### operador de asignación (<-): Alt + - (guion medio)
                       ### operador Pipe (%>%): Ctrl + Shift + M 
                       ### vectores utilizan rangos al crearse, (nombre_variable)
                       ### utilizando el formato "YYYY-MM-DD" (Año-Mes-Día), que es el formato ISO 8601
                       ### # ruta de los archivos de datos_epi"C:/Users/Gregorio Rivas/Desktop/epidemiologia DIEPI/datos_epi"


# INSTALACION DE PAQUETES

install.packages(c("tidyverse", "janitor", "lubridate", "stringr", "tidyr", "tibble", "readxl", 
                   "writexl"))

library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)
library(tibble)
library(readxl)

library(writexl)
# ruta de los archuvos de datos_epi"C:/Users/Gregorio Rivas/Desktop/epidemiologia DIEPI/datos_epi"
write.csv(datos_pacientes, file = "datos_epi.csv", row.names = FALSE)
write_xlsx(datos_pacientes_ext, path = "C:/Users/TuUsuario/Desktop/datos_pacientes_ext.xlsx")
write.csv(

# ============================================
# PASO 1:CREAR LOS OBJETOS PRINCIPALES PARA LAS TABLAS
# ============================================
## Declaracion de Variables: NOTA: cuando se declara variables no se puede colocar 
   # mayusculas, caractor o numeros delante de ella al escribirla.
   # siempre utilizar (_) (Nombre_Completo) cuando se trata separar dos o mas palabras 
   # RECORDAR QUE LAS VARIABLES DEPENDERA DEL TIPO DE DATOS 
   # rm(nombre_variable u pbjeto) # borra una variable mal creada o que no se requiera

# Nota: Usamos mayúsculas y guiones bajos a propósito para probar la limpieza después.

Edad_Paciente <- c("25", "40", "15", "25", "32", "55", "19", "28")

Fecha_Sintomas <-  c("2024-06-10", "2024-06-12", "2024-06-15", "2024-06-18", 
                     "2024-06-20", "2024-06-22", "2024-06-25", "2024-06-28"),

Sexo          <-  c("Masculino", "Femenino", "Masculino", "Femenino", 
                    "Masculino", "Femenino", "Masculino", "Femenino"),

Resultado_Lab <-  c("Positivo", "Negativo", "Positivo", "Positivo", 
                    "Negativo", "Positivo", "Positivo", "Negativo"),

Provincia     <-  c("Santo Domingo", "Santiago", "La Vega", "Puerto Plata", 
                    "Santo Domingo", "Santiago", "La Vega", "Puerto Plata")
)

#------

# tipo de carater de la tabla haste ese momeno y antes de las conversiones
class(Edad_Paciente)  # Arroja [1] "character"
class(Sexo)           # Arroja [1] "character"
class(Fecha_Sintomas) # Arroja [1] "character"
class(Resultado_Lab)  # Arroja [1] "character"
class(Provincia)      # Arroja [1] "character"

#-------------------------------------------------

# Visualizar la composcicion del archivo con la variable dada "Edad_Paciente o  una tabla de datos" inspeccionar y manipular la información:

head(Edad_Paciente)     # Ver las primeras filas
                        # [1] "25" "40" "15" "25" "32" "55"
str(Edad_Paciente)      # Ver la estructura y tipos de variables
                        # chr [1:8] "25" "40" "15" "25" "32" "55" "19" "28"
summary(Edad_Paciente)  # Resumen estadístico rápido, Usa el código con precaución.
                        # ______________________________________________                         
                        #    Length                N.unique                   N.blank                Min.nchar                    Max.nchar 
                        #      8                      7                             0                     2                          2 
                        # (Longitud)         (Valores únicos)               (Celdas en blanco)   (Número de caracteres mínimo)   (Número de caracteres máximo)
                        # número total de    valores distintos que existen  columnas vacías      longitud del texto más corto    longitud del texto más largo
                        # filas o registros. sin contar los repetidos.
                        # _______________________________________________

#=======================================================================================================================

# Vamos a Casting o Castinar, trasnformar o convertir un tipo dato a otro tipo dato

Edad_Paciente  <- c("25", "40", "15", "25", "32", "55", "19", "28"), # actual vatiable 

# Creación de un vector de edades de pacientes con el cambio de caracter a numero entero y Se reutiliza 
edad_paciente_ <- as.integer(c("25", "40", "15", "25", "32", "55", "19", "28")) 
class(edad_paciente_) # ahora se comvirtio en [1] "integer" Solo datos de números enteros
edad_paciente_        # manteniendo los mismos valores [1] 25 40 15 25 32 55 19 28

#-------------------------------------------------------------------------------------------------------

# Creación de un vector de Sintomas de pacientes con el cambio de caracter a DATOS y Se reutiliza
Fecha_Sintomas <-  c("2024-06-10", "2024-06-12", "2024-06-15", "2024-06-18", 
                     "2024-06-20", "2024-06-22", "2024-06-25", "2024-06-28")
class(Fecha_Sintomas)   # es [1] "character" Actual
print(Fecha_Sintomas)   # desde la fila [1] "2024-06-10" "2024-06-12" "2024-06-15" "2024-06-18" "2024-06-20" "2024-06-22"
                        # desde la fila [7] "2024-06-25" "2024-06-28"

          #  Creación de un Nuevo vector





print(fecha_sintomas_)   # desde la fila [1] "2024-06-10" "2024-06-12" "2024-06-15" "2024-06-18" "2024-06-20" "2024-06-22"
                         # desde la fila [7] "2024-06-25" "2024-06-28"
fecha_sintomas_ <- as.Date(Fecha_Sintomas, format = "%d-%m-%Y")

print(fecha_sintomas_)   # se cambia a otro carater [1] NA NA NA NA NA NA NA NA
class(fecha_sintomas_)   # ahora se cambia a [1] "Date" y es reutilizada

#----------------------------------------------------------------------------------


# Crear Categorías o Variables Cualitativas

Sexo          <-  c("Masculino", "Femenino", "Masculino", "Femenino", 
                    "Masculino", "Femenino", "Masculino", "Femenino"),

Resultado_Lab <-  c("Positivo", "Negativo", "Positivo", "Positivo", 
                    "Negativo", "Positivo", "Positivo", "Negativo"),

Provincia     <-  c("Santo Domingo", "Santiago", "La Vega", "Puerto Plata", 
                    "Santo Domingo", "Santiago", "La Vega", "Puerto Plata")

#-----------

# se crea nuevas variables para convertir o castinar las Variables o Categorías Cualitativas a (factor)

Sexo_          <- as.factor(Sexo)          
Sexo_                                      # [1] Masculino Femenino  Masculino Femenino  Masculino Femenino  Masculino
                                           # [8] Femenino 
                                           # Levels (Niveles): Femenino Masculino

Resultado_Lab_ <- as.factor(Resultado_Lab) 
Resultado_Lab_                             # [1] [1] Positivo Negativo Positivo Positivo Negativo Positivo Positivo Negativo
                                           # Levels (Niveles): Negativo Positivo (arroja 2 valores o niveles)

Provincia_     <- as.factor(Provincia)     
Provincia_                                 # [1] Santo Domingo Santiago      La Vega       Puerto Plata  Santo Domingo
                                           # [6] Santiago      La Vega       Puerto Plata 
                                           # Levels (Niveles): La Vega Puerto Plata Santiago Santo Domingo (arroja 4 valores o niveles)


# ============================================
# PASO 2: Crear una tabla o tibble con 8 casos,  
# ============================================
# Nota: Usamos mayúsculas y guiones bajos a propósito para probar la limpieza después.

datos_lepto <- tibble(
  ID_Caso <- 1:8,
  Edad_Paciente  <- c("25", "40", "15", "25", "32", "55", "19", "28"), # "NA" y "-" eran inválidos corregidos manualmente
  Sexo           <-  c("Masculino", "Femenino", "Masculino", "Femenino", 
                    "Masculino", "Femenino", "Masculino", "Femenino"),
  Fecha_Sintomas <-  c("2024-06-10", "2024-06-12", "2024-06-15", "2024-06-18", 
                     "2024-06-20", "2024-06-22", "2024-06-25", "2024-06-28"),
  Resultado_Lab  <-  c("Positivo", "Negativo", "Positivo", "Positivo", 
                    "Negativo", "Positivo", "Positivo", "Negativo"),
  Provincia     <-  c("Santo Domingo", "Santiago", "La Vega", "Puerto Plata", 
                    "Santo Domingo", "Santiago", "La Vega", "Puerto Plata")
)
#--------------------------------------------------------------------

# visualizando las variables y los datos de la tabla o los objetos creados

datos_lepto            # tabla completa (variables y valores)
Edad_Paciente          # se pide ver la Edad_Paciente [1] "25" "40" "15" "25" "32" "55" "19" "28"
Sexo                   # se pide ver el sexo
Fecha_Sintomas         # se pide ver Fecha_Sintomas
Resultado_Lab          # se pide ver Resultado_Lab
Provincia              # se pide ver Provincia

View(datos_lepto)         # Ver cómo se ve al principio
print(datos_lepto)        # Imprimir la tabla en la consola

#---------------------------------------------------------------------------

# ruta de los archuvos de datos_epi"C:/Users/Gregorio Rivas/Desktop/epidemiologia DIEPI/datos_epi"
write.csv(datos_pacientes, file = "datos_epi.csv", row.names = FALSE)
write_xlsx(datos_lepto, path = "C:/Users/Gregorio Rivas/Desktop/epidemiologia DIEPI/datos_epi")
write.csv(datos_lepto_ext, path = "C:/Users/Gregorio Rivas/Desktop/epidemiologia DIEPI/datos_epi")


write_xlsx(datos_lepto, path = "C:/Users/Gregorio Rivas/Desktop/epidemiologia DIEPI/datos_epi.xlsx")
# rm write_xlsx(datos_lepto, path = "C:/Users/Gregorio Rivas/Documents/datos_epi.xlsx")



# ============================================
# PASO 3: Normalizar nombres con janitor
# ============================================
# Esto convierte "Fecha_Sintomas" en "fecha_sintomas", etc.
datos_lepto <- datos_lepto %>% 
  clean_names()

# Verifica en el panel de Environment o con names(datos_lepto) que los nombres cambiaron.

# ============================================
# PASO 4: Convertir tipos de datos
# ============================================

datos_lepto <- datos_lepto %>%
  mutate(
    edad_paciente = parse_number(edad_paciente),              # Convertir edad a numérico. parse_number ignora los "-" y "NA" y los vuelve NA real.
    sexo = factor(sexo, levels = c("Masculino", "Femenino")), # Convertir sexo a factor con niveles específicos
    fecha_sintomas = ymd(fecha_sintomas)                      # Convertir fecha a formato Date (lubridate)
  )

class(edad_paciente)



# ============================================
# PASO 5: Crear semana epidemiológica y año ISO
# ============================================
datos_lepto <- datos_lepto %>%
  mutate(
    semana_epi <- epiweek(fecha_sintomas), # Número de la semana (1-53)
    anio_iso   <- isoyear(fecha_sintomas)  # Año ISO (a veces difiere del año calendario en enero/diciembre)
  )

print(datos_lepto)


# ============================================
# PASO 6: Cálculos y Resúmenes
# ============================================

# (a) Casos por semana_epi y provincia
casos_semana_prov <- datos_lepto %>%
  group_by(semana_epi, provincia) %>%
  summarise(
    n_casos = n(),
    .groups = "drop" # Buena práctica para evitar problemas después
  )
print("Tabla (a): Casos por semana y provincia")
print(casos_semana_prov)

# (b) Proporción de positivos
prop_positivos <- datos_lepto %>%
  summarise(
    total_casos = n(),
    total_positivos = sum(resultado_lab == "Positivo", na.rm = TRUE),
    proporcion_positivos = total_positivos / total_casos
  )
print("Tabla (b): Proporción de positivos")
print(prop_positivos)

# (c) Tabla cruzada: sexo x resultado (usando tabyl de janitor, ¡es mágica!)
tabla_sexo_res <- datos_lepto %>%
  tabyl(sexo, resultado_lab) %>%
  adorn_percentages("row") %>%  # Agrega porcentajes por fila
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()                    # Agrega los números (n) entre paréntesis

print("Tabla (c): Sexo x Resultado")
print(tabla_sexo_res)

# ============================================
# PASO 7: Reglas de Calidad de Datos
# ============================================

# Aquí aplicamos las 3 reglas para "auditar" nuestro dataset.

datos_lepto <- datos_lepto %>%
  mutate(
    check_edad_paciente = ifelse(edad_paciente >= 0 & edad_paciente <= 120, "OK", "Revisar Edad"),  # Regla 1:  
                                                                         # Edades plausibles (entre 0 y 120 años).
                                                                         # Si no, marca "Error".
    
    check_fecha_sintomas = ifelse(fecha_sintomas <= Sys.Date(), "OK", "Fecha Futura"), # Regla 2: 
                                                                              # Fechas no futuras. 
                                                                              # La fecha no puede ser mayor a hoy (Sys.Date()).
   
    check_sexo = ifelse(!is.na(sexo), "OK", "Sexo Faltante")                  # Regla 3: Sexo consistente. No debe estar vacío (NA).
  )

# corregir algun nombre o carater que influya en la salida correcta de los nombres de las variables

names(datos_lepto)  # Opción 1: Ver solo los nombres
str(datos_lepto)    # Opción 2: Ver la estructura completa (recomendado)
View(datos_lepto)   # Opción 3: Ver la tabla completa

# Ver el resumen de calidad visualizando variable por variabe 

print("Resumen de Calidad de Datos:")
print(table(datos_lepto$check_edad_paciente))   #Arrojo OK 8
print(table(datos_lepto$check_fecha_sintomas))
print(table(datos_lepto$check_sexo))
