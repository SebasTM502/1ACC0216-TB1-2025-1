rm(list=ls(all=TRUE))
graphics.off()
cat("\014")

# Librerias Utilizadas en el proyecto

library(ggplot2)     # para visualización de datos
library(cowplot)     # para mejorar temas y combinar gráficos
library(VIM)         # muestra la tabla de valores faltantes
library(mlr)         # para imputar valores (como la moda)
library(dplyr)       # para manipulación de datos
library(lubridate)   # para manejo de fechas


# -----   CARGA DE DATOS.   -----

setwd("~/Desktop/1ACC0216-TB1-2025-1/data") #Directorio
df <- read.csv("hotel_bookings.csv", header=TRUE, stringsAsFactors = FALSE)

# ----- INSPECCION DE DATOS -----

head(df) #Encabezado del df

dim(df) #Dimension del df

names(df) # Nombre de las variables

summary(df) # Resumen estadistico

str(df) #Estructura de los datos

#Cambio de los tipos de datos
# Lista de columnas categóricas
categorical_cols <- c(
  "hotel", "meal", "country", "market_segment",
  "distribution_channel", "is_repeated_guest", "reserved_room_type", "assigned_room_type",
  "deposit_type", "agent", "company", "customer_type", "reservation_status"
)

# Convertir cada columna a factor
df[categorical_cols] <- lapply(df[categorical_cols], as.factor)
# Convetir a Date
df$reservation_status_date <- as.Date(df$reservation_status_date)

sapply(df,class) # Verificamos los cambios del tipo en la variables

str(df) # Verificamos la nueva estructura de los datos

## Eliminamos los duplicados

table(duplicated(df)) # Nos mostrara la cantidad de datos duplicados en TRUE

df<- unique(df) # Eliminamos duplicados

table(duplicated(df)) #Verificamos nuevamente los valores duplicados 
#Nota: Observamos que no muestra valores para TRUE, lo cual indica que ya no tenemos duplicados

# ----- PRE PROCESAR DATOS -----

#- Resumen
summary(df)
#- Identificacion de datos faltantes
#Verificamos los valores nulos por columnas
colSums(is.na(df)) 
#Observamos el contenido para un mayor analisis
View(df)

#Observacion obtenidas de summary(df) y View:
#Si bien con la funcion colSums(is.na(df)) solo muestra valores faltantes en children, 
#podemos verificar que los datos con NULL no fueron considerados como vacios o Na, por lo tanto
#Se tienen los siguientes resultados:

#4 valores Na en "children" | 0.005%
#12193 valores NULL en 'agent' | 13.95%
#82137 valores NULL en 'company' | 94.7%
#452 valores NULL en 'country' -> Dato obtenido de la table(df$country) | 0.52%

#Convertimos los valores NULL a vacios para un mejor manejo de los datos
df[] <- lapply(df, function(x) {
  if (is.character(x) || is.factor(x)) {
    x[x == "NULL"] <- NA
  }
  return(x)
})

#Verificamos nuevamente los valores nulos por columnas
colSums(is.na(df)) 

#Grafico de valores faltantes

aggr(df,numbers = T, sortVar=T)

#- Tratamiento de datos faltantes

# children
# Datos faltantes se impunaran con el valor entero de mean observado con summary(df)

df$children[is.na(df$children)] <- round(mean(df$children, na.rm = TRUE))

# Country

# Imputamos con la moda 
imputacion <- impute(df, 
                     cols = list(country = imputeMode()))

df <- imputacion$data

# Agent
# para el caso de Agent los valores Na lo cambiaremos por no No Agent
# despues de analizar esta columna categorica que es Id del agente externo que hizo la reserva
# podemos calificar el valor NULL como 'No Agent' al realizar sin agente la reserva 
# de tal manera que evitamos realizar un distorsion de la distrubucion de los resultados al aplicar la moda 
levels(df$agent) <- c(levels(df$agent), "No Agent")
df$agent[is.na(df$agent)] <- "No Agent"

# Company
# Para el caso de la columna Company se opto por eliminar la columna ya que
# el valor NULL representa el aprox 95% de todos los datos, ademas de que no utilizaremos
# esta variable para el analisis

df$company <- NULL

# Verificamos los valores nulos nuevamente
colSums(is.na(df)) 

# Resumen de las variables Categoricas, en cual observamos la cantidad y porcentaje por cada variable

cat_vars <- names(df)[sapply(df, function(x) class(x) %in% c("character", "factor"))]
for (var in cat_vars) {
  cat("\nResumen de:", var, "\n")
  print(table(df[[var]]))
  print(prop.table(table(df[[var]])))
}


#- Deteccion de Outliers:

#Visualizar outliers con boxplots
boxplot(df$lead_time, main = "Outliers en lead_time", horizontal = TRUE)
boxplot(df$adr, main = "Outliers en ADR", horizontal = TRUE)
boxplot(df$adults, main = "Outliers en adultos", horizontal = TRUE)

#- Tratamiento de Outliers

# Calculos para definir los cuartiles
identify_outliers <- function(x) {
  Q1 <- quantile(x, .25, na.rm=TRUE)
  Q3 <- quantile(x, .75, na.rm=TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5*IQR
  upper <- Q3 + 1.5*IQR
  x[x < lower | x > upper]
}

# Definimos la columnas a analizar
num_vars <- c("lead_time", "adr", "children", "babies", "adults")
for (v in num_vars) {
  cat("\n--- Variable:", v, "---\n")
  print(summary(df[[v]]))
  boxplot(df[[v]], main = paste("Boxplot de", v))
}

# Definimos y aplicamos el Winsorización
winsorize <- function(x, probs = c(0.01, 0.99)) {
  qs <- quantile(x, probs, na.rm = TRUE)
  x[x < qs[1]] <- qs[1]
  x[x > qs[2]] <- qs[2]
  x
}

for (col in num_vars) {
  df[[col]] <- winsorize(df[[col]], probs = c(0.01, 0.99))
}

# Resumen de los resultados
summary(df[, num_vars])
par(mfrow = c(2, 3))
for (v in num_vars) {
  boxplot(df[[v]], main = paste("Boxplot de", v))
}
par(mfrow = c(1, 1))

for (v in num_vars) {
  n_out <- length(identify_outliers(df[[v]]))
  cat("Outliers en", v, ":", n_out, "\n")
}

#- Creación de nuevas columnas

## Creamos una columna fecha que una los datos arrival_date_year,arrival_date_month y arrival_date_day_of_month en arrival_date

meses <- c("January", "February", "March", "April", "May", "June", 
           "July", "August", "September", "October", "November", "December")

# Convertir los meses de texto a números
df$arrival_date_month_num <- match(df$arrival_date_month, meses)

# Crear la nueva columna de fecha
df$arrival_date <- as.Date(paste(df$arrival_date_year, df$arrival_date_month_num, df$arrival_date_day_of_month, sep = "-"), "%Y-%m-%d")

# Eliminar la columna temporal de mes numérico
df$arrival_date_month_num <- NULL

# Verificar la nueva columna
head(df$arrival_date)

# Crear variable de estadía total 'stay total'
df$stay_total <- df$stays_in_weekend_nights + df$stays_in_week_nights

# Revision de los datos LIMPIOS

summary(df)

colSums(is.na(df)) 

sapply(df,class)

# Guardamos la data limpia 
write.csv(df, "hotel_bookings_limpio.csv", row.names = FALSE)


# ----- VISUALIZACION DE DATOS -----

cat('¿Cuántas reservas se realizan por tipo de hotel? ¿Qué tipo de hotel prefiere la gente?')


g1<- ggplot(df,aes(x= hotel ,fill = hotel)) +
  geom_bar()+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Cantidad de Reservas por Tipo de Hotel",
       x = "Tipo de hotel",
       y = "Cantidad de Reservas"
       )+ 
  theme_cowplot()
g1


cat('¿Está aumentando la demanda con el tiempo?')


df$reserva_fecha <- as.Date(paste(df$arrival_date_year, df$arrival_date_month, 1, sep = "-"), format = "%Y-%B-%d")

reservas_mensuales <- df %>%
  group_by(reserva_fecha = floor_date(reserva_fecha, "month")) %>%
  summarise(cantidad = n())

ggplot(reservas_mensuales, aes(x = reserva_fecha, y = cantidad)) +
  geom_line(color = "#33a02c") +
  labs(title = "Evolución de Reservas en el Tiempo", 
       x = "Fecha", 
       y = "Cantidad de Reservas") +
  theme_minimal()
                             





