library(dplyr)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#CARGAR DATOS
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Tabla_modificada <- read.csv("Data/hotel_bookings_miss.csv", header = TRUE, stringsAsFactors = FALSE)
Tabla_original <- read.csv("Data/hotel_bookings_miss.csv", header = TRUE, stringsAsFactors = FALSE)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#INSPECCIONAR DATOS
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
View(Tabla_original)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#PREPROCESAR DATOS
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#   VERIFICAR VALORES NA
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
verificar_NA <- function(df){
  for (variable in names(df)) {
    print(variable)
    var <- is.na(df[,c(variable)])
    print(table(var))
  }
}
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#   REEMPLAZAR VALORES NA x MEDIA POBLACIONAL
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Reemplazar_NA <- function(columna){
  colum <- Tabla_modificada[,c(columna)]
  Tabla_modificada[,c(columna)][colum == 0] <- NA
  temp.mean <- ifelse(is.na(colum), mean(colum, na.rm = TRUE), colum)
  temp.mean <- round(temp.mean, digits = 0)
  return(temp.mean)
}
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#   VER DATOS NA
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
verificar_NA(Tabla_modificada)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#   CAMBIAR NA
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#       stays_in_weekend_nights
#----------------------------------------------------------------------------------
Tabla_modificada$stays_in_weekend_nights.mean <- Reemplazar_NA("stays_in_weekend_nights")
Tabla_modificada$stays_in_weekend_nights <- NULL
names(Tabla_modificada)[32] <- "stays_in_weekend_nights"
#----------------------------------------------------------------------------------

#       lead_time
#----------------------------------------------------------------------------------
Tabla_modificada$lead_time.mean <- Reemplazar_NA("lead_time")
Tabla_modificada$lead_time <- NULL
names(Tabla_modificada)[32] <- "lead_time"
#----------------------------------------------------------------------------------

#       arrival_date_year
#----------------------------------------------------------------------------------
Tabla_modificada$arrival_date_year.mean <- Reemplazar_NA("arrival_date_year")
Tabla_modificada$arrival_date_year <- NULL
names(Tabla_modificada)[32] <- "arrival_date_year"
#----------------------------------------------------------------------------------

#       arrival_date_week_number
#----------------------------------------------------------------------------------
Tabla_modificada$arrival_date_week_number.mean <- Reemplazar_NA("arrival_date_week_number")
Tabla_modificada$arrival_date_week_number <- NULL
names(Tabla_modificada)[32] <- "arrival_date_week_number"
#----------------------------------------------------------------------------------

#       stays_in_week_nights
#----------------------------------------------------------------------------------
Tabla_modificada$stays_in_week_nights.mean <- Reemplazar_NA("stays_in_week_nights")
Tabla_modificada$stays_in_week_nights <- NULL
names(Tabla_modificada)[32] <- "stays_in_week_nights"
#----------------------------------------------------------------------------------

#       adults
#----------------------------------------------------------------------------------
Tabla_modificada$adults.mean <- Reemplazar_NA("adults")
Tabla_modificada$adults <- NULL
names(Tabla_modificada)[32] <- "adults"
#----------------------------------------------------------------------------------

#       children
#----------------------------------------------------------------------------------
Tabla_modificada$children.mean <- Reemplazar_NA("children")
Tabla_modificada$children <- NULL
names(Tabla_modificada)[32] <- "children"
#----------------------------------------------------------------------------------

#       babies
#----------------------------------------------------------------------------------
Tabla_modificada$babies.mean <- Reemplazar_NA("babies")
Tabla_modificada$babies <- NULL
names(Tabla_modificada)[32] <- "babies"
#----------------------------------------------------------------------------------

#       days_in_waiting_list
#----------------------------------------------------------------------------------
Tabla_modificada$days_in_waiting_list.mean <- Reemplazar_NA("days_in_waiting_list")
Tabla_modificada$days_in_waiting_list <- NULL
names(Tabla_modificada)[32] <- "days_in_waiting_list"
#----------------------------------------------------------------------------------

#       arrival_date_day_of_month
#----------------------------------------------------------------------------------
Tabla_modificada$arrival_date_day_of_month.mean <- Reemplazar_NA("arrival_date_day_of_month")
Tabla_modificada$arrival_date_day_of_month <- NULL
names(Tabla_modificada)[32] <- "arrival_date_day_of_month"
#----------------------------------------------------------------------------------

#   VER DATOS NA
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
verificar_NA(Tabla_modificada)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#   VERIFICAMOS VALORES ATÍPICOS
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
verificar_outliers <- function(df, df_names){
  for(variable in df_names) {
    outliers.values <- boxplot(df[,c(variable)], main = paste(variable,"con Outliers"))$out
    outliers.values 
  }
}
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#   MODIFICAMOS VALORES ATÍPICOS
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Corregir_outliers <- function(x, removeNA = TRUE){
  quantiles <- quantile(x, c(0.05, 0.95), na.rm = removeNA)
  x[x<quantiles[1]] <- mean(x, na.rm = removeNA)
  x[x>quantiles[2]] <- median(x, na.rm = removeNA)
  x
}
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#       adr
#----------------------------------------------------------------------------------
par(mfrow = c(1,2))
verificar_outliers(Tabla_modificada, c("adr"))
boxplot(Corregir_outliers(Tabla_modificada$adr), main = "adr sin Outliers")

Tabla_modificada$adr <- Corregir_outliers(Tabla_modificada$adr)
#----------------------------------------------------------------------------------

#       lead_time
#----------------------------------------------------------------------------------
par(mfrow = c(1,2))
verificar_outliers(Tabla_modificada, c("lead_time"))
boxplot(Corregir_outliers(Tabla_modificada$lead_time), main = "lead_time sin Outliers")

Tabla_modificada$lead_time <- Corregir_outliers(Tabla_modificada$lead_time)
#----------------------------------------------------------------------------------

#       stays_in_weekend_nights
#----------------------------------------------------------------------------------
par(mfrow = c(1,2))
verificar_outliers(Tabla_modificada, c("stays_in_weekend_nights"))
boxplot(Corregir_outliers(Tabla_modificada$stays_in_weekend_nights), main = "stays_in_weekend_nights sin Outliers")

Tabla_modificada$stays_in_weekend_nights <- Corregir_outliers(Tabla_modificada$stays_in_weekend_nights)
#----------------------------------------------------------------------------------

#       stays_in_week_nights
#----------------------------------------------------------------------------------
par(mfrow = c(1,2))
verificar_outliers(Tabla_modificada, c("stays_in_week_nights"))
boxplot(Corregir_outliers(Tabla_modificada$stays_in_week_nights), main = "stays_in_week_nights sin Outliers")

Tabla_modificada$stays_in_week_nights <- Corregir_outliers(Tabla_modificada$stays_in_week_nights)
#----------------------------------------------------------------------------------

#       adults
#----------------------------------------------------------------------------------
par(mfrow = c(1,2))
verificar_outliers(Tabla_modificada, c("adults"))
boxplot(Corregir_outliers(Tabla_modificada$adults), main = "adults sin Outliers")

Tabla_modificada$adults <- Corregir_outliers(Tabla_modificada$adults)
#----------------------------------------------------------------------------------

#       children
#----------------------------------------------------------------------------------
par(mfrow = c(1,2))
verificar_outliers(Tabla_modificada, c("children"))
boxplot(Corregir_outliers(Tabla_modificada$children), main = "children sin Outliers")

Tabla_modificada$children <- Corregir_outliers(Tabla_modificada$children)
#----------------------------------------------------------------------------------

#       babies
#----------------------------------------------------------------------------------
par(mfrow = c(1,2))
verificar_outliers(Tabla_modificada, c("babies"))
boxplot(Corregir_outliers(Tabla_modificada$babies), main = "babies sin Outliers")

Tabla_modificada$babies <- Corregir_outliers(Tabla_modificada$babies)
#----------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#GUARDAMOS DATOS PROCESADOS:
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
write.csv(Tabla_modificada,"Data/hotel_bookings_miss_processed.csv")
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------














