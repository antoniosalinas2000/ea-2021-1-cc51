library(dplyr)

#TABLA PROCESADA
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Tabla_procesada <- read.csv("Data/hotel_bookings_miss_processed.csv", header = TRUE, stringsAsFactors = FALSE)
View(Tabla_procesada)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#TABLA ORIGINAL
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Tabla_original <- read.csv("Data/hotel_bookings_miss.csv", header = TRUE, stringsAsFactors = FALSE)
View(Tabla_original)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#COMPARACION
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# VERIFICAR NA
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
verificar_NA <- function(df, col){
  print(col)
  var <- is.na(df[,c(col)])
  print(table(var))
}

verificar_NA(Tabla_Original, "adults")
verificar_NA(Tabla_procesada, "adults")

verificar_NA(Tabla_Original, "babies")
verificar_NA(Tabla_procesada, "babies")

verificar_NA(Tabla_Original, "children")
verificar_NA(Tabla_procesada, "children")

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# VERFICAR DATOS ATÍPICOS
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
verificar_outliers <- function(df, df_names){
  for(variable in df_names) {
    outliers.values <- boxplot(df[,c(variable)], main = paste(variable,"con Outliers"))$out
    outliers.values 
  }
}
par(mfrow = c(1,2))

verificar_outliers(Tabla_original, c("stays_in_weekend_nights"))
boxplot((Tabla_procesada$stays_in_weekend_nights), main = "stays_in_weekend_nights sin outliers")

verificar_outliers(Tabla_original, c("stays_in_week_nights"))
boxplot((Tabla_procesada$stays_in_week_nights), main = "stays_in_week_nights sin outliers")
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
