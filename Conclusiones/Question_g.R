
# ¿En qué meses del año se producen más cancelaciones de reservas?

library("dplyr")
library("lubridate")

hotel_data <- read.csv("Data/hotel_bookings_miss_processed.csv")

hotel_data.sts <- hotel_data[hotel_data$is_canceled == 1,]
m_x <- month(as.POSIXlt(hotel_data.sts$reservation_status_date,format="%d/%m/%Y"))
hotel_data.sts$mon <- m_x

hotel_data.grp <- hotel_data.sts %>% group_by(mon) %>% summarise(n = n())
hotel_data.grp$monName <- month.abb[hotel_data.grp$mon]

# Obtener Mes con mayores cancelamientos
hotel_data.maximo <- max(hotel_data.grp$n)
hotel_data.r <- hotel_data.grp[hotel_data.grp$n == hotel_data.maximo, ]

colors <- hotel_data.grp$n == hotel_data.maximo
colors <- ifelse(colors, "red", "white")

barplot(hotel_data.grp$n, names.arg=hotel_data.grp$monName, col=colors, main="Mes del año en el que se producen más cancelaciones de reservas")