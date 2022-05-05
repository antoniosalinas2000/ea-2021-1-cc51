
# ¿Está aumentando la demanda con el tiempo?

library("dplyr")
library("lubridate")

hotel_data <- read.csv("Data/hotel_bookings_miss_processed.csv")
m_x <- month(as.POSIXlt(hotel_data$reservation_status_date,format="%d/%m/%Y"))

hotel_data$mon <- m_x

hotel_data.grp <- hotel_data %>%group_by(mon) %>% summarise(n = n())

regresion <- lm(hotel_data.grp$n ~ hotel_data.grp$mon,col="red")

barplot(hotel_data.grp$n, names.arg=month.abb, main="Demanda en el tiempo")
abline(regresion)