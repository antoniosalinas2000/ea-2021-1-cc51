
# ¿Cuántas reservas se realizan por tipo de hotel? o ¿Qué tipo de hotel prefiere la gente?

library(dplyr)

hotel_data <- read.csv("Data/hotel_bookings_miss_processed.csv")
hotel_table <- table(hotel_data$ï..hotel)

hotel <- barplot(hotel_table,main="Tipos de hotel")