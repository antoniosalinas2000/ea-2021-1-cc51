
# ¿Cuántas reservas incluyen niños y/o bebes?

hotel_data <- read.csv("Data/hotel_bookings_miss_processed.csv")

hotel_data.babies <- hotel_data[hotel_data$babies>0,]
hotel_data.babies <- hotel_data.babies[is.na(hotel_data.babies$children) == 0,]

hotel_data.children <- hotel_data[hotel_data$children>0,]
hotel_data.children <- hotel_data.children[is.na(hotel_data.children$children) == 0,]

hotel_data.all <- hotel_data[(hotel_data$children==0)&(hotel_data$babies==0),]
hotel_data.all <- hotel_data.all[(is.na(hotel_data.all$children)|is.na(hotel_data.all$babies))==0,]

n_children <- nrow(hotel_data.children)
n_babies <- nrow(hotel_data.babies)
n_all <- nrow(hotel_data.all)

colors <- c("red", "yellow", "gray")
labels <- c("Childrens", "Babies", "Ninguno")
values <- c(n_children, n_babies, n_all)
etiquetas <- paste0(labels, " ", values)

pie(values, labels = etiquetas, col = colors, main="Reservas incluyen niños y/o bebes")