install.packages("patchwork")
install.packages("dplyr")

library(readxl)
data <- read_excel("C:/Users/ivank/Downloads/LocationData-GNSS-(20230201-20230926).xlsx")
View(LocationData_GNSS_20230201_20230926_)

library(readxl)
ODBA1<- read_excel("C:/Users/ivank/Downloads/ODBA-(20230201-20230926).xlsx")
View(ODBA_20230201_20230926_)


ODBA1$datetime <- as.POSIXct(strptime(ODBA1$`Collecting time`, tz = "UTC", "%Y-%m-%dT%H:%M:%OSZ"))
Location$datetime <- as.POSIXct(strptime(Location$`Collecting time`, tz = "UTC", "%Y-%m-%dT%H:%M:%OSZ"))

str(ODBA1$`Collecting time`)
summary(ODBA1$`Collecting time`)


library(dplyr)


str(Location$`Collecting time`)
summary (Location$`Collecting time`)

library(ggplot2); theme_set(theme_bw() +
                              theme(axis.title.y = element_text(size = rel(1.0), angle = 90)) +
                              theme(axis.title.y = element_text(size = rel(1.5), angle = 90))  +
                              theme(axis.text.y = element_text(size=15)) +
                              theme(axis.title.x = element_text(size = rel(1.5), angle = 0)) +
                              theme(axis.text.x = element_text(size=15)) +
                              theme(legend.position="none") +
                              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                    panel.background = element_blank(), axis.line = element_line(colour = "black")) +
                              theme(axis.line.x = element_line(color="black"),scale_linewidth(1),
                                    axis.line.y = element_line(color="black"),scale_linewidth(1)))


library(patchwork)

start <- "2023-07-08"
end <- "2023-07-16"

F_ODBA <- ggplot()+
  geom_line(data=ODBA1, aes(x=datetime,y=ODBA))+
  geom_point(data=ODBA1[ODBA1$ODBA<400,], aes(x=datetime,y=ODBA),colour="red",size=0.5)+
  scale_x_datetime(limits = c(as.POSIXct(start),as.POSIXct(end)))+
  facet_grid(UUID~.)

print(F_ODBA)

F_lon <-ggplot()+
  geom_line(data=Location, aes(x=datetime,y=Longitude))+
  scale_x_datetime(limits = c(as.POSIXct(start),as.POSIXct(end)))+
  facet_grid(.~UUID)

F_speed <-ggplot()+
  geom_line(data=Location, aes(x=datetime,y=Speed))+
  scale_x_datetime(limits = c(as.POSIXct(start),as.POSIXct(end)))+
  facet_grid(.~UUID)

F_ODBA

F_ODBA + F_lon