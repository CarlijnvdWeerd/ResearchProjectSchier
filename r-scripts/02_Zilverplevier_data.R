## Zilverplevier data van Thomas

renv::restore()

## Load the data
ODBA <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSXxOsWv70O0JwwAiW-9_AyqbHVUoEGR3oiwqI5Aydd8ff2MmdAVw5qzHgfMRYAJ_s0Ch2pYfMQ9D_T/pub?gid=368748468&single=true&output=csv")
data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSXxOsWv70O0JwwAiW-9_AyqbHVUoEGR3oiwqI5Aydd8ff2MmdAVw5qzHgfMRYAJ_s0Ch2pYfMQ9D_T/pub?gid=1460124996&single=true&output=csv")

ODBA$datetime <- as.POSIXct(strptime(ODBA$Collecting_time, tz = "UTC", "%Y-%m-%dT%H:%M:%OSZ"))
data$datetime <- as.POSIXct(strptime(data$Collecting_time, tz = "UTC", "%Y-%m-%dT%H:%M:%OSZ"))

ggplot2:: theme_set(theme_bw() +
            theme(axis.title.y = element_text(size = rel(1.0), angle = 90)) +
            theme(axis.title.y = element_text(size = rel(1.5), angle = 90))  +
            theme(axis.text.y = element_text(size=15)) +
            theme(axis.title.x = element_text(size = rel(1.5), angle = 0)) +
            theme(axis.text.x = element_text(size=15)) +
            theme(legend.position="none") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                  panel.background = element_blank(), axis.line = element_line(colour = "black")) +
            theme(axis.line.x = element_line(color="black", size = 1),
                  axis.line.y = element_line(color="black", size = 1)))

start <- "2023-07-08"
end <- "2023-07-16"

F_ODBA <- ggplot()+
  geom_line(data=ODBA, aes(x=datetime,y=ODBA))+
  geom_point(data=ODBA[ODBA$ODBA<400,], aes(x=datetime,y=ODBA),colour="red",size=0.5)+
  scale_x_datetime(limits = c(as.POSIXct(start),as.POSIXct(end)))+
  facet_grid(UUID~.)

F_lon <-ggplot()+
  geom_line(data=data, aes(x=datetime,y=Longitude))+
  scale_x_datetime(limits = c(as.POSIXct(start),as.POSIXct(end)))+
  facet_grid(.~UUID)

F_speed <-ggplot()+
  geom_line(data=data, aes(x=datetime,y=Speed))+
  scale_x_datetime(limits = c(as.POSIXct(start),as.POSIXct(end)))+
  facet_grid(.~UUID)

F_ODBA/F_lon/F_speed
F_total <- F_ODBA+F_lon+F_speed+patchwork::plot_layout(ncol=3)
F_total

## hiooo
