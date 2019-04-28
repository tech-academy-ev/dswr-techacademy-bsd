####### external Plots ########

library(magick)
TA_logo <- image_read("TA_logo.png")

ggplot(train[1:168,], aes(datetime, count)) +
  geom_line() +
  theme_light(base_size=12) +
  labs(title = "Demand Over a Seven-Day Period",
       subtitle = "Looks like there is some seasonality",
       x = "Date and Time",
       y = "Number of Bike Rentals",
       caption = "by Lukas Jürgensmeier") +
  theme(plot.title = element_text(color = "#3c4ee0", face = 'bold'))

train$hour  <- hour(ymd_hms(train$datetime))

train$workingday <- factor(train$workingday, levels = c("0","1"), labels = c("Weekend", "Weekday"))
train$hour <- as.factor(train$hour)


ggplot(train, aes(hour, count)) +
  geom_jitter(aes(color = day), shape = 1, width = 0.5, alpha = 0.5) +
  theme_light(base_size=12) +
  labs(title = "Demand by Hour and Weekday",
       subtitle = "Difference in days of the week",
       x = "Hour",
       y = "Number of Bike Rentals",
       caption = "by Lukas Jürgensmeier") +
  theme(plot.title = element_text(color = "#3c4ee0", face = 'bold')) +
  scale_colour_manual(name="Day", values= c("red", "black", "black", "black", "black", "black", "red"))

ggplot(train)+
  geom_boxplot(aes(y = count, group = day))

# add TechAcademy Logo
grid::grid.raster(TA_logo, x = 0.08, y = 0.02, just = c('left', 'bottom'), width = unit(1.5, 'inches'))


############## second/third data set ##############
monthly <- read.csv("BikeSharing_data_201902.csv")
head(monthly)
View(monthly)

monthly$Duration <- monthly$Duration/60

ggplot(monthly, aes(Duration)) +
  geom_density(aes(y=..density.., fill = Member.type), alpha = 0.6) +
  xlim(0, 60) +
  theme_light(base_size=12) +
  labs(title = 'Density Plot of Rental Duration',
       x = "Rental Duration in Minutes",
       subtitle = "There seems to be a structural difference between the two groups",
       caption = "by Lukas Jürgensmeier") +
  theme(plot.title = element_text(color = "#3c4ee0", face = 'bold')) +
  scale_fill_manual(name= "Membership Type", values = c("black", "#3c4ee0"))


stations <- read.csv("BikeSharing_data_stations.csv")
head(stations)                     
summary(stations)


#merge data sets by station number
# merged_test <- join(monthly, stations, by = "Station.number")
# alldata$market <- with(zipcodes, market[match(alldata$zip, zip)])
# monthly$lon_start <- with(stations, monthly$Start.station.number[match(monthly$Start.station.number, lon)])
# 
# monthly %>%
#   select(-Start.station.number) %>%
#   left_join(stations, by="Station.number")
# 
# merged_test <- left_join(monthly, stations, by =c("Start.station.number", "lon"))
library(dplyr)
monthly <- merge(monthly, stations, by.x ="Start.station.number", by.y = "Station.number")
monthly <- monthly %>% 
  rename(start.station.name = Station.name,
         start.lon = lon,
         start.lat = lat)

monthly <- merge(monthly, stations, by.x ="End.station.number", by.y = "Station.number")
monthly <- monthly %>% 
  rename(End.station.name = Station.name,
         End.lon = lon,
         End.lat = lat)


######### maps #########
library(mapsapi)
API <- "AIzaSyBgXHF94_ttuxJj8yho8atBKb6van9GhRw"
register_google(key = API)

library(ggmap)
map <- get_googlemap("Washington, DC", zoom = 13, maptype = "roadmap")

DC_map <- get_map(location = c(lon = mean(monthly$start.lon), lat = mean(monthly$start.lat)), zoom = 13,
                  maptype = "roadmap", scale = 2)

ggmap(DC_map) + 
  geom_point(data = monthly, aes(x = start.lon, y = start.lat, alpha = 0.8), color = "#3c4ee0", size = 1, shape = 3) +
  theme_light(base_size=12) +
  labs(title = 'Location of Rental Bike Stations',
       x = "Longitude",
       y = "Latitude",
       subtitle = "Stations seem to be clustered in the city center",
       caption = "by Lukas Jürgensmeier") +
  theme(plot.title = element_text(color = "#3c4ee0", face = 'bold'), legend.position = "none")

# plot with all start locations
ggmap(DC_map) +
  geom_point(data = monthly, aes(x = start.lon, y = start.lat, color = "red", alpha = 0.8), size = 1, shape = 3) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  stat_density2d(data = monthly,
                 aes(x = start.lon, y = start.lat, fill = ..level.., alpha = ..level..), size = 0.01,
                 bins = 16, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red")

#include density of stations
ggmap(DC_map) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  geom_density2d(data = monthly, aes(x = start.lon, y = start.lat), size = 0.3) +
  stat_density2d(data = monthly,
                 aes(x = start.lon, y = start.lat, fill = ..level.., alpha = ..level..), size = 0.01,
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0, 0.3), guide = FALSE)


####### stargazer ########
library(stargazer)
stargazer((monthly))
