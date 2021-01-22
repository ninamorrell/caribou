####################################################################################################################
###################################### Caribou Collar Data Animation ###############################################
####################################################################################################################

# September 24, 2020
# Written by Nina Morrell

####################################################################################################################

# clear workspace and set local directories
rm(list=ls())

dataDir <- paste0(getwd(), "\\Rdata\\")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,lubridate, ggplot2, tidyr, ggmap, gganimate)

# NOTE: data are proprietary, permissions granted on a case by case basis by the Government of Northwest Territories
# This example dataset is from 8 individuals in 2016 - 4 from each of the two herds
collar.dat <- readRDS(paste0(dataDir, "\\example_dat_2016.rds"))

## Format the variables we want for timeseries in gganimate
collar.dat %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y"),
         Herd_gender = paste(Herd, Gender, sep = ", "),
         ID_yr = paste0(ID, Year)) -> collar.dat

collars <- unique(collar.dat$ID_yr[which(collar.dat$Year==2016)])

collar.dat.spring.2016 <- collar.dat[grep(paste(collars,collapse="|"), collar.dat$ID_yr),] %>%
  filter(between(Month, 2, 6))

## define spatial window for figure
  min.N <- min(collar.dat.spring.2016$Latitude) +1# add 10th of a decimal degree for approx 10 km
  max.N <- max(collar.dat.spring.2016$Latitude) +.5
  
  min.E <- min(collar.dat.spring.2016$Longitude) - 1
  max.E <- max(collar.dat.spring.2016$Longitude) + 1

#create basemap using stamenmaps
bg <- ggmap(get_stamenmap(bbox=c(min.E, min.N, max.E, max.N), zoom = 8, maptype = "terrain"))

#create static plot - will look like an absolute mess, don't worry
map <- bg +
  geom_line(aes(x = Longitude, y = Latitude, colour=Herd_gender, group=ID),
            data = collar.dat.spring.2016,
            alpha = .3, size = 1) +
  scale_colour_manual(values = c("#080472", "#446F8E", "#B83D0B", "#F09811"))+
  theme(legend.title = element_blank(), legend.text=element_text(size=12), plot.title = element_text(size = 18, hjust = 0.5, face = "bold"))

#animate static plot
anim <- map + 
  geom_point(aes(x = Longitude, y = Latitude, colour=Herd_gender, group=ID),
             data = collar.dat.spring.2016, # add points in so they can be rendered at the front of the line
             alpha = .7, size = 3) +
  labs(title = paste("Date:{frame_along}")) +
  transition_reveal(Date)

#render animation
gganimate::animate(anim, width = 700, height = 500)

#can play with frames per second and level of detail but this can really slow it down
#gganimate::animate(anim, detail = 4, fps  =  5,  width = 700, height = 500)
