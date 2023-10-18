library(sp)
library(sf)
library(tidyverse)
library(gganimate)
library(magick)
library(ggmap)
library(mapdata)
library(basemaps)
library(rstac)
library(raster)
library(RStoolbox)
library(ggsn)
library(ggrepel)

xmin = 134.026725505556
xmax = 137.45773037202082
ymin = -21
ymax = -18.44953772929013
lon = c(xmin, xmax)
lat = c(ymin, ymax)
start_date = '2023-09-01'
end_date = '2023-10-17'
max_features = 50000

url = paste0("https://hotspots.dea.ga.gov.au/geoserver/public/wfs?service=WFS&version=1.1.0&request=GetFeature&typeName=public:hotspots&outputFormat=application/json&CQL_FILTER=((sensor=%27AVHRR%27%20AND%20(product=%27SRSS%27))%20OR%20(sensor=%27MODIS%27%20AND%20(product=%27MOD14%27))%20OR%20(sensor=%27VIIRS%27%20AND%20(product=%27AFMOD%27%20OR%20product=%27EDR%27)))%20AND%20datetime%20%3E%20%27",start_date, "%27%20AND%20datetime%20%3C%20%27",end_date, "%27%20AND%20INTERSECTS(location,%20POLYGON((",ymin, "%20",xmin, ",%20",ymin, "%20",xmax, ",%20",ymax, "%20",xmax, ",%20",ymax, "%20",xmin, ",%20",ymin, "%20",xmin, ")))&maxFeatures=",max_features, "&startIndex=0&sortBy=sensor%20A")

a_fire <- st_read(url)

a_fire$start_date_dt <- as.POSIXct(a_fire$start_dt)
class(a_fire$start_date_dt)
b <- st_drop_geometry(a_fire) %>% dplyr::mutate(long = unlist(purrr::map(a_fire$geometry,1)),
                                         lat = unlist(purrr::map(a_fire$geometry,2)))

rgb <- projectRaster(stack('data/barkly_rgb.tif'), crs= '+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0')
ggRGB(rgb, r=1, g=2, b=3)

anim_a <- ggRGB(rgb, r=1, g=2, b=3) + geom_point(data=b, aes(x=long, y=lat), colour='orange') +
  transition_time(start_date_dt) + 
  geom_label_repel(aes(y= -19.71, x=135.82), nudge_x=0.3, nudge_y=0.3,label="Barkly Homestead",segment.colour='white') +
  geom_label_repel(aes(y= -19.647, x=134.19),  nudge_x=0.3, nudge_y=0.3, label="Tennant Creek", segment.colour='white') + 
  geom_label_repel(aes(y= -20.637, x=135.607),  nudge_x=0.3, nudge_y=0.3, label="Canteen Creek", segment.colour='white') +
  geom_label_repel(aes(y= -20.464, x=135.251),  nudge_x=-0.6, nudge_y=-0.4, label="Epenarra", segment.colour='white') +
  shadow_mark(colour='black', alpha=0.3) + labs(subtitle="{frame_time}") +
  ggsn::scalebar(x.min=xmin, x.max=xmax, y.min=ymin, y.max=ymax, dist = 50, dist_unit="km",
                 st.size=3, height=0.03, transform  = TRUE, model = 'WGS84') + coord_sf() +
  labs(x="Long", y="Lat", title = "Fire Spread") + theme_bw()

anim_a

#### Get weather data ####

# Load Tennant Ck files from BOM
Sep_TC <- read.csv("data/IDCJDW8045.202309.csv")
Oct_TC <- read.csv("data/IDCJDW8045.202310.csv")

TC_dat <- bind_rows(Sep_TC, Oct_TC)
names(TC_dat)
class(TC_dat$Date)
TC_dat$date_dt <- as.POSIXct(TC_dat$Date, format = "%d/%m/%Y")

# Produce min and max temp plot
anim_b <- ggplot(data=TC_dat) + geom_line(aes(x=date_dt, y=maxtemp), col="red") +
  transition_reveal(date_dt) + geom_line(aes(x=date_dt, y=mintemp), col="blue") +
  theme_bw() + labs(y="Temperature (C)", x="Date", title = "Daily Min & Max Temp.")

anim_b

### Wind rose ###

dir <- setNames( seq(0, 337.5 , by=22.5), 
                 c("N","NNE","NE", "ENE", "E", "ESE", "SE", "SSE", 
                   "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"))

nsew <- c("N","NNE","NE", "ENE", "E", "ESE", "SE", "SSE", 
         "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")

mtc3 <- match(TC_dat$wnddir3pm, nsew)
TC_dat$wnddirdeg3pm <- as.vector(dir[mt])


mtc9 <- match(TC_dat$wnddir9am, nsew)
TC_dat$wnddirdeg9am <- as.vector(dir[mt])


anim_c <- ggplot(data=TC_dat) + geom_point(aes(x=wnddirdeg9am, y=wndspd9am), size=10) + coord_polar() +
  transition_time(date_dt) + labs(subtitle="{frame_time}") + theme_bw() + 
  labs(y="Wind Speed (km/h)", x="Wind Direction (deg)", title="Wind Speed & Direction (9am)")

anim_c

### Combine animations ###
max(b$datetime)
max(TC_dat$date_dt)

a_gif <- gganimate::animate(anim_a, width = 460, height = 460)
b_gif <- gganimate::animate(anim_b, width = 230, height = 230)
c_gif <- gganimate::animate(anim_c, width = 230, height = 230)

a_mgif <- image_read(a_gif)
b_mgif <- image_read(b_gif)
c_mgif <- image_read(c_gif)

new_gif <- image_append(c(b_mgif[1], c_mgif[1]))
for(i in 2:80){
  combined <- image_append(c(b_mgif[i], c_mgif[i]))
  new_gif <- c(new_gif, combined)
}

new_new_gif <- image_append(c(a_mgif[1], new_gif[1]), stack=TRUE)
for(i in 2:80){
  combined <- image_append(c(a_mgif[i], new_gif[i]), stack=TRUE)
  new_new_gif <- c(new_new_gif, combined)
}

new_new_gif

image_write_gif(new_new_gif, 'fire2.gif', delay=1/5)
