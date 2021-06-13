# leaflet_script.R

# Leaflet is used by FB, Pinterest & WaPo
# Open source library
# Must say quite nice

# Project overview of Udemy course
# 1. Overlay homicide data for Alaska & attach labels to data points
# 2. Summarize solve rates for various states with a choropleth
# 3. How to report maps using markdown
# 3i. embed static map in Word
# 3ii. embed dynamic version in HTML 
# 4. Create shiny app to enhance dynamic features of leaflet (gives us greater control over how the data is represented)

# load packages
library(leaflet)
library(rgdal) # to read shape files later
library(tidyverse) # for data mani later
library(ggmap) # to get lat lon via Google
library(tidygeocoder) # stupid ggmap too fussy!
library(htmltools) # for formatting multiline map labels later
library(RColorBrewer) # display.brewer.all() for palettes
# https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
library(htmlwidgets) # if you want to save html programatically

# create empty leaflet object
# will render gray because it needs additional layers to actually display
m <- leaflet()

# now add base map
# will render as world map
# can be zoomed in/out and panned
leaflet() %>% 
  addTiles() -> m
m

# now center it over Alaska
# coords for AK are 64.2008° N, 149.4937° W
# DONT FORGET zoom
leaflet() %>% 
  addTiles() %>% 
  setView(lng = -149.4937, lat = 64.2008, zoom = 4) -> m
m

# change the base map with a different tile provider
m %>% leaflet() %>% 
  addProviderTiles('CartoDB') %>% 
  setView(lng = -149.4937, lat = 64.2008, zoom = 4)

# code below is identical
m %>% leaflet() %>%
  addProviderTiles(providers$CartoDB) %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 4)

# some better looking maps. go to leaflet extras website to browse gallery
# 1 very Nat Geo
m %>% leaflet() %>%
  addProviderTiles('Esri.NatGeoWorldMap') %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 4)

# 2 Delicate Watercolor
m %>% leaflet() %>%
  addProviderTiles('Stamen.Watercolor') %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 4)

# 3 very batman -- can't see a thing
m %>% leaflet() %>%
  addProviderTiles('CartoDB.DarkMatter') %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 4)

# finally settle down on b/w map for project
m %>% leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 5)


# ---------------------------------------------------------------
#
#  OVERLAY COUNTIES WITH SHAPE FILES
# 
# ---------------------------------------------------------------

# ADD CONTEXT TO MAP WITH COUNTY LINES
# info contained in shape file
# shape files when unzipped contain other files too
# like projection (prj) files
# ignore them right now

# need rgdal to read shape files
# R Geospatial Data Abstraction Library

# load counties shape file
ak_counties <- readOGR('data/tl_2013_02_cousub/tl_2013_02_cousub.shp')

# add to map as polygons
# sometimes takes extra time
m %>% leaflet() %>%
  addProviderTiles('Stamen.Toner') %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 4) %>% 
  addPolygons(data = ak_counties)

# ugly & fat lines will now be classily svelte
# smoothFactor controls level of detail
# ideal is 1, less than 1 gives greater detail
# greater than 1 is faster to load, but can be silly
# since detail is lost(try smoothFactor = 5)
m %>% leaflet() %>%
  addProviderTiles('Stamen.Toner') %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 4) %>% 
  addPolygons(data = ak_counties,
              color = '#660000',
              weight = 1,
              smoothFactor = 1)


# ---------------------------------------------------------------
#
#  PREP HOMICIDE DATA
# 
# ---------------------------------------------------------------

# large dataset, but no latlon 
fbi_data <- read_csv('data/database.csv')

# how to find latlon?
# use ggmap. Input address & get coords using google
# limited to 1000 queries in 24 hours
# so use thoughtfully by prepping data first

names(fbi_data)

ak <- fbi_data %>% filter(State == 'Alaska')

# create an address variable to pass to ggmap
ak %>% mutate(address = paste(City, State, 'United States', sep = " ")) -> ak
view(ak)

# examine file
# lots of repeats in newly created ADDRESS variable
# so extract the unique address
ak_addresses <- ak %>% distinct(address)

# get the geocodes via tidygeocoder
# be patient, took a whole tense minute
ak_geocodes <- ak_addresses %>% geocode(address)

# but has 2 NAs
# I think ceate loop structure to isoate them? wierd...
# counter <- 0
# while (sum(is.na(ak_geocodes$lat)) > 0 && counter < 10) {
#   missing_addresses <- ak_geocodes %>% filter(is.na(lat)==TRUE)
#   ak_addresses <- missing_addresses
#   ak_geocodes <- ak_addresses %>% geocode(as.character(address))
#   
#   
#   
#   
# }
# can't understand this shit so will revert later



# ---------------------------------------------------------



# join ak geocodes to ak
ak <- left_join(ak, ak_geocodes, by = "address") 

# need unsolved cases of murder
ak_unsolved <- ak %>% filter(`Crime Solved` == 'No' & `Crime Type` == 'Murder or Manslaughter')

# last map created
m %>% leaflet() %>%
  addProviderTiles('Stamen.Toner') %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 4) %>% 
  addPolygons(data = ak_counties,
              color = '#660000',
              weight = 1,
              smoothFactor = 1) %>% 
  addCircleMarkers(lng = ak_unsolved$long, lat = ak_unsolved$lat)

# all good but only 11 datapoint for Anchorage
# because addresses werent granular enough in location info
# so introduce artiicial jitter (controlled by factor)
ak$lat <- jitter(ak$lat, factor = 1)
ak$long <- jitter(ak$long, factor = 1)

# run aain
ak_unsolved <- ak %>% filter(`Crime Solved` == 'No' & `Crime Type` == 'Murder or Manslaughter')

# repeat map again
m %>% leaflet() %>%
  addProviderTiles('Stamen.Toner') %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 4) %>% 
  addPolygons(data = ak_counties,
              color = '#660000',
              weight = 1,
              smoothFactor = 1) %>% 
  addCircleMarkers(lng = ak_unsolved$long, 
                   lat = ak_unsolved$lat,
                   color = 'ffffff',
                   weight = 1,
                   radius = 5)

# add label markers
# first just a baby step -- add year
# then nicely html formatted ramayan
# repeat map again
m %>% leaflet() %>%
  addProviderTiles('Stamen.Toner') %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 4) %>% 
  addPolygons(data = ak_counties,
              color = '#660000',
              weight = 1,
              smoothFactor = 1) %>% 
  addCircleMarkers(lng = ak_unsolved$long, 
                   lat = ak_unsolved$lat,
                   color = 'ffffff',
                   weight = 1,
                   radius = 5,
                   label = ak_unsolved$Year)

# if year isn't displayed, use label = as.character(ak_unsolved$Year)

# here's the ramayan
# but there's a problem
# the HTML code itself shows up! solution after this
ak_unsolved$label <- paste("<p>", ak_unsolved$City, "</p>",
                           "<p>", ak_unsolved$Month, " ", ak_unsolved$Year, "</p>",
                           "<p>", ak_unsolved$`Victim Age`, " ", ak_unsolved$`Victim Sex`, "</p>",
                           "<p>", ak_unsolved$`Victim Race`, "</p>",
                           "<p>", ak_unsolved$Weapon, " (UNSOLVED)", "</p>")

m %>% leaflet() %>%
  addProviderTiles('Stamen.Toner') %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 4) %>% 
  addPolygons(data = ak_counties,
              color = '#660000',
              weight = 1,
              smoothFactor = 1) %>% 
  addCircleMarkers(lng = ak_unsolved$long, 
                   lat = ak_unsolved$lat,
                   color = 'ffffff',
                   weight = 1,
                   radius = 5,
                   label = ak_unsolved$label)

# now apply html formatting
m %>% leaflet() %>%
  addProviderTiles('Stamen.Toner') %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 4) %>% 
  addPolygons(data = ak_counties,
              color = '#660000',
              weight = 1,
              smoothFactor = 1) %>% 
  addCircleMarkers(lng = ak_unsolved$long, 
                   lat = ak_unsolved$lat,
                   color = 'ffffff',
                   weight = 1,
                   radius = 5,
                   label = lapply(ak_unsolved$label, HTML))


# now apply cluster options (is this Ml clusterig?
m %>% leaflet() %>%
  addProviderTiles('CartoDB') %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 4) %>% 
  addPolygons(data = ak_counties,
              color = '#66eeee',
              weight = 1,
              smoothFactor = 1) %>% 
  addCircleMarkers(lng = ak_unsolved$long, 
                   lat = ak_unsolved$lat,
                   color = '#ff0000',
                   weight = 1,
                   radius = 5,
                   label = lapply(ak_unsolved$label, HTML),
                   clusterOptions = markerClusterOptions())

#------------------------------------------------------
# 1. create a df for "solved stuff" (add HTMLish label too)
# 2. layer on map with another addCircleMarkers (
# 2i. remove cluster pls
# 2ii  add group names
# 2iii made labels diff colors
# 3. add checkbox control for user control

ak_solved <- ak %>% filter(`Crime Solved` == 'Yes' & `Crime Type` == 'Murder or Manslaughter')

ak_solved$label <- paste("<p><b>", ak_solved$City, " ", ak_solved$Month, ", ", ak_solved$Year,"</b></br>",
                           ak_solved$`Victim Age`, " year old ", ak_solved$`Victim Race`, " ", ak_solved$`Victim Sex`, "</br>", "Weapon:", ak_solved$Weapon, " (SOLVED)", "</p>")


m %>% leaflet() %>%
  addProviderTiles('CartoDB') %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 4) %>% 
  addPolygons(data = ak_counties,
              color = '#66eeee',
              weight = 1,
              smoothFactor = 1) %>% 
  addCircleMarkers(lng = ak_unsolved$long, 
                   lat = ak_unsolved$lat,
                   color = '#ff0000',
                   weight = 1,
                   radius = 5,
                   group = 'Unsolved',
                   label = lapply(ak_unsolved$label, HTML)) %>% 
                   ##clusterOptions = markerClusterOptions())
  addCircleMarkers(lng = ak_solved$long, 
                   lat = ak_solved$lat,
                   color = '#00ff00',
                   weight = 1,
                   radius = 5,
                   group = 'Solved',
                   label = lapply(ak_solved$label, HTML)) %>% 
  addLayersControl(overlayGroups = c('Solved', 'Unsolved'),
                   options = layersControlOptions(collapsed = FALSE))


# ------------------------------------------------
# 
#               CHOROPLETHS
#
# ------------------------------------------------

# 1 subset for entire us
# 2 convert y/n from crime solved into 1/0

us <- fbi_data %>% 
  mutate(Solved = if_else(`Crime Solved`=="Yes", 1, 0)) %>% 
  filter(`Crime Type`=="Murder or Manslaughter") %>% 
  group_by(State) %>% 
  summarise('Total Murders' = n(),
            'Total Solved' = sum(Solved)) %>% 
  mutate('Total Unsolved' = `Total Murders` - `Total Solved`) %>% 
  mutate('Solve rate' = `Total Solved`/`Total Murders`) # PLEASE pay attention to backticks
# much confounded grief resolved when single quotes were replaced by backticks
# much much angst

# now read the shape file for US states
states <- readOGR("data/cb_2016_us_state_500k/cb_2016_us_state_500k.shp")

# now you don't know it o inexperienced one, but states (which is a SpatialPolygonsDataFrame)
# has several variables. 
names(states)
# [1] "STATEFP"  "STATENS"  "AFFGEOID" "GEOID"    "STUSPS"   "NAME"     "LSAD"     "ALAND"    "AWATER"

# the actual state names (i.e. Alaska, Arkansas, California) are contained in states$NAME
# what exactly are they? all 51? more than 51?
states$NAME
# [1] "Alabama"                                      "Alaska"                                      
# [3] "Arizona"                                      "Arkansas"                                    
# [5] "California"                                   "Colorado"                                    
# [7] "Connecticut"                                  "Delaware"                                    
# [9] "District of Columbia"                         "Georgia"
# and so on....

# so now, is us$State and states$NAME bhai-bhai?
# this function below questions if part 1 is found in part 2
# aka elements of 1st object are present in 2nd object
# and give T?F based on that.
is.element(us$State, states$NAME)
# [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [20]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [39]  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE

# I see a false, I see afalse!!!!! (and I read that as faasle)
# turns out to be "Rhodes Island"
us$State[40] <- "Rhode Island"

# now check if vice-versa is true
is.element(states$NAME, us$State)
# [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [20]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
# [39]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE

# aiyyo lots of false 

aiyyo <- is.element(states$NAME, us$State)
states$NAME[!aiyyo]
# [1] "American Samoa"                               "Puerto Rico"                                 
# [3] "Guam"                                         "Commonwealth of the Northern Mariana Islands"
# [5] "United States Virgin Islands"

# remove them from the dataset
# regular dplyr::filter no work on SpatialPolygonsDataFrame
# states1 <- states %>% filter(NAME != aiyyo)
states <- subset(states, aiyyo)

# now kindly reorder the rows to match the names order in the shape file.
dingdong <- match(us$State, states$NAME)
# [1]  1  2  3  4  5  6  7  8  9 33 10 11 12 13 14 15 16 34 35 36 17 37 38 18 19 39 20 40 21 41 22 23 42 43 24 44 25 45
# [39] 26 46 27 28 47 48 29 30 49 50 31 51 32

# this means the 10th element of us$State appears as the 33rd element of states$NAME
# just saying it's FLORIDA

# so order us$State by that
singsong <- order(dingdong)
us <- us[singsong, ]

# a grownup version is below
# us <- us[order(match(us$State, states$NAME)), ]

# --------------------------------------------------------------
#  the hardest is behind us
# --------------------------------------------------------------
bins <- seq(0.3:1.0, by = 0.1)
# bins <- c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
bins
# [1] 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0

pal <- colorBin("RdYlBu", domain = us$`Solve rate`, bins = bins)

labels <- paste("<p><b>", us$State, "</b></br>",
                "Solve rate: ", round(us$`Solve rate`*100, digits = 1), "%")
# Stamen.Toner
m %>% leaflet() %>% 
  setView(-96, 37.8, 4) %>% 
  addProviderTiles("CartoDB") %>% 
  addPolygons(data = states,
              weight = 1,
              smoothFactor = 0.5,
              color = "white",
              fillOpacity = 0.8,
              fillColor = pal(us$`Solve rate`),
              label = lapply(labels, HTML)
              # highlight = highlightOptions(
              #   weight = 5,
              #   color = "#666666",
              #   dashArray = "",
              #   fillOpacity = 0.7,
              #   bringToFront = TRUE
              # )
              ) %>% 
  addLegend(pal = pal,
            values = us$`Solve rate`*100,
            opacity = 0.7,
            position = "topright")

m








<- m %>% addTiles() %>% setView(lng = -121.9500, lat = 37.2872, zoom=12)
m

# use another provider
n <- leaflet() %>% addProviderTiles("Esri.NatGeoWorldMap") %>% setView(lng = -121.9500, lat = 37.2872, zoom=12)
n

# alternative format
n <- leaflet() %>% addTiles(provider="CartoDB") %>% setView(lng = -121.9500, lat = 37.2872, zoom=12)
n

# use another provider
n <- leaflet() %>% addProviderTiles("CartoDB.DarkMatter") %>% setView(lng = -121.9500, lat = 37.2872, zoom=12)
n

# Print the list of 100+ provider tiles that are included in the leaflet package.
providers

# To make this output more readable, print just the names of the provider tiles using the names() function.
names(providers)

# Use the str_detect() function from the stringr package to determine which provider tiles include the string "CartoDB".
str_detect(providers, "CartoDB")

# Print the name of every provider map tile that includes the string "CartoDB".
names(providers[str_detect(providers, "CartoDB")])

names(providers[str_detect(providers, "Esri")])


# geocoding in R
library(ggmap)

geocode("1234 Main St, Campbell, CA 95008")

geocode(location,
        output = c('latlon', 'latlona', 'more', 'all'),
        source = c('google', 'dsk'))
geocode("Colby College", output = "more", source = "google")

# default map view
# setView
# fitBounds()
n <- leaflet() %>% addProviderTiles("Esri.NatGeoWorldMap") %>% fitBounds(lng1 = -121.9500, lat1 = 37.2872, lng2 = -122.0839, lat2 = 37.3861)
n

# staying focused and taking away too much user freedom

# 1 take control of zoom levels. and prevent dragging
n <- leaflet(options = leafletOptions(dragging = FALSE,
                                 minZoom = 14,
                                 maxZoom = 18)) %>% 
  addProviderTiles("CartoDB") %>% 
  setView(lng = -121.9500, lat = 37.2872, zoom = 18)

# user can drag the map but can't stray too far
# set the map's max boundaries
n <- leaflet() %>% 
  addProviderTiles("CartoDB") %>% 
  setView(lng = -121.9500, lat = 37.2872, zoom = 18) %>% 
  setMaxBounds(lng1 = -122.4194, lat1 = 37.7749, lng2 = -121.5566, lat2 = 37.0030)

# limit max bounds to 0.05 decimal degrees from Taj Mahal
tmahal <- data.frame(lat = 27.1751, lon = 78.0421)

m <- leaflet() %>% 
  addProviderTiles("CartoDB") %>% 
  setView(lng = tmahal$lon, lat = tmahal$lat, zoom = 18)

m <- m %>% setMaxBounds(lng1 = tmahal$lon - 0.05, lat1 = tmahal$lat - 0.05, lng2 = tmahal$lon + 0.05, tmahal$lat + 0.05)

# plot a point
n <- leaflet() %>% addProviderTiles("Esri.NatGeoWorldMap") %>% 
  addMarkers(lng = -121.9500, lat = 37.2872)

# plot multiple points
msd <- data.frame(school = c('Baker', 'MMS', 'EDS', 'Latimer'), lat = c(37.285473267762406, 37.29038993098368, 37.30445527641242, 37.292199455313515), lon = c(-121.9858074, -121.98417661689318, -121.98572156936035, -121.97765348446596))

msdmap <- leaflet() %>% addProviderTiles("Esri.NatGeoWorldMap") %>% addMarkers(lng = msd$lon, lat = msd$lat, popup = msd$school)
  
# clean up base map
# restore view based on data  displayed on map

n <- leaflet() %>% addProviderTiles("Stamen.Watercolor") %>% setView(lng = -121.9500, lat = 37.2872, zoom=12)
n

n %>% clearBounds() %>% clearMarkers()
















  





















