# A template for basic leaflet maps
# The first few chunks are POC only, so not prettified

# just the map
my_map <- leaflet() %>%
  addProviderTiles("Stamen.TerrainLabels") %>% 
  setView(-96, 37.8, 5)

# add spatial overlay of state borders
states <- readOGR("data/cb_2016_us_state_500k/cb_2016_us_state_500k.shp")

my_map <- my_map %>% 
  addPolygons(data = states)

# add markers
my_data <- 
my_map <- my_map %>% 
  addCircleMarkers(data=ipeds_clean, lat = ipeds_clean$lat, lng = ipeds_clean$lng)


# ------------------------------------------------------
# 1. group data
# 2. merge into spatial file
# 3. create bins
# 4. create palette
# 5. create map
# ------------------------------------------------------

# 1. group data
ipeds_states <- ipeds_clean %>% 
  group_by(state) %>% 
  summarise(total = n())

# order of state names in DATA dataframe HAS TO MATCH 
# order of state names in SPATIAL dataframe
aiyyo <- is.element(ipeds_states$state, states$NAME)
ipeds_states[!is.element(ipeds_states$state, states$NAME), ]
# well so Marshall Is, Northern Ma & Virgin Is are not in shape file, so remove them
ipeds_states <- subset(ipeds_states, aiyyo)
dingdong <- match(ipeds_states$state, states$NAME)
singsong <- order(dingdong)
ipeds_states <- ipeds_states[singsong, ]


# 2. merge this with spatial file based on state
states <- merge(states, ipeds_states, by.x = "NAME", by.y = "state")

# 3. create bins on range of variable being represented
bins <- seq(1:279)

# 4. create palette
pal <- colorBin("Greens", domain = ipeds_states$total, bins = bins)

# 5. create map
my_choro <- leaflet() %>%
  addProviderTiles("CartoDB") %>% 
  setView(-96, 37.8, 5) %>% 
  addPolygons(data = states,
              fillColor = pal(ipeds_states$total))
                         
my_choro

# --------------------------------------------------------
# now redo map above to be more palatable.

# 5. create map
# now make it more beautiful
my_choro <- leaflet() %>%
  addProviderTiles("Stamen.Toner") %>% 
  setView(-96, 37.8, 5) 

bins <- c(0, 25, 50, 100, 200, 300, Inf)
pal <- colorBin("Blues", domain = ipeds_states$total, bins = bins)
labels <- paste(ipeds_states$state, " ", ipeds_states$total)

my_choro %>% addPolygons(data = states,
              color = "#000000",
              weight = 1,
              dashArray = 3,
              fillColor = pal(ipeds_states$total),
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 2,
                color = "black",
                dashArray = "",
                fillOpacity = 1,
                bringToFront = TRUE),
              label = labels) %>% 
  addLegend(pal = pal, 
            values = ipeds_states$total, 
            opacity = 0.7, 
            title = NULL,
            position = "bottomright")


