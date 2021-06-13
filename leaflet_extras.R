# leaflet extras from Datacamp

library(leaflet.extras)
m2 <- leaflet() %>% 
  #addProviderTiles("Stamen.Watercolor") %>% 
  addProviderTiles("MapBox") %>%
  addSearchOSM() %>% 
  addReverseSearchOSM() %>% 
  setView(lat = 39.8282, lng = -98.5795, zoom=3)

m2 %>% addCircleMarkers(data=ipeds)
m2
pal <- colorFactor(palette = c("red", "blue", "#9b4a11"), 
                   levels = c("Public", "Private", "For-Profit"))

m2 %>% 
  addCircleMarkers(radius = 2, label = ipeds$name, color = pal(ipeds$sector))
