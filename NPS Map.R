# US NP Map
# Original Direction From: https://liz-muehlmann.github.io/notes/cartography-part-three

#libraries
library("tidyverse")        # data manipulation & map creation
library("sf")               # loads shapefile
library("leaflet")          # creates the map
library("operator.tools")   # not-in function
library('tigris')

#states
states <- read_sf("/Users/minerva/Library/Mobile Documents/com~apple~CloudDocs/Git/Visited: NPS Map/usa.shp")
territories <- c("AS", "GU", "MP", "PR", "VI")

## load and process nps data
# https://public-nps.opendata.arcgis.com/datasets/nps::nps-boundary-4/explore
nps2 <- read_sf("/Users/minerva/Library/Mobile Documents/com~apple~CloudDocs/Git/Visited: NPS Map/nps_boundary.shp")  %>% 
  select(STATE, PARKNAME, AreaID, geometry) %>% 
  filter(STATE %!in% territories) %>%  
  mutate(visited = case_when(PARKNAME == "Hot Springs" ~ "#eb0c99", 
                             PARKNAME == "Redwood" ~ "#eb0c99", 
                             PARKNAME == "Rocky Mountain" ~ "#eb0c99", 
                             PARKNAME == "Hawaii Volcanoes" ~ "#eb0c99", 
                             PARKNAME == "Glacier" ~ "#eb0c99",
                             PARKNAME == "Badlands" ~ "#eb0c99",
                             PARKNAME == "Wind Cave" ~ "#eb0c99",
                             PARKNAME == "Grand Teton" ~ "#eb0c99",
                             PARKNAME == "Yellowstone" ~ "#eb0c99",
                             PARKNAME == "Theodore Roosevelt" ~ "#eb0c99",
                             PARKNAME == "Crater Lake" ~ "#eb0c99",
                             TRUE ~ "#0ceb8a")) %>%  
  
  shift_geometry(preserve_area = FALSE, position = "below") %>% 
  sf::st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

allparks<- c('Denali',
             'Gates of the Arctic',
             'Glacier Bay',
             'Katmai',
             'Kenai Fjords',
             'Kobuk Valley',
             'Lake Clark',
             'Wrangell-St. Elias',
             'Grand Canyon',
             'Petrified Forest',
             'Saguaro',
             'Hot Springs',
             'Channel Islands',
             'Joshua Tree',
             'Kings Canyon',
             'Lassen Volcanic',
             'Pinnacles',
             'Redwood',
             'Sequoia',
             'Yosemite',
             'Death Valley',
             'Black Canyon of the Gunnison',
             'Great Sand Dunes',
             'Mesa Verde',
             'Rocky Mountain',
             'Biscayne',
             'Dry Tortugas',
             'Everglades',
             'Haleakala',
             'Hawaii Volcanoes',
             'Indiana Dunes',
             'Mammoth Cave',
             'Acadia',
             'Isle Royale',
             'Gateway Arch',
             'Voyageurs',
             'Glacier',
             'Great Basin',
             'Carlsbad Caverns',
             'White Sands',
             'Theodore Roosevelt',
             'Cuyahoga Valley',
             'Crater Lake',
             'Congaree',
             'Badlands',
             'Wind Cave',
             'Great Smoky Mountains',
             'Big Bend',
             'Guadalupe Mountains',
             'Arches',
             'Bryce Canyon',
             'Canyonlands',
             'Capitol Reef',
             'Zion',
             'Virgin Islands',
             'Shenandoah',
             'Mount Rainier',
             'North Cascades',
             'Olympic',
             'New River Gorge',
             'Grand Teton',
             'Yellowstone')
nps <- nps2[nps2$PARKNAME %in% allparks, ]

## add National Parks to USA Base Map using leaflet()
map <- leaflet() %>%
  addPolygons(data = states,
              smoothFactor = 0.2,
              fillColor = "snow",
              fillOpacity = 0.5,
              stroke = TRUE,
              weight = 0.5,
              opacity = 0.5,
              color = "#808080",
              highlight = highlightOptions(
                weight = 0.5,
                color = "#000000",
                fillOpacity = 0.7,
                bringToFront = FALSE),
              group = "Base Map",
              popup = states$NAME)  %>% 
  addPolygons(data = nps,
              smoothFactor = 0.2,                 
              fillColor = nps$visited,
              fillOpacity = 1,
              stroke = TRUE,
              weight = 1,     
              opacity = 0.5,                       
              color = "#354f52",             
              highlight = highlightOptions(
                weight = 3,
                color = "#fff",
                fillOpacity = 0.8,
                bringToFront = TRUE),
              group = "National Park",
              popup = nps$PARKNAME,
              options=popupOptions(closeButton=TRUE)
  )  %>%
  addLayersControl(
    baseGroups = "Base Map",
    overlayGroups = "National Park",
    options = layersControlOptions(collapsed = FALSE))


map
