library(sf)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(magrittr)
library(ggplot2)
library(mapview)
library(htmlwidgets)
library(htmltools)
library(httr)
library(jsonlite)
library(parsedate)

make_gif <- FALSE

complaint_pattern <- '[Ff]ireworks'

dates_of_interest <- c(as.Date('2020-06-09'), Sys.Date())

utm_proj <- '+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
geo_proj <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

if (!exists('dat')) {
  request <- URLencode("https://data.cityofnewyork.us/resource/erm2-nwe9.json?$where=created_date>='2010-01-01'&complaint_type='Illegal Fireworks'&$limit=500000")
  
  full_response <-  httr::GET(request)
  
  jsonRespText <- content(full_response, as="text") 
  full_dat <- fromJSON(jsonRespText)
  
  dat <- full_dat %>%
    mutate(created_date = as.Date(parsedate::parse_iso_8601(created_date)))
}

fh <- read.csv('csv/FDNY_Firehouse_Listing.csv')
pp <- read.csv('csv/NYPD_Station_Houses_Geo.csv')

fw <- dat %>%
  filter(!is.na(latitude),
         latitude != '',
         !is.na(longitude),
         longitude != '')

plot_dat <- fw %>%
  filter(grepl(complaint_pattern, complaint_type)) %>%
  mutate(Subset = between(created_date, dates_of_interest[1], dates_of_interest[2]))

g <- ggplot(data = plot_dat) +
  geom_bar(mapping = aes(x = created_date, colour = Subset, fill = Subset)) +
  labs(title = sprintf('Growth of %s 311 Calls',
                       paste(unique(plot_dat$complaint_type, collapse = ' & '))),
       subtitle = paste(range(strftime(plot_dat$created_date, format = '%m/%d/%Y')), collapse = ' - '),
       x = 'Date',
       y = 'Count of Complaints')

fw_sf <- fw %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs = geo_proj) %>%
  filter(between(created_date, dates_of_interest[1], dates_of_interest[2]),
         grepl(complaint_pattern, complaint_type))

fh_sf <- fh %>%
  filter(!is.na(Latitude),
         Latitude != '',
         !is.na(Longitude),
         Longitude != '') %>%
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = geo_proj)

pp_sf <- pp %>%
  filter(!is.na(Lat),
         Lat != '',
         !is.na(Long),
         Long != '') %>%
  st_as_sf(coords = c('Long', 'Lat'), crs = geo_proj)

rr <- tags$div(
  HTML(paste0(
    '<p><a style="color:black;font-size:18px;">',
    paste(unique(fw_sf$complaint_type), sep = ' & '), '</a></p>',
    paste(paste0('<a style="color:black;font-size:18px;">',
                 strftime(dates_of_interest, format = '%m/%d/%Y'), '</a>'),
          collapse = ' - '),
    '<p><a style="color:black;font-size:18px;">(n = ',
    nrow(fw_sf),
    ')</a></p>'
    ))
)  

l <- leaflet() %>%
  setView(lng = -73.967959,
          lat = 40.752047,
          zoom = 11) %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  # addCircleMarkers(data = fh_sf, radius = 2, color = 'red') %>%
  addMarkers(data = pp_sf,
             icon = icons(iconUrl = 'icons8-police-badge-50.png',
                          iconWidth = 10, iconHeight = 10,
                          iconAnchorX = 5, iconAnchorY = 5)) %>%
  addHeatmap(data = fw_sf,
             radius = 8) %>%
  addControl(rr, position = "bottomleft")

l


# Create GIF --------------------------------------------------------------

if (make_gif) {
  date_range <- seq.Date(as.Date('2020-05-23'), max(fw_sf$created_date, na.rm = TRUE), by = 1)
  
  for (d in date_range) {
    date = as.Date(d, origin = '1970-01-01')
    
    out_file <- sprintf('~/Documents/fw_geo/gif_plots/fw_%s.png', strftime(date, format = '%Y%m%d'))
    
    filtered <- fw_sf %>%
      filter(created_date == date)
    
    rr <- tags$div(
      HTML(paste0('<p style="color:black;font-size:18px;">', strftime(date, format = '%m/%d/%Y'), '</p>'))
    )  
    
    l <- leaflet() %>%
      setView(lng = -73.967959,
              lat = 40.752047,
              zoom = 11) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      addMarkers(data = pp_sf,
                 icon = icons(iconUrl = 'icons8-police-badge-50.png',
                              iconWidth = 10, iconHeight = 10,
                              iconAnchorX = 5, iconAnchorY = 5)) %>%
      addHeatmap(data = filtered,
                 radius = 8) %>%
      addControl(rr, position = "bottomleft")
    
    mapshot(x = l, file = out_file)
  }
}

# Geospatial stuff --------------------------------------------------------

library(ggmap)

fw_geo <- fw_sf %>%
  st_transform(crs = utm_proj) %>%
  filter(between(created_date, dates_of_interest[1], dates_of_interest[2]))

pp_geo <- pp_sf %>%
  st_transform(crs = utm_proj)

pp_buff <- st_buffer(pp_geo, dist = 1000)

inter <- st_intersection(fw_geo, pp_buff)

fw_geo <- fw_geo %>%
  mutate(in_buffer = unique_key %in% inter$unique_key)

zips <- st_read('geo/geo_export_f0d52cb0-1372-4e72-9272-1bc381f0846e.shp') %>%
  st_transform(crs = utm_proj)
boroughs <- st_read('geo//borough/geo_export_b9b63e75-0091-4599-91d5-66eda65e3fd9.shp') %>%
  st_transform(crs = utm_proj)

areas <- c()
area_rats <- c()
for (i in 1:nrow(zips)) {
  x <- zips[i,]
  
  zip_inter <- st_intersection(x, pp_buff)
  
  if (nrow(zip_inter) > 0) {
    final_inter <- st_intersection(zip_inter)
    
    final_area <- sum(st_area(final_inter))
    
    pp_area <- final_area
    full_area <- sum(st_area(x))
  } else {
    pp_area <- 0
    
    units(pp_area) <- 'm^2'
  }
  
  areas <- append(areas, pp_area)
  area_rats <- append(area_rats, pp_area/full_area)
}

zips$pp_area <- areas
zips$pp_area_rats <- area_rats
zips$pp_pop <- zips$pop_est * zips$pp_area_rats

zips_w_boroughs <- zips %>%
  st_intersection(boroughs)

man <- zips_w_boroughs %>%
  filter(boro_name == 'Manhattan')

pop_rats <- zips_w_boroughs %>%
  group_by(boro_name) %>%
  summarise(pp_pop = sum(pp_pop),
            pop_est = sum(pop_est),
            pp_rat = pp_pop / pop_est)
