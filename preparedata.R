library(photosearcher)
library(tidyverse)
library(tidygeocoder)

start.time <- Sys.time()

# Search across all my photos so far
geotagged <- photo_search(
  mindate_taken = "2005-03-26",
  maxdate_taken = "2021-05-25",
  user_id = "dadaa",
  has_geo = TRUE
) 

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken # ~1 min

write_rds(geotagged, "flickr_geotagged.RDS")

# Split file to prevent data loss in sudden wifi hiccups when using the OSM API
geotagged1 <- geotagged[1:1000,]
geotagged2 <- geotagged[1001:2000,]
geotagged3 <- geotagged[2001:3000,]
geotagged4 <- geotagged[3001:nrow(geotagged),]

# Reverse geocode with OSM at 1/sec
geocoded1 <- geotagged1 %>% 
  reverse_geocode(lat = latitude, long = longitude, method = 'osm',
                address = address_found, full_results = TRUE)
write_rds(geocoded1, "flickr_geocoded1.RDS")

geocoded2 <- geotagged2 %>% 
  reverse_geocode(lat = latitude, long = longitude, method = 'osm',
                  address = address_found, full_results = TRUE)
write_rds(geocoded2, "flickr_geocoded2.RDS")

geocoded3 <- geotagged3 %>% 
  reverse_geocode(lat = latitude, long = longitude, method = 'osm',
                  address = address_found, full_results = TRUE)
write_rds(geocoded3, "flickr_geocoded3.RDS")

geocoded4 <- geotagged4 %>% 
  reverse_geocode(lat = latitude, long = longitude, method = 'osm',
                  address = address_found, full_results = TRUE)
write_rds(geocoded4, "flickr_geocoded4.RDS")

# geocoded1 <- readRDS("flickr_geocoded1.RDS")
# geocoded2 <- readRDS("flickr_geocoded2.RDS")
# geocoded3 <- readRDS("flickr_geocoded3.RDS")
# geocoded4 <- readRDS("flickr_geocoded4.RDS")

# Select columns and combine all
geocoded1 <- geocoded1 %>% 
  select(title,tags,
         datetaken,count_views,count_faves,
         latitude,longitude,url_q, url_z, 
         town,municipality,state_district,state,region,country,country_code)
geocoded2 <- geocoded2 %>% 
  select(title,tags,
         datetaken,count_views,count_faves,
         latitude,longitude,url_q, url_z, 
         town,municipality,state_district,state,region,country,country_code)
geocoded3 <- geocoded3 %>% 
  select(title,tags,
         datetaken,count_views,count_faves,
         latitude,longitude,url_q, url_z, 
         town,municipality,state_district,state,region,country,country_code)
geocoded4 <- geocoded4 %>% 
  select(title,tags,
         datetaken,count_views,count_faves,
         latitude,longitude,url_q, url_z, 
         town,municipality,state_district,state,region,country,country_code)

geocoded <- rbind(geocoded1, geocoded2, geocoded3, geocoded4)
write_rds(geocoded, "flickr_geocoded.RDS")

geo_coded <- geocoded %>% 
  mutate(popup_img = paste("<a href='", geocoded$url_z,"'><img src='", geocoded$url_q, "'></a>"),
         tags = gsub("uploaded:by=flickrmobile flickriosapp:filter=nofilter", "", tags)) %>% 
  select(-starts_with("url_"))

# Remove big objects
rm(geocoded1,geocoded2,geocoded3,geocoded4,
   geotagged1,geotagged2,geotagged3,geotagged4,
   geotagged,geocoded)
gc()

# Clean, and replace native names with English
geo_coded <- geo_coded %>% 
  mutate(country = case_when(
    country == "中国" ~ "Hong Kong",
    country == "République démocratique du Congo" ~ "Uganda",
    country == "Brasil" ~ "Brazil",
    country == "Madagasikara" ~ "Madagascar",
    country == "Luzon" ~ "The Philippines",
    country == "ኢትዮጵያ" ~ "Ethiopia",
    country_code == "cz" ~ "Czechia",
    country == "Eesti" ~ "Estonia", 
    country == "España" ~ "Spain",
    country == "Italia" ~ "Italy",
    country == "Sverige" ~ "Sweden",
    country == "Norge" ~ "Norway",
    country == "Deutschland" ~ "Germany",
    country == "Nederland" ~ "The Nederlands",
    country == "Suomi / Finland" ~ "Finland", 
    datetaken == "2017-06-16 08:24:49" ~ "International waters",
    TRUE ~ country
    ))
    
# The number of photos by country
c_stats <- geo_coded %>% 
  group_by(country) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

write_rds(geo_coded, "flickr_geo_coded.RDS")
