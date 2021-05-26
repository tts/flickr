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

# Split file to prevent data loss in sudden wifi hiccups
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
         latitude,longitude,starts_with("url_"),
         town,municipality,state_district,state,region,country,country_code)
geocoded2 <- geocoded2 %>% 
  select(title,tags,
         datetaken,count_views,count_faves,
         latitude,longitude,starts_with("url_"),
         town,municipality,state_district,state,region,country,country_code)
geocoded3 <- geocoded3 %>% 
  select(title,tags,
         datetaken,count_views,count_faves,
         latitude,longitude,starts_with("url_"),
         town,municipality,state_district,state,region,country,country_code)
geocoded4 <- geocoded4 %>% 
  select(title,tags,
         datetaken,count_views,count_faves,
         latitude,longitude,starts_with("url_"),
         town,municipality,state_district,state,region,country,country_code)

geocoded <- rbind(geocoded1, geocoded2, geocoded3, geocoded4)
write_rds(geocoded, "flickr_geocoded.RDS")

# Add popup link to photo
geo_coded <- geocoded %>% 
  mutate(popup_medium = paste("<a href='", geocoded$url_z,"'>", geocoded$url_z, "</a>"),
         popup_small = paste("<a href='", geocoded$url_m,"'>", geocoded$url_m, "</a>"))

write_rds(geo_coded, "flickr_geo_coded.RDS")

# Remove big objects
rm(geocoded1,geocoded2,geocoded3,geocoded4,
   geotagged1,geotagged2,geotagged3,geotagged4,
   geotagged, geocoded)
gc()

#geo_coded <- readRDS("flickr_geo_coded.RDS")

# Clean names
geo_coded <- geo_coded %>% 
  mutate(country = ifelse(country == "中国", "Hong Kong", 
                          ifelse(country == "République démocratique du Congo", "Uganda", # at the border
                                 ifelse(country == "Madagasikara", "Madagascar",
                                        ifelse(country == "Luzon", "The Philippines",
                                               ifelse(country == "ኢትዮጵያ", "Ethiopia",
                                                      ifelse(country_code == "cz", "Czechia",
                                                             ifelse(country == "Eesti", "Estonia", 
                                                                    ifelse(country == "España", "Spain",
                                                                           ifelse(country == "Italia", "Italy",
                                                                                  ifelse(country == "Sverige", "Sweden",
                                                                                         ifelse(country == "Norge", "Norway",
                                                                                                ifelse(country == "Deutschland", "Germany",
                                                                                                       ifelse(country == "Nederland", "The Nederlands",
                                                                                                              ifelse(country == "Suomi / Finland", "Finland", country)))))))))))))))
# One row without a country name for a reason
geo_coded$country <- with(geo_coded, replace(country, 
  popup_medium == "<a href=' https://live.staticflickr.com/4230/34534378343_bc18f47537_z.jpg '> https://live.staticflickr.com/4230/34534378343_bc18f47537_z.jpg </a>",
  "International waters")) 

# Stats
c_stats <- geo_coded %>% 
  group_by(country) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

write_rds(geo_coded, "flickr_geo_coded.RDS")
