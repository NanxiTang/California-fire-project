loss_data<-read.csv("~/Desktop/USRA/california fire/loss/mapdataall.csv")

#data_manipulation of the data time

loss_data$incident_dateonly_created<-loss_data$incident_dateonly_created %>% 
  as.Date()
loss_data$incident_dateonly_extinguished<-as.Date(
  ifelse(loss_data$incident_dateonly_extinguished=="",
         substr(loss_data$incident_date_last_update,1,10),
         loss_data$incident_dateonly_extinguished),
  format = "%Y-%m-%d")
loss_data$duration<-loss_data$incident_dateonly_extinguished-
  loss_data$incident_dateonly_created
loss_data$start_year<-format(loss_data$incident_dateonly_created, "%Y") %>% as.numeric()
loss_data$start_mon<-format(loss_data$incident_dateonly_created, "%m") %>% as.numeric()


library(rvest)
library(dplyr)
library(stringr)
library(sf)
library(ggplot2)

get_fire_resources_and_damage <- function(url) {
  page <- read_html(url)
  
  #Extract Burned Area
  area <- page %>%
    html_nodes(xpath = "//*[contains(text(), ' Acres')]") %>%  # any element containing "Acres"
    html_text(trim = TRUE) %>%
    .[1] %>%                                # take the first occurrence
    str_extract("[0-9,]+") %>%              # extract numeric part
    str_replace_all(",", "") %>%            # remove commas
    as.numeric()
  
  # Extract resources assigned
  personnel <- page %>% html_nodes(xpath = "//div[contains(text(), 'Personnel')]/preceding-sibling::div") %>% 
    html_text(trim = TRUE) %>% as.numeric()
  helicopters <- page %>% html_nodes(xpath = "//div[contains(text(), 'Helicopters')]/preceding-sibling::div") %>% 
    html_text(trim = TRUE) %>% as.numeric()
  engines <- page %>% html_nodes(xpath = "//div[contains(text(), 'Engines')]/preceding-sibling::div") %>% 
    html_text(trim = TRUE) %>% as.numeric()
  dozer <- page %>% html_nodes(xpath = "//div[contains(text(), 'Dozer')]/preceding-sibling::div") %>% 
    html_text(trim = TRUE) %>% as.numeric()
  Water_Tenders <- page %>% html_nodes(xpath = "//div[contains(text(), 'Water Tenders')]/preceding-sibling::div") %>% 
    html_text(trim = TRUE) %>% as.numeric()
  crews <- page %>% html_nodes(xpath = "//div[contains(text(), 'Crews')]/preceding-sibling::div") %>% 
    html_text(trim = TRUE) %>% as.numeric()
  Other_Assigned <- page %>% html_nodes(xpath = "//div[contains(text(), 'Other Assigned')]/preceding-sibling::div") %>% 
    html_text(trim = TRUE) %>% as.numeric()
  
  # Extract damage assessment
  structures_damaged <- page %>% html_nodes(xpath = "//div[contains(text(), 'Structures Damaged')]/preceding-sibling::div") %>% 
    html_text(trim = TRUE) %>% as.numeric()
  structures_destroyed <- page %>% 
    html_nodes(xpath = "//div[contains(text(), 'Structures Destroyed')]/preceding-sibling::div") %>% 
    html_text(trim = TRUE) %>% 
    gsub(",", "", .) %>%       # remove commas
    as.numeric()
  civilian_fatalities <- page %>% html_nodes(xpath = "//div[contains(text(), 'Confirmed Civilian Fatalities')]/preceding-sibling::div") %>% 
    html_text(trim = TRUE) %>% as.numeric()
  civilian_injuries <- page %>% html_nodes(xpath = "//div[contains(text(), 'Confirmed Civilian Injuries')]/preceding-sibling::div") %>% 
    html_text(trim = TRUE) %>% as.numeric()
  firefighter_fatalities<-page %>% html_nodes(xpath = "//div[contains(text(), 'Confirmed Firefighter Fatalities')]/preceding-sibling::div") %>% 
    html_text(trim = TRUE) %>% as.numeric()
  firefighter_injuries <- page %>% html_nodes(xpath = "//div[contains(text(), 'Confirmed Firefighter Injuries')]/preceding-sibling::div") %>% 
    html_text(trim = TRUE) %>% as.numeric()
  
  # Return as a data frame row
  return(data.frame(
    area_acres = ifelse(length(area) == 0, NA, area),
    
    personnel = ifelse(length(personnel) == 0, NA, personnel),
    helicopters = ifelse(length(helicopters) == 0, NA, helicopters),
    engines = ifelse(length(engines) == 0, NA, engines),
    dozer = ifelse(length(dozer) == 0, NA, dozer),
    Water_Tenders = ifelse(length(Water_Tenders) == 0, NA, Water_Tenders),
    crews = ifelse(length(crews) == 0, NA, crews),
    Other_Assigned = ifelse(length(Other_Assigned) == 0, NA, Other_Assigned),
    
    structures_damaged = ifelse(length(structures_damaged) == 0, NA, structures_damaged),
    structures_destroyed = ifelse(length(structures_destroyed) == 0, NA, structures_destroyed),
    civilian_fatalities = ifelse(length(civilian_fatalities) == 0, NA, civilian_fatalities),
    civilian_injuries = ifelse(length(civilian_injuries) == 0, NA, civilian_injuries),
    firefighter_fatalities = ifelse(length(firefighter_fatalities) == 0, NA, firefighter_fatalities),
    firefighter_injuries = ifelse(length(firefighter_injuries) == 0, NA, firefighter_injuries),
    stringsAsFactors = FALSE
  ))
}

#get the details for each fire according to their links
#it takes approximately 30 mins
loss_link <- loss_data$incident_url
get_fire_resources_and_damage("https://www.fire.ca.gov/incidents/2025/7/9/orleans-complex")
result <- lapply(loss_link, get_fire_resources_and_damage)

#combine the loss data with the fire data
loss_df<-bind_rows(result)
loss_df[is.na(loss_df)] <- 0
loss_data_combined<-bind_cols(loss_data,loss_df)
loss_data_combined$loss_estimated<-loss_data_combined$structures_damaged*150+
  loss_data_combined$structures_destroyed*500+
  loss_data_combined$civilian_fatalities*2000+
  loss_data_combined$civilian_injuries*300+
  loss_data_combined$firefighter_injuries*300+
  loss_data_combined$firefighter_fatalities*750


#select the rows that I will use for analysis and prediction
loss_data_clean<-loss_data_combined %>% 
  select(area = incident_acres_burned,
         longitude = incident_longitude,
         latitude = incident_latitude,
         start_year = start_year,
         start_mon = start_mon,
         duration,
         loss_estimated)
loss_data_clean<-loss_data_clean %>% filter(longitude>=-125,longitude<=-114,
                                            latitude>=32,latitude<=42.1)

fire_sf <- st_as_sf(loss_data_clean, coords = c("longitude", "latitude"), crs = 4326)
fire_sf<-fire_sf %>% na.omit()

#visualization of the wildfire corresponding to its size among all years
ggplot() +
  geom_sf(data = fire_sf, aes( size = area), shape = 21, fill = "red", color = "black", alpha = 0.6) +
  scale_size(range = c(1, 15)) + 
  theme_minimal() +
  labs(title = "Wildfire Area Map", size = "Area (acres)")


fire_2024<-fire_sf %>% filter(start_year == 2024) %>% 
  filter(start_mon %in% c(6,7,8))

ggplot() +
  geom_sf(data = fire_2024, aes( size = area), shape = 21, fill = "red", color = "black", alpha = 0.6) +
  geom_sf(data = CA_shape, fill = NA)+
  scale_size(range = c(1, 15)) + 
  theme_minimal() +
  labs(title = "Wildfire Area Map (2024)", size = "Area (acres)")

fire_2024_original<-fire_data %>% 
  filter(YEAR_ == 2024,
         season == "Summer")

ggplot() +
  geom_sf(data = fire_2024_original,fill = "darkorange", color = "darkred")+
  geom_sf(data = fire_2024, aes( size = area), shape = 21, fill = "red", color = "black", alpha = 0.6) +
  geom_sf(data = CA_shape, fill = NA)+
  scale_size(range = c(1, 15)) + 
  theme_minimal() +
  labs(title = "Wildfire Area Map (2024)", size = "Area (acres)")




#Instead of determining size by the area roughly, convert it to an approximate buffer.


#Convert acres to m² and then to radius (meters)
fire_2024 <- fire_2024 %>%
  mutate(area_m2 = area * 4046.86,
         radius_m = sqrt(area_m2 / pi))

fire_2024_proj <- st_transform(fire_2024, crs = 3310)#Project to a CRS with meters

fire_2024_proj <- fire_2024_proj %>%
  mutate(geometry = st_buffer(geometry, dist = radius_m))#Create spatial circles (buffers in meters)

fire_2024_wgs84 <- st_transform(fire_2024_proj, crs = 4326)#Reproject back to EPSG:4326 (lon/lat degrees)

ggplot() +
  geom_sf(data = fire_2024_original,fill = "darkorange", color = "darkred")+
  geom_sf(data = fire_2024_wgs84, fill = "blue", color = "black", alpha = 0.5) +
  geom_sf(data = CA_shape, fill = NA)+
  labs(title = "Wildfire Area Map (2024)",
       subtitle = "Circle area = fire area (acres), scaled to map",
       caption = "Projected via EPSG:3310 then transformed back to WGS84")


# transform the overall sf polygon ----------------------------------------

fire_polygon<- fire_sf %>%
  mutate(area_m2 = area * 4046.86,
         radius_m = sqrt(area_m2 / pi))

fire_proj <- st_transform(fire_polygon, crs = 3310)#Project to a CRS with meters

fire_proj <- fire_proj %>%
  mutate(geometry = st_buffer(geometry, dist = radius_m))#Create spatial circles (buffers in meters)

fire_wgs84 <- st_transform(fire_proj, crs = 4326)#Reproject back to EPSG:4326 (lon/lat degrees)
fire_wgs84$duration<-fire_wgs84$duration %>% 
  as.numeric()
write_sf(fire_wgs84, "~/Desktop/USRA/california fire/Fire data/fire_polygon.geojson")



