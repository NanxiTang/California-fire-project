#The goal of this file is to rasterize the sf dataframe


setwd("~/Desktop/USRA/california fire")

temperature_2024<-read.csv("temperature_202406-202408.csv")

avg_temp_2024<-temperature_2024 %>% 
  group_by(STATION) %>% 
  summarize(avg_temp = mean(TAVG),
            lat = mean(LATITUDE),
            lon = mean(LONGITUDE),
            ele = mean(ELEVATION)) %>% 
  na.omit()
avg_temp_sf_2024<-st_as_sf(avg_temp_2024,coords = c("lon","lat"), crs = 4326)

temp_2024_vec<-vect(avg_temp_sf_2024)
temp_2024_vec

cell_size <- 1 #modify the size of the grid
lon_min <- -125; lon_max <- -114; lat_min <- 32; lat_max <- 42
ncols <- ((lon_max - lon_min)/cell_size)+1
nrows <- ((lat_max - lat_min)/cell_size)+1
CA_temp_2024 <- rast(nrows=nrows, 
                     ncols=ncols, 
                     xmin=lon_min, 
                     xmax=lon_max, 
                     ymin=lat_min, 
                     ymax=lat_max, 
                     res=cell_size, 
                     crs="+proj=longlat +datum=WGS84")

CA_temp_2024 <- rasterize(temp_2024_vec, 
                          CA_temp_2024, 
                          field = "avg_temp", 
                          fun="mean")
plot(CA_temp_2024,col=brewer.pal(9,"YlOrRd"))
plot(CA_shape$geometry,add = TRUE)
plot(fire_2024_lightening$geometry,col = "black", add = TRUE)

CA_temp_2024_df<-as.data.frame(CA_temp_2024,xy = TRUE)
ggplot() +
  geom_raster(data = CA_temp_2024_df, aes(x = x, y = y, fill = mean)) +
  geom_sf(data = CA_shape, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = fire_2024_lightening, fill = NA, color = "black")+
  coord_sf() +
  theme_minimal() +
  labs(title = "Avg Temp & Fire in 2024")+
  scale_fill_gradient(name = "Temp (F)", low = "yellow", high = "red")



# IDW method Inversed Distance Weighting --------------------------------------------------------------

library(gstat)

temp_sp_2024 <- as_Spatial(avg_temp_sf_2024)
raster_2024 <- raster(extent(temp_sp_2024), res = 0.1)
crs(raster_2024) <- crs(temp_sp_2024)
# Convert empty raster to SpatialPixelsDataFrame
grid_2024 <- as(raster_2024, "SpatialPixelsDataFrame")

# Run IDW interpolation correctly
idw_result <- idw(formula = avg_temp ~ 1,
                  locations = temp_sp_2024,
                  newdata = grid_2024)

idw_raster <- raster(idw_result)
idw_df_2024 <- as.data.frame(idw_raster, xy = TRUE)

ggplot() +
  geom_raster(data = idw_df_2024, aes(x = x, y = y, fill = var1.pred)) +
  geom_sf(data = CA_shape, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = fire_2024_lightening, fill = NA, color = "black")+
  coord_sf() +
  theme_minimal() +
  labs(title = "Avg Temp & Fire in 2024")+
  scale_fill_gradient(name = "Temp (F)", low = "yellow", high = "red",limits = c(35, 80))
