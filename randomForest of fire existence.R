library(sf)
library(terra)
library(tidyverse)
library(jsonlite)
library(sp)

#try to distribute losses according to population distribution
population<-fromJSON("~/Desktop/USRA/california fire/loss/california-counties-by-population.json")
library(tigris)
ca_tracts <- tracts(state = "CA", cb = TRUE, class = "sf")


shapefile_path <- "/Users/nancy/Desktop/USRA/california fire/tl_2023_us_state/tl_2023_us_state.shp"
stateshapes <- st_read(shapefile_path)
CA_shape<-stateshapes %>% filter(NAME=="California")

fire_polygon<-st_read("~/Desktop/USRA/california fire/Fire data/fire_polygon.geojson")
year<-unique(fire_polygon$start_year)
mon<-unique(fire_polygon$start_mon)

raster_list <- list()

CA_vect<-vect(CA_shape)
template_raster <- rast(ext(CA_vect), resolution = 0.05)

for (i in year){
  for (j in mon){
    temp <- fire_polygon %>% 
      filter(start_year == i,
             start_mon == j)
    if (nrow(temp) > 0){
    temp_vect <- vect(temp)
    
    fire_raster <- rasterize(temp_vect, template_raster,
                             field = 1, background = 0)
    loss_raster <- rasterize(temp_vect, template_raster, 
                             field = "loss_estimated", background = 0)
    
    names(fire_raster) <- paste0("fire_", i, "_", sprintf("%02d", j))
    names(loss_raster) <- paste0("loss_", i, "_", sprintf("%02d", j))
    
    raster_list[[paste0(i, "-", sprintf("%02d", j), "_fire")]] <- fire_raster
    raster_list[[paste0(i, "-", sprintf("%02d", j), "_loss")]] <- loss_raster
    
    }
  }
}

full_stack <- rast(raster_list)

crs(CA_vect)<-crs(full_stack)
full_stack<-crop(full_stack,CA_vect)
full_stack <- mask(full_stack, CA_vect)

df <- as.data.frame(full_stack, xy = TRUE)

df_long <- pivot_longer(
  df,
  cols = c(ends_with("fire"), ends_with("loss")),
  names_to = "variable",
  values_to = "value"
)

fire_existence <- df %>%
  select(x, y, ends_with("_fire"))
fire_existence<-fire_existence %>% 
  pivot_longer(cols = -c(x,y),
               names_to = "time",
               values_to = "value")
fire_existence$year<-substr(fire_existence$time,1,4) %>% 
  as.numeric()
fire_existence$month<-substr(fire_existence$time,6,7) %>% 
  as.numeric()
fire_existence<-fire_existence %>% 
  select(-time) %>% 
  filter(year != 1969)

fire_loss<-df %>% 
  select(x, y, ends_with("_loss"))
fire_loss<-fire_loss %>% 
  pivot_longer(cols = -c(x,y),
               names_to = "time",
               values_to = "value")
fire_loss$year<-substr(fire_loss$time,1,4)%>% 
  as.numeric()
fire_loss$month<-substr(fire_loss$time,6,7)%>% 
  as.numeric()
fire_loss<-fire_loss %>% 
  select(-time)%>% 
  filter(year != 1969)


#rasterize the geographical factors
# elevation --------------------------------------
library(elevatr)
elev_raster <- get_elev_raster(locations = CA_shape, z = 8, clip = "bbox")
plot(elev_raster)

crs(elev_raster)<-crs(CA_vect)
elev_raster<-rast(elev_raster)
elev_raster<-crop(elev_raster,CA_vect)
elev_raster <- mask(elev_raster, CA_vect)
plot(elev_raster)

elev_raster<-resample(elev_raster,full_stack,method = "near")
elev_df<-as.data.frame(elev_raster,xy = TRUE)
elev_df <- elev_df %>% 
  rename("elevation" = filebe6924b87ba8)


# temp&prcp ---------------------------------------------------------------
temp_n_prcp<-read.csv("~/Desktop/USRA/california fire/california_temp_n_prcp_with_location_2013-2024.csv")
temperature_data<-temp_n_prcp %>% select(-prcp)
#Rasterize it
temperature_sf<-st_as_sf(temperature_data,coords = c("latitude","longitude"),crs = 4326)

rasterization_kriging <- function(data) {
  # Keep longitude and latitude as columns before setting coordinates
  data$longitude_val <- data$longitude
  data$latitude_val <- data$latitude
  
  # Generate prediction grid
  grid_lon <- seq(min(data$longitude), max(data$longitude), by = 0.1)
  grid_lat <- seq(min(data$latitude), max(data$latitude), by = 0.1)
  grid_df <- expand.grid(lon = grid_lon, lat = grid_lat)
  
  # Convert to spatial
  coordinates(data) <- ~longitude + latitude
  coordinates(grid_df) <- ~lon + lat
  gridded(grid_df) <- TRUE
  
  # IDW interpolation of elevation to fill grid
  idw_ele <- idw(elevation ~ 1, locations = data, newdata = grid_df, idp = 2.0)
  idw_ele_df <- as.data.frame(idw_ele)
  names(idw_ele_df)[names(idw_ele_df) == "var1.pred"] <- "elevation"
  
  prediction_grid_df <- as.data.frame(grid_df)
  prediction_grid_df$elevation <- idw_ele_df$elevation
  
  # Add longitude and latitude to prediction grid as needed for regression
  prediction_grid_df$longitude_val <- coordinates(grid_df)[,1]
  prediction_grid_df$latitude_val <- coordinates(grid_df)[,2]
  
  coordinates(prediction_grid_df) <- ~lon + lat
  gridded(prediction_grid_df) <- TRUE
  
  # Fit variogram using elevation + lat/lon as predictors
  vgm_model <- variogram(temp ~ elevation + longitude_val + latitude_val, data = data)
  fitted_vgm <- fit.variogram(vgm_model, model = vgm("Sph", nugget = 0.1))
  
  # Kriging regression with consistent formula
  kriging_result <- krige(temp ~ elevation + longitude_val + latitude_val,
                          locations = data,
                          newdata = prediction_grid_df,
                          model = fitted_vgm)
  
  # Convert to raster and post-process
  kriging_rast <- rast(kriging_result)[[1]]
  kriging_rast <- resample(kriging_rast, full_stack, method = "near")
  cropped_temp <- crop(kriging_rast, CA_vect)
  masked_temp <- mask(cropped_temp, CA_vect)
  
  return(masked_temp)
}

years<-2013:2024
mons<-1:12
temp_list <- list()

for (yr in years) {
  temp_list[[as.character(yr)]] <- list()
  for (mon in mons) {
    temp <- temperature_data %>%
      filter(year == yr, month == mon) %>%
      distinct(longitude, latitude, .keep_all = TRUE)
    
    temp_list[[as.character(yr)]][[as.character(mon)]] <- rasterization_kriging(temp)
  }
}
flat_temp_list <- unlist(temp_list, recursive = TRUE)
temp_stack <- rast(flat_temp_list)

temp_df <- as.data.frame(temp_stack, xy = TRUE, na.rm = FALSE)
temp_df <- temp_df %>%
  pivot_longer(
    cols = c(-x, -y),
    names_to = "time",
    values_to = "value"
  ) %>%
  mutate(
    year = as.numeric(substr(time, 1, 4)),
    month = as.numeric(substr(time, 6, 6))
  ) %>%
  rename(temperature = value)


fire_existence_with_temp<-fire_existence %>% 
  left_join(temp_df, by = c("x", "y", "year", "month")) %>% 
  na.omit()



#precipitation

precipitation_data<-temp_n_prcp %>% select(-temp)


rasterization_kriging_prcp <- function(data) {
  # Keep longitude and latitude as columns before setting coordinates
  data$longitude_val <- data$longitude
  data$latitude_val <- data$latitude
  
  # Generate prediction grid
  grid_lon <- seq(min(data$longitude), max(data$longitude), by = 0.1)
  grid_lat <- seq(min(data$latitude), max(data$latitude), by = 0.1)
  grid_df <- expand.grid(lon = grid_lon, lat = grid_lat)
  
  # Convert to spatial
  coordinates(data) <- ~longitude + latitude
  coordinates(grid_df) <- ~lon + lat
  gridded(grid_df) <- TRUE
  
  # IDW interpolation of elevation to fill grid
  idw_ele <- idw(elevation ~ 1, locations = data, newdata = grid_df, idp = 2.0)
  idw_ele_df <- as.data.frame(idw_ele)
  names(idw_ele_df)[names(idw_ele_df) == "var1.pred"] <- "elevation"
  
  prediction_grid_df <- as.data.frame(grid_df)
  prediction_grid_df$elevation <- idw_ele_df$elevation
  
  # Add longitude and latitude to prediction grid as needed for regression
  prediction_grid_df$longitude_val <- coordinates(grid_df)[,1]
  prediction_grid_df$latitude_val <- coordinates(grid_df)[,2]
  
  coordinates(prediction_grid_df) <- ~lon + lat
  gridded(prediction_grid_df) <- TRUE
  
  # Fit variogram using elevation + lat/lon as predictors
  vgm_model <- variogram(prcp ~ elevation + longitude_val + latitude_val, data = data)
  fitted_vgm <- fit.variogram(vgm_model, model = vgm("Sph", nugget = 0.1))
  
  # Kriging regression with consistent formula
  kriging_result <- krige(prcp ~ elevation + longitude_val + latitude_val,
                          locations = data,
                          newdata = prediction_grid_df,
                          model = fitted_vgm)
  
  # Convert to raster and post-process
  kriging_rast <- rast(kriging_result)[[1]]
  kriging_rast <- resample(kriging_rast, full_stack, method = "near")
  cropped_temp <- crop(kriging_rast, CA_vect)
  masked_temp <- mask(cropped_temp, CA_vect)
  
  return(masked_temp)
}

prcp_list <- list()

for (yr in years) {
  prcp_list[[as.character(yr)]] <- list()
  for (mon in mons) {
    temp <- precipitation_data %>%
      filter(year == yr, month == mon) %>%
      distinct(longitude, latitude, .keep_all = TRUE) %>% 
      na.omit()
    
    prcp_list[[as.character(yr)]][[as.character(mon)]] <- rasterization_kriging_prcp(temp)
  }
}
flat_prcp_list <- unlist(prcp_list, recursive = TRUE)
prcp_stack <- rast(flat_prcp_list)

prcp_df <- as.data.frame(prcp_stack, xy = TRUE, na.rm = FALSE)
prcp_df <- prcp_df %>%
  pivot_longer(
    cols = c(-x, -y),
    names_to = "time",
    values_to = "value"
  ) %>%
  mutate(
    year = as.numeric(substr(time, 1, 4)),
    month = as.numeric(substr(time, 6, 6))
  ) %>%
  rename(precipitation = value)

#there are some duplicated parts
#take the average of the same position and same time
prcp_df <- prcp_df %>%
  group_by(x, y, year, month) %>%
  summarise(precipitation = mean(precipitation, na.rm = TRUE), .groups = "drop")



fire_existence_with_temp_prcp<-fire_existence_with_temp %>% 
  left_join(prcp_df, by = c("x", "y", "year", "month")) %>% 
  na.omit()

fire_existence_with_temp_prcp<-fire_existence_with_temp_prcp %>% 
  left_join(elev_df, by = c("x", "y")) %>% 
  na.omit()

write.csv(fire_existence_with_temp_prcp, "/Users/nancy/Desktop/USRA/california fire/california_fire_existence_data.csv", row.names = FALSE)



# random forest -----------------------------------------------------------
fire_existence_data <- read.csv("/Users/nancy/Desktop/USRA/california fire/california_fire_existence_data.csv")
fire_existence_data$value <- factor(fire_existence_data$value, levels = c(0, 1), labels = c("no", "yes"))

library(caret)
set.seed(123)
split_index <- createDataPartition(fire_existence_data$value, p = 0.8, list = FALSE)
train_data <- fire_existence_data[split_index, ]
test_data  <- fire_existence_data[-split_index, ]

library(ranger)
rf_model <- ranger(value ~ elevation + temperature + precipitation,
                   data = train_data,
                   num.trees = 200,
                   mtry = 2,
                   probability = TRUE, # predicted probabilities
                   min.node.size = 10,
                   class.weights = c("no" = 1, "yes" = 10))

rf_pred <- predict(rf_model, data = test_data)$predictions
rf_prob_yes <- rf_pred[, "yes"]

threshold <- 0.1
rf_pred_class <- ifelse(rf_prob_yes > threshold, "yes", "no")
rf_pred_class <- as.factor(rf_pred_class)
confusionMatrix(rf_pred_class, test_data$value)

library(pROC)
rf_roc <- roc(response = test_data$value,
              predictor = rf_prob_yes,
              levels = c("no", "yes"))
plot(rf_roc)
auc(rf_roc)


# overfitting -------------------------------------------------------------

# Predicted probabilities for training and test sets
train_pred <- predict(rf_model, data = train_data)$predictions[, "yes"]
test_pred  <- predict(rf_model, data = test_data)$predictions[, "yes"]

# True labels (numeric)
train_true <- ifelse(train_data$value == "yes", 1, 0)
test_true  <- ifelse(test_data$value == "yes", 1, 0)

library(Metrics)

train_rmse <- rmse(train_true, train_pred)
test_rmse  <- rmse(test_true, test_pred)

train_r2 <- 1 - sum((train_true - train_pred)^2) / sum((train_true - mean(train_true))^2)
test_r2  <- 1 - sum((test_true - test_pred)^2) / sum((test_true - mean(test_true))^2)

data.frame(
  Train_R2 = train_r2,
  Test_R2  = test_r2,
  Train_RMSE = train_rmse,
  Test_RMSE  = test_rmse
)

#suggest overfitting

# optimization ------------------------------------------------------------

control <- trainControl(method = "cv")

model <- train(value ~ elevation + temperature + precipitation, 
               data = train_data,
               method = "ranger", 
               trControl = control)
model$bestTune
#mtry = 3
#splitrule = gini
#min.node.size = 1




# -------------------------------------------------------------------------

control <- trainControl(method = "cv", number = 5)

grid <- expand.grid(
  mtry = c(2, 3),
  splitrule = "gini",
  min.node.size = c(5, 10, 20)
)

set.seed(123)
model <- train(
  value ~ elevation + temperature + precipitation,
  data = train_data,
  method = "ranger",
  trControl = control,
  tuneGrid = grid,
  importance = "impurity"
)

print(model$bestTune)
# 假设输出为 mtry = 2, splitrule = "gini", min.node.size = 10

# Step 4: 用最优参数重新训练 ranger 模型
rf_model <- ranger(
  value ~ elevation + temperature + precipitation,
  data = train_data,
  num.trees = 200,
  mtry = model$bestTune$mtry,
  splitrule = model$bestTune$splitrule,
  min.node.size = model$bestTune$min.node.size,
  probability = TRUE
)

rf_pred <- predict(rf_model, data = test_data)$predictions
rf_prob_yes <- rf_pred[, "yes"]


threshold <- 0.1 
rf_pred_class <- ifelse(rf_prob_yes > threshold, "yes", "no")
rf_pred_class <- as.factor(rf_pred_class)

confusionMatrix(rf_pred_class, test_data$value)


rf_roc <- roc(response = test_data$value, predictor = rf_prob_yes, levels = c("no", "yes"))
plot(rf_roc)
auc(rf_roc)


train_pred <- predict(rf_model, data = train_data)$predictions[, "yes"]
test_pred  <- rf_prob_yes

train_true <- ifelse(train_data$value == "yes", 1, 0)
test_true  <- ifelse(test_data$value == "yes", 1, 0)

Train_R2 <- 1 - sum((train_true - train_pred)^2) / sum((train_true - mean(train_true))^2)
Test_R2  <- 1 - sum((test_true - test_pred)^2) / sum((test_true - mean(test_true))^2)
Train_RMSE <- rmse(train_true, train_pred)
Test_RMSE  <- rmse(test_true, test_pred)

data.frame(
  Train_R2 = Train_R2,
  Test_R2  = Test_R2,
  Train_RMSE = Train_RMSE,
  Test_RMSE  = Test_RMSE
)


# xgbooster ---------------------------------------------------------------



#xgbooster method
fire_existence_data <- read.csv("/Users/nancy/Desktop/USRA/california fire/california_fire_existence_data.csv")

#install.packages("gbm")
library(gbm)
brt_model <- gbm(
  formula = value ~ .,
  data = fire_existence_data,
  distribution = "bernoulli",  
  n.trees = 1000,
  interaction.depth = 3,
  shrinkage = 0.01,
  bag.fraction = 0.5,
  train.fraction = 0.8,
  verbose = TRUE
)

#install.packages("xgboost")
library(xgboost)

labels <- fire_existence_data$value
features <- fire_existence_data %>% select(-value,time, year, month) %>% as.matrix()

set.seed(42)
train_index <- createDataPartition(labels, p = 0.8, list = FALSE)
train_data <- xgb.DMatrix(data = features[train_index, ], label = labels[train_index])
test_data  <- xgb.DMatrix(data = features[-train_index, ], label = labels[-train_index])


params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 4,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

n_positive <- sum(labels == 1)
n_negative <- sum(labels == 0)
params$scale_pos_weight <- n_negative / n_positive

xgb_model <- xgb.train(
  params = params,
  data = train_data,
  nrounds = 100,
  watchlist = list(train = train_data, eval = test_data),
  early_stopping_rounds = 10,
  print_every_n = 10
)

pred_prob <- predict(xgb_model, test_data)
pred_class <- ifelse(pred_prob > 0.8, 1, 0)
confusionMatrix(as.factor(pred_class), as.factor(labels[-train_index]))


params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",  
  max_depth = 4,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  scale_pos_weight = sum(labels == 0) / sum(labels == 1)
)

set.seed(42)
cv <- xgb.cv(
  params = params,
  data = train_data,
  nrounds = 500,
  nfold = 5,
  stratified = TRUE,
  early_stopping_rounds = 10,
  verbose = 1
)

best_nrounds <- cv$best_iteration

xgb_model <- xgb.train(
  params = params,
  data = train_data,
  nrounds = best_nrounds
)

pred_prob <- predict(xgb_model, test_data)

threshold <- 0.9
pred_class <- ifelse(pred_prob > threshold, 1, 0)

confusionMatrix(as.factor(pred_class), as.factor(labels[-train_index]))








