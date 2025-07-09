# -------------------------------------------------------------------------

# Step 1: Install/load libraries
library(httr)
library(jsonlite)
library(dplyr)
library(sf)
library(ggplot2)

token <- "IgBwexLSxntlocXcGGjgQKRNksaehRlM"  # your token

# Step 2: Download data


get_end_day <- function(month) {
  if (month == "06") {
    return("30")
  } else 
    return("31")
}
  
  
years <- 1990:2024
months <- c("06", "07", "08")
max_retries <- 3
limit <- 1000  
all_data <- list()
  
for (year in years) {
    for (month in months) {
      success <- FALSE
      offset <- 0
      collected_data <- list()
      
      repeat {
        for (attempt in 1:max_retries) {
          res <- tryCatch({
            GET(
              url = "https://www.ncei.noaa.gov/cdo-web/api/v2/data",
              query = list(
                datasetid = "GSOM",
                datatypeid = "TAVG",
                locationid = "FIPS:06",
                startdate = paste0(year, "-", month, "-01"),
                enddate = paste0(year, "-", month, "-", get_end_day(month)),
                units = "standard",
                limit = limit,
                offset = offset,
                includemetadata = TRUE
              ),
              add_headers(token = token)
            )
          }, error = function(e) NULL)
          
          if (!is.null(res)) {
            text <- content(res, "text", encoding = "UTF-8")
            
            if (!grepl("^\\s*<", text)) {
              content_json <- fromJSON(text)
              results <- content_json$results
              
              if (!is.null(results)) {
                collected_data <- append(collected_data, list(results))
                message(paste("✅ Year", year, "Month", month, 
                              ": page offset", offset, "collected"))
                offset <- offset + limit  # next page
                success <- TRUE
                break  # if succeed, then break
              } else {
                message(paste("⚠️ Year", year, "Month", month, 
                              ": no more data (offset", offset, ")"))
                success <- TRUE
                break  # if no data, break
              }
            } else {
              message(paste("⛔ Year", year, "Month", month, 
                            ": HTML returned (offset", offset, 
                            "Attempt", attempt, ")"))
            }
          } else {
            message(paste("❌ Year", year, "Month", month, 
                          ": Request failed (offset", offset, 
                          "Attempt", attempt, ")"))
          }
          
          Sys.sleep(runif(1, 3, 6))  # wait before retry
        }
        
        if (!success) {
          message(paste("🚫 Year", year, "Month", month, 
                        ": failed after", max_retries, 
                        "attempts (offset", offset, ")"))
          break  # fail for 3 times, break
        }
        
        if (length(results) < limit) break
      }
      
      # combine all the data
      if (length(collected_data) > 0) {
        all_data[[paste0(year, "-", month)]] <- do.call(rbind, collected_data)
      }
    }
  }

df <- bind_rows(all_data)
df$year <- substr(df$date, 1, 4)
df$month <- substr(df$date, 6, 7)
station_ids <- unique(df$station)

temp_2024<-df %>% filter(year=="2024")


offset <- 0
limit <- 1000
stations_all <- list()

repeat {
  res <- GET(
    url = "https://www.ncei.noaa.gov/cdo-web/api/v2/stations",
    query = list(
      locationid = "FIPS:06",   # California
      datasetid = "GSOM",       # Monthly summary dataset
      limit = limit,
      offset = offset
    ),
    add_headers(token = token)
  )
  
  content_text <- content(res, "text", encoding = "UTF-8")
  
  if (grepl("^\\s*<", content_text)) {
    cat("❌ HTML error at offset", offset, "\n")
    break
  }
  
  json_data <- fromJSON(content_text)
  
  if (is.null(json_data$results) || length(json_data$results) == 0) {
    cat("✅ No more stations found. Done.\n")
    break
  }
  
  stations_all[[length(stations_all) + 1]] <- json_data$results
  cat("📦 Retrieved", length(json_data$results), "stations at offset", offset, "\n")
  
  offset <- offset + limit
  Sys.sleep(0.5)  # avoid rate limiting
}


station_info<-rbind(stations_all[[1]],stations_all[[2]],stations_all[[3]]) %>% 
  as.data.frame()
station_df<-station_info %>% filter(id %in% station_ids)

temp_data<-df %>% left_join(station_info, by = c("station" = "id"))

data_sf<-temp_data %>% 
  group_by(year,latitude,longitude,elevation) %>% 
  summarize(avg_temp = mean(value)) 

write.csv(data_sf, "california_summer_avg_with_location.csv", row.names = FALSE)

data_sf<-st_as_sf(data_sf, coords = c("longitude","latitude"),crs = 4326)

temp_2024<-data_sf %>% filter(year=="2024")
  
shapefile_path <- "/Users/nancy/Desktop/USRA/california fire/tl_2023_us_state/tl_2023_us_state.shp"

# read california border map
stateshapes <- st_read(shapefile_path)
CA_shape<-stateshapes %>% filter(NAME=="California")

ggplot()+
  geom_sf(data = CA_shape)+
  geom_sf(data = temp_2024,aes(color = avg_temp))+
  scale_color_gradient(low = "yellow", high = "red",
                       limits = c(35, 105))+
  labs(title = "Average temperature in 2024 summer")






