## 08_data crawling from JDC (Citrus fruit, weather, and agricultural practice data)

rm(list=ls())
library(jsonlite)
library(httr)
library(tidyverse)

# Get Citrus farm list data
# You need to sign up for the Open Data Portal to get your personal API authentication key.
url <- "http://apis.data.go.kr/B551391/farmsInfo/getCitrusFarmInfo?"

key <- "your personal API key"

type_data_format <- "json"

res <- GET(url=url, 
           query = list(serviceKey=key |> I(),
                        resultType=type_data_format))

print(x=res)
res |> 
  content(as='text', encoding='UTF-8') |> 
  fromJSON() -> json
str(json$result)
class(json$result)

json.v <- unlist(json, recursive = TRUE, use.names = TRUE)
json.v
farm_id <- json.v[grepl("farm_id", names(json.v))]
farm_name <- json.v[grepl("farm_name", names(json.v))]
farm_address <- json.v[grepl("farm_address", names(json.v))]
farm_latitude <- json.v[grepl("farm_latitude", names(json.v))]
farm_longitude <- json.v[grepl("farm_longitude", names(json.v))]
farm_area <- json.v[grepl("farm_area", names(json.v))]
str(farm_name)
citrus_df <- data.frame(id=farm_id,
                        name=farm_name,
                        address=farm_address,
                        latitude=farm_latitude,
                        longitude=farm_longitude,
                        area=farm_area)
citrus_df
citrus <- as_tibble(citrus_df)
citrus

#dir.create("08_Citrus_farm_list")
write.csv(citrus, "./08_Citrus_farm_list/citrus_farm.csv")
# Then move this file to other folder.


## Get Citrus fruit data
url_2 <- "https://apis.data.go.kr/B551391/citrusInfo/getCitrusInfoByWeek?"

key <- "your personal API key"

farmid <- c("aab", "bab", "cab", "dab", "eab", "fab", "gab", 
            "hab", "iab", "jab", "kab",
            "lab", "mab", "nab", "oab", "pab", "qab", "rab",
            "sab", "tab", "uab", "vab", "wab",
            "xab", "yab", "zab", "abb", "bbb", "cbb", "dbb")


getMonth <- 11     ##10, 11
getWeek <- 4      ##1, 2, 3, 4

url_list <- list()
cnt <- 0

for ( i in 1:30) {
  
  cnt <- cnt + 1
  url_list[cnt] <- paste0(url_2,
                          "serviceKey=", key,
                          "&farm_id=", farmid[i],
                          "&s_month=", getMonth,
                          "&s_week=", getWeek)
  Sys.sleep(0.1)
  msg <- paste0("[", i, "/", 30, "]  ", farmid[i], "-", getMonth, "month", "-", getWeek, "week", "의 크롤링 목록이 생성됨")
  cat(msg, "\n\n")
}

head(url_list, 20)



## json parsing
finalTotalData <- data.frame()
res <- list()

for (i in 1:length(url_list)){
  {
    res <- GET(url_list[[i]])
    res |> 
      content(as='text', encoding = 'UTF-8') |> 
      fromJSON() -> citrusjson
    
    doc <- unlist(citrusjson$result)
    
    citrus = doc[grepl("citrus_id", names(doc))]
    id = doc[grepl("farm_id", names(doc))]
    month = doc[grepl("researched_month", names(doc))]
    week = doc[grepl("researched_week", names(doc))]
    at = doc[grepl("researched_at", names(doc))]
    tag = doc[grepl("tag_no", names(doc))]
    brix = doc[grepl("brix", names(doc))]
    size = doc[grepl("size", names(doc))]
    
    citrus_df <- tibble(citrus_id = citrus,
                        farm_id = id,
                        month = month,
                        week = week,
                        research_day = at,
                        tag = tag, 
                        brix = brix,
                        size = size)
    
    farm_nm <- farmid[i]
    month_nm <- getMonth
    week_nm <- getWeek
    path <- as.character(paste0("./08_Citrus_farm_list/", farm_nm, "-", month_nm, "month", "-", week_nm, "week", ".csv"))
    write.csv(citrus_df, path)
    
    msg <- paste0("[", i, "/", length(url_list), "] 데이터를 수집합니다")
    cat(msg, "\n\n")
  }
}



## Saving files
files <- dir("./08_Citrus_farm_list")
library(plyr)             
citrus_fruit <- ldply(as.list(paste0("./08_Citrus_farm_list/", files)), read.csv) 
head(citrus_fruit, 2)  
tail(citrus_fruit, 2)

save(citrus_fruit, file = "./08_Citrus_farm_list/citrus_fruit_data.rdata") 
write.csv(citrus_fruit, "./08_Citrus_farm_list/citrus_fruit_data.csv")  

## Merge two files (Citrus farm list and fruit data)
citrus_fruit <- read.csv("./08_Citrus_farm_list/citrus_fruit_data_UTF8.csv", encoding="UTF-8")
citrus_geo <- read.csv("./08_Citrus_farm_list/citrus_farm.csv", encoding="UTF-8")
citrus_geo

citrus_geodata <- merge(x=citrus_geo, y=citrus_fruit, by = 'farm_id')
View(citrus_geodata)
library(dplyr)
citrus_geodata <- citrus_geodata |> select(-X)

save(citrus_geodata, file = "./08_Citrus_farm_list/citrus_farm_fruit_geodata.rdata")
write.csv(citrus_geodata, "./08_Citrus_farm_list/citrus_farm_fruit_geodata.csv")

## We used the above method to get Citrus fruit, weather, and agricultural practice data.


## Map visualizing Citrus farm on Jeju Island
library(sf)
citrus_sf <- st_as_sf(citrus_geodata, coords=c("longitude", "latitude"), crs=4326)
citrus_sf

st_write(
  citrus_sf,
  dsn = "Citrus_farm_list",
  layer = "citrus_farm",
  driver = "ESRI Shapefile",
  append = FALSE
)

saveRDS(citrus_sf, "./08_Citrus_farm_list/citrus_farm.rds")

st_write(citrus_sf, dsn = "./08_Citrus_farm_list/citrus_farm.gpkg", append = FALSE)


library(tmap)
jeju <- st_read("./Jeju_shp/jeju.shp")

ggplot() +
  geom_sf(data = jeju) +
  geom_sf(data = citrus_sf)+
  theme_bw()







