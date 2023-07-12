##05_geocoding

rm(list=ls())
load("../04_preprocess/04_preprocess_Jeju.rdata")
head(toyang$jibun)
apt_juso <- data.frame(toyang$jibun)
apt_juso <- data.frame(apt_juso[!duplicated(apt_juso), ])
head(apt_juso, 2)
apt_juso[1, 1]
add_list <- list()
cnt <- 0
kakao_key = "your REST API key" 
#You need to sign up for the Kakao developers to get your personal REST API authentication key.
write.csv(apt_juso, "./04_preprocess/04_Jeju_juso.csv")

library(httr)
library(RJSONIO)
library(data.table)
library(dplyr)

for(i in 1:nrow(apt_juso)){
  
  tryCatch(
    {
      lon_lat <- GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
                     query = list(query = apt_juso[i, ]),
                     add_headers(Authorization = paste0("KakaoAK ", kakao_key)))
      coordxy <- lon_lat %>% content(as = 'text') %>% fromJSON()
      cnt = cnt + 1
      add_list[[cnt]] <- data.table(apt_juso = apt_juso[i, ],
                                    coord_x = coordxy$documents[[1]]$x,
                                    coord_y = coordxy$documents[[1]]$y)
      message <- paste0("[", i, "/", nrow(apt_juso), "] 번째 (",
                        round(i/nrow(apt_juso)*100, 2), " %) [", apt_juso[i, ] , "] 지오 코딩 중입니다:
                        X= ", add_list[[cnt]]$coord_x, " / Y= ", add_list[[cnt]]$coord_y)
      cat(message, "\n\n")
    }, error=function(e){cat("ERROR :", conditionMessage(e), "\n")}
  )
}

juso_geocoding <- rbindlist(add_list)
juso_geocoding$coord_x <- as.numeric(juso_geocoding$coord_x)
juso_geocoding$coord_y <- as.numeric(juso_geocoding$coord_y)
juso_geocoding <- na.omit(juso_geocoding)
View(juso_geocoding)
dir.create("./05_geocoding")
save(juso_geocoding, file="./05_geocoding/05_Jeju_geocoding.rdata")
write.csv(juso_geocoding, "./05_geocoding/05_Jeju_geocoding.csv")
head(juso_geocoding)
