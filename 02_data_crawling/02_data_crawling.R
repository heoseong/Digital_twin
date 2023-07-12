## Data crawling

rm(list=ls())

options(scipen=999)
loc <- read.csv("Jeju_Hanrim_1.csv", fileEncoding="CP949", encoding="UTF-8")  #Perform iteration in this order
# loc <- read.csv("Jeju_Aewol_2.csv", fileEncoding="CP949", encoding="UTF-8")
# loc <- read.csv("Jeju_Gujwa_3.csv", fileEncoding="CP949", encoding="UTF-8")
# loc <- read.csv("Jeju_Jocheon_4.csv", fileEncoding="CP949", encoding="UTF-8")
# loc <- read.csv("Jeju_Hankyeong_5.csv", fileEncoding="CP949", encoding="UTF-8")
# loc <- read.csv("Jeju_Chuja_6.csv", fileEncoding="CP949", encoding="UTF-8")
# loc <- read.csv("Jeju_Udo_7.csv", fileEncoding="CP949", encoding="UTF-8")
# Create two separate files for Jeju and Seogwipo and combine them in file saving section below.
# loc <- read.csv("Seogwipo_Daejeong_1.csv", fileEncoding="CP949", encoding="UTF-8")
# loc <- read.csv("Seogwipo_Namwon_2.csv", fileEncoding="CP949", encoding="UTF-8")
# loc <- read.csv("Seogwipo_Seongsan_3.csv", fileEncoding="CP949", encoding="UTF-8")
# loc <- read.csv("Seogwipo_Andeok_4.csv", fileEncoding="CP949", encoding="UTF-8")
# loc <- read.csv("Seogwipo_Pyoseon_5.csv", fileEncoding="CP949", encoding="UTF-8")

loc$code <- as.character(loc$code)   
head(loc, 2)

Page_Size <- 200
Page_No <- 1

service_key <- "your personal authentication key"
#You need to sign up for the Open Data Portal to get your personal API authentication key.
#Daily data traffic is fixed for each data, so it takes a lot of time to collect data.

url_list <- list() 
cnt <-0	           



for(i in 1:nrow(loc)){         
  
  cnt <- cnt + 1              
 
  url_list[cnt] <- paste0("http://apis.data.go.kr/1390802/SoilEnviron/SoilExam/getSoilExamList?",
                          "serviceKey=", service_key,
                          "&BJD_Code=", loc[i,1],       
                          "&Page_No=", 1,    
                          "&Page_Size=", 200           
  )  
  
  Sys.sleep(0.1)  
  msg <- paste0("[", i,"/",nrow(loc), "]  ", loc[i,3], " 의 크롤링 목록이 생성됨 => 총 [", cnt,"] 건")
  cat(msg, "\n\n") 
}


length(url_list)               
browseURL(paste0(url_list[1])) 


url_list <- url_list[-1]
url_list <- url_list[(1:77)]

head(url_list)


library(XML)          
library(data.table) 
library(stringr)   

raw_data <- list()      
root_Node <- list()      
total <- list()          
dir.create("02_raw_data")



for(i in 1:length(url_list)){   
  tryCatch(
    {  
      raw_data[[i]] <- xmlTreeParse(url_list[i], useInternalNodes = TRUE,encoding = "utf-8")
      root_Node[[i]] <- xmlRoot(raw_data[[i]])	
      
      items <- root_Node[[i]][[2]][['items']]  
      size <- xmlSize(items)                      
 

      item <- list()  
      item_temp_dt <- data.table()  
      Sys.sleep(.1) 
      for(m in 1:size){  
        
        item_temp <- xmlSApply(items[[m]],xmlValue)
        item_temp_dt <- data.table(BJD_Code = item_temp[2],      
                                   year = item_temp[3],    
                                   day = item_temp[4],      
                                   type = item_temp[5],    
                                   acid = item_temp[7],    
                                   phosphate = item_temp[8], 
                                   silicate = item_temp[9],   
                                   OM = item_temp[10],  
                                   Mg = item_temp[11],      
                                   K = item_temp[12],    
                                   Ca = item_temp[13],
                                   EC = item_temp[14],
                                   jibun = item_temp[6]
        ) 
        item[[m]] <- item_temp_dt}   
      apt_bind <- rbindlist(item)     
      
     
      region_nm <- subset(loc, code== str_sub(url_list[i],190, 199))$addr_2 #The numbers should change depending on the length of your authentication key.

      
      
      path <- as.character(paste0("./02_raw_data/", region_nm, ".csv"))
      write.csv(apt_bind, path)     
      msg <- paste0("[", i,"/",length(url_list), "] 수집한 데이터를 [", path,"]에 저장 합니다.") 
      cat(msg, "\n\n")
    }
  )
}   


# File integration
files <- dir("./02_raw_data")   

library(plyr)             
library(readr)
apt_price <- ldply(as.list(paste0("./02_raw_data/", files)), read.csv)
#apt_price <- ldply(as.list(paste0("./02_raw_data/", files)), read.csv, fileEncoding = "CP949", encoding = "UTF-8")

head(apt_price, 2)
tail(apt_price, 2)

# File saving
dir.create("../03_integrated_total_jeju")
save(apt_price, file = "../03_integrated_total_jeju/03_jeju_data.rdata")
write.csv(apt_price, "../03_integrated_total_jeju/03_jeju_data.csv") 
#write.csv(apt_price, "../03_integrated_total_jeju/03_seogwipo_data.csv")   

# After editing the file, it was saved as Jejusi.csv and Seogwiposi.csv in the integrated folder.
# And then, two files were merged into Jejudo_total_UTF8.csv or 03_jeju_data.rdata.
files <- dir("../03_integrated_total_jeju/integrated/") 
totalsoil <- ldply(as.list(paste0("../03_integrated_total_jeju/integrated/", files)), read.csv, fileEncoding = "CP949", encoding = "UTF-8")
write.csv(totalsoil, "../03_integrated_total_jeju/integrated/Jejudo_total_UTF8.csv", fileEncoding="UTF-8")
apt_price <- read.csv("../03_integrated_total_jeju/integrated/Jejudo_total_UTF8.csv", fileEncoding="UTF-8")
head(apt_price)
tail(apt_price)
save(apt_price, file = "./03_integrated_total_jeju/03_jeju_data.rdata")

