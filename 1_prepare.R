library(xlsx)
library(dplyr)
Start_YQ =10701

#----------- 匯入舊縣市檔案 -----------
County_old = (read.xlsx2("./M50/County_old.xlsx",1
                        ,header = T ,stringsAsFactors = F))
colnames(County_old) = c("COUNTY_F","COUNTY","LOCATION","COUNTY_OLD","COUNTY_OLD_F")
County_old = select(County_old,COUNTY,COUNTY_F,COUNTY_OLD,COUNTY_OLD_F,LOCATION)

saveRDS(County_old,"C:/Users/Eric/Desktop/SARA/rds/COUNTY_OLD.rds")

#-- 匯入要使用的縣市區和建物型態明細 --
CLB_USE = read.xlsx2("./M50/CLB_USE.xlsx",1,header = T 
                     ,colClasses = c("character","character","character","numeric","character","character")
                     ,stringsAsFactors= F)
colnames(CLB_USE) = c("COUNTY_F","COUNTY","LOCATION","LOCATION_NO","BDTYPE_G","USE")
CLB_USE = select(CLB_USE,BDTYPE_G,USE,COUNTY,COUNTY_F,LOCATION,LOCATION_NO)

saveRDS(CLB_USE,"C:/Users/Eric/Desktop/SARA/rds/CLB_USE.rds")

#------------ 匯入永慶檔案 ------------
###Start_YQ =10701
YC_data = read.xlsx2("./M50/YC_data.xlsx",3 
                     ,colClasses = c("character","character","character","character","character"
                                     ,"numeric","character","numeric","numeric","numeric","numeric","numeric")
                     ,stringsAsFactors= F) 
colnames(YC_data) = c("COUNTY_F","COUNTY","COUNTY_OLD_F","COUNTY_OLD","LOCATION","LOCATION_NO","BDTYPE_G","TRADEDATE_YQF","YCB","YCT","YCC","YCN")
YC_data = mutate(YC_data
                ,YCB = (round(YC_data$YCB,1))*10000
                ,YCC = (round(YC_data$YCC,1))*10000
                ,YCN = (round(YC_data$YCN,1))*10000
                ,YCT = (round(YC_data$YCT,1))*10000)
YC_data = select(filter(YC_data,TRADEDATE_YQF >= Start_YQ),BDTYPE_G,TRADEDATE_YQF,YCB,YCC,YCN,YCT,COUNTY,COUNTY_F,COUNTY_OLD,COUNTY_OLD_F,LOCATION,LOCATION_NO)

saveRDS(YC_data,"C:/Users/Eric/Desktop/SARA/rds/YC_DATA.rds")

#------------ 匯入信義檔案 ------------
###Start_YQ =10701
SY_data = read.xlsx2("./M50/SY_data.xlsx",2 
                     ,colClasses = c("character","character","numeric","numeric","numeric")
                     ,stringsAsFactors= F)
colnames(SY_data) = c("COUNTY_F","COUNTY","TRADEDATE_YQF","SYC","SYN")

SY_data = mutate(SY_data
                 ,SYC = (round(SY_data$SYC,1))*10000
                 ,SYN = (round(SY_data$SYN,1))*10000)

SY_data = select(filter(SY_data,TRADEDATE_YQF >= Start_YQ),SYC,SYN,TRADEDATE_YQF,COUNTY,COUNTY_F)

saveRDS(SY_data,"C:/Users/Eric/Desktop/SARA/rds/SY_DATA.rds")                 


#---------------end---------------


                                     