library(xlsx)
library(dplyr)
library(stringr)
library(reshape2)
library(rio)
library(ggplot2)

Base_YQ = 10701
End_YQ = 10703	
alldata_base_1 = readRDS("./rds/ALLDATA_BASE_1.rds")
alldata_base_2 = readRDS("./rds/ALLDATA_BASE_2.rds")
SY_data = readRDS("./rds/SY_data.rds")
YC_data = readRDS("./rds/YC_data.rds")
CLB_USE = readRDS("./rds/CLB_USE.rds")
COUNTY_OLD = readRDS("./rds/County_old.rds")

##-----計算每季交易量(新縣市)-----
## 台北A、新北F、桃園H、台中B、台南D、高雄E、新竹市O、基隆C、宜蘭G
## 公寓2、大樓3.4、套房5、透天6
usedata_0 = filter(alldata_base_1,
                     (TRADEDATE_YQF>=Base_YQ
                      &TRADEDATE_YQF<=End_YQ
                      &(COUNTY_F=="A" | COUNTY_F=="F" | COUNTY_F=="H" 
                        | COUNTY_F=="B" | COUNTY_F=="D" | COUNTY_F=="E"
                        | COUNTY_F=="O" | COUNTY_F=="C" | COUNTY_F=="G"))) 
usedata_0_out = filter(alldata_base_1,
                         !(TRADEDATE_YQF>=Base_YQ
                           &TRADEDATE_YQF<=End_YQ
                           &(COUNTY_F=="A" | COUNTY_F=="F" | COUNTY_F=="H" 
                             | COUNTY_F=="B" | COUNTY_F=="D" | COUNTY_F=="E"
                             | COUNTY_F=="O" | COUNTY_F=="C" | COUNTY_F=="G"))) %>%
                  arrange(COUNTY_F,LOCATION_NO,BDTYPE_G)
##dealcount_c_detail
dealcount_c_detail = select(usedata_0,COUNTY_F,COUNTY,LOCATION,TRADEDATE_YQ,BDTYPE_K)
dealcount_c_detail$N = 1
dealcount_c_detail = aggregate(N ~., dealcount_c_detail, FUN = length,na.action = na.omit)%>%
                     arrange(COUNTY_F,COUNTY,LOCATION,BDTYPE_K,TRADEDATE_YQ)
##----匯出資料----
saveRDS(dealcount_c_detail,"C:/Users/Eric/Desktop/SARA/rds/dealcount_c_detail.rds")
export(dealcount_c_detail,"C:/Users/Eric/Desktop/SARA/M50/output_r_excel/dealcount_c_detail.xlsx")

##dealcount_c_table
dealcount_c_table = select(usedata_0,COUNTY_F,COUNTY,TRADEDATE_YQ)
dealcount_c_table$N = 1
dealcount_c_table = aggregate(N ~., dealcount_c_table, FUN = length,na.action = na.omit)%>%
                    arrange(COUNTY_F,COUNTY,TRADEDATE_YQ)
dealcount_c_table$TRADEDATE_YQC = paste("YQC",dealcount_c_table$TRADEDATE_YQ,sep ="" )
dealcount_c_table = select(dealcount_c_table,COUNTY_F,COUNTY,TRADEDATE_YQC,N)

dealcount_c_table = reshape(dealcount_c_table,v.names = c("N")
                             ,idvar = c(1:2),timevar = c("TRADEDATE_YQC")
                             ,direction = "wide", varying = NULL,sep = ".")%>%
                    arrange(COUNTY_F)
names(dealcount_c_table)[names(dealcount_c_table)=="N.YQC10701"]="YQC10701"
names(dealcount_c_table)[names(dealcount_c_table)=="N.YQC10702"]="YQC10702"
names(dealcount_c_table)[names(dealcount_c_table)=="N.YQC10703"]="YQC10703"
##----匯出資料----
saveRDS(dealcount_c_table,"C:/Users/Eric/Desktop/SARA/rds/dealcount_c_table.rds")
export(dealcount_c_table,"C:/Users/Eric/Desktop/SARA/M50/output_r_excel/dealcount_c_table.xlsx")

##----- 計算房價指數 (新縣市) -----
## 台北A、新北F、桃園H、台中B、台南D、高雄E、新竹市O、基隆C、宜蘭G
## 公寓A、大樓B、透天C

usedata_1 = filter(alldata_base_2,
                   ((TRADEDATE_YQF>=Base_YQ)
                    &(TRADEDATE_YQF<=End_YQ)
                    &(BDTYPE_C=="A" | BDTYPE_C=="B" | BDTYPE_C=="C")
                    &(COUNTY_F=="A" | COUNTY_F=="F" | COUNTY_F=="H" 
                      | COUNTY_F=="B" | COUNTY_F=="D" | COUNTY_F=="E"
                      | COUNTY_F=="O" | COUNTY_F=="C" | COUNTY_F=="G"))) 
usedata_1_OUT = filter(alldata_base_2,
                       !((TRADEDATE_YQF>=Base_YQ)
                        &(TRADEDATE_YQF<=End_YQ)
                        &(BDTYPE_C=="A" | BDTYPE_C=="B" | BDTYPE_C=="C")
                        &(COUNTY_F=="A" | COUNTY_F=="F" | COUNTY_F=="H" 
                          | COUNTY_F=="B" | COUNTY_F=="D" | COUNTY_F=="E"
                          | COUNTY_F=="O" | COUNTY_F=="C" | COUNTY_F=="G"))) 

##---------------- 樣本篩選 ----------------

##(D2)特殊關係交易##
usedata_2 = select(usedata_1,1:67)
usedata_2$D2 = ifelse(( (grepl("親", usedata_2$OTHER)) | (grepl("夫", usedata_2$OTHER))
                      | (grepl("妻", usedata_2$OTHER)) | (grepl("配偶", usedata_2$OTHER))
                      | (grepl("婆", usedata_2$OTHER)) | (grepl("媳", usedata_2$OTHER))
                      | (grepl("叔", usedata_2$OTHER)) | (grepl("侄", usedata_2$OTHER))
                      | (grepl("姪", usedata_2$OTHER)) | (grepl("姑", usedata_2$OTHER))
                      | (grepl("祖", usedata_2$OTHER)) | (grepl("孫", usedata_2$OTHER))
                      | (grepl("父", usedata_2$OTHER)) | (grepl("母", usedata_2$OTHER))
                      | (grepl("子", usedata_2$OTHER)) | (grepl("女", usedata_2$OTHER))
                      | (grepl("兒", usedata_2$OTHER)) | (grepl("哥", usedata_2$OTHER))
                      | (grepl("兄", usedata_2$OTHER)) | (grepl("姊", usedata_2$OTHER))
                      | (grepl("姐", usedata_2$OTHER)) | (grepl("弟", usedata_2$OTHER))
                      | (grepl("妹", usedata_2$OTHER)) | (grepl("友", usedata_2$OTHER))
                      | (grepl("員工", usedata_2$OTHER)) | (grepl("關係", usedata_2$OTHER))
                      | (grepl("特殊交易", usedata_2$OTHER))) == T ,1,0)
##(D4)非都市用地##
usedata_2$D4 = ifelse((usedata_2$USETYPE=="") & (usedata_2$RURALTYPE!= ""),1,0)
##(D5)預售屋##
usedata_2$D5 = ifelse(((usedata_2$PRESALE == "1") | (usedata_2$PRESALE == "2")),1,0)

##(D6)使用分區或編定##
##usedata_2$D6 = ifelse((usedata_2$USETYPE == "其他"),1,0)

##(D7)主要用途##
##usedata_2$D7 = ifelse((usedata_2$MAINUSE == "辦公室"),1,0)

##(D8)屋齡大於(含)50年##
usedata_2$D8 = ifelse(((!is.na(usedata_2$AGE_Y)) & (usedata_2$AGE_Y >= 50)),1,0)

##(D9)屋齡小於(不含)2年##
usedata_2$D9 = ifelse(((!is.na(usedata_2$AGE_Y)) & (usedata_2$AGE_Y < 2)),1,0)

usedata_2$OUT = (usedata_2$D2+usedata_2$D4+usedata_2$D5
                    ##+usedata_2$D6+usedata_2$D7
                 +usedata_2$D8+usedata_2$D9)

usedata_2$BMPRICE = ifelse((usedata_2$BUILDPRICE ==0),usedata_2$MIXPRICE,usedata_2$BUILDPRICE)


           
usedata_4 = filter(usedata_2,OUT==0) %>%
            arrange(TRANSFLOOR)
usedata_4_out = filter(usedata_2,OUT!=0)
##--------(D3)排除特殊交易樓層---------
usedata_4$TRANSFLOOR_BAK = usedata_4$TRANSFLOOR
usedata_4$LL = nchar(usedata_4$TRANSFLOOR)
usedata_4$K = str_count(usedata_4$TRANSFLOOR_BAK,c("，"))
usedata_4$KK = regexpr("，",usedata_4$TRANSFLOOR_BAK)
usedata_4$KC = usedata_4$K

usedata_4 = select(usedata_4
                   ,ID,IMPORT,IMPORT_F,TRADEDATE,TRADEDATE_F
                   ,TRADEDATE_YM,TRADEDATE_YMF,TRADEDATE_YQ,TRADEDATE_YQF,BDFNDATE,
                   BDFNDATE_F,BDFNDATE_YM,BDFNDATE_YMF,BDFNDATE_YQ,BDFNDATE_YQF,
                   AGE_Y,AGE_C,PRESALE,COUNTY_F,COUNTY,
                   COUNTY_OLD_F,COUNTY_OLD,LOCATION_NO,LOCATION,ADDRESS,
                   ADDRESS_C,TRADETARGET,BDTYPE,BDTYPE_F,BDTYPE_C,
                   BDTYPE_G,BDTYPE_K,MAINUSE,USETYPE,MAINMT,
                   TOLFLOOR,TRANSFLOOR,TRADEBDNUM,LAND,BUILD,
                   PARK,ROOM,HALL,TOILET,STRUCTURE,
                   MANAGEMENT,PARKTYPE,PARKTYPE_F,MOTO,RURALTYPE,
                   RURALTYPE_DT,LANDAREA,BDAREA,PARKAREA,TOLPRICE,
                   TOLPARKPRICE,PRICE,BUILDPRICE,MIXPRICE,PARKPRICE,
                   OTHER,YCB,YCT,YCC,YCN,
                   SYC,SYN,D2,D4,
                   D5,D8,D9,OUT,BMPRICE,
                   TRANSFLOOR_BAK,LL,K,KK,KC) %>% 
            arrange(K)

usedata_5_check = filter(usedata_4,K=="0")
usedata_5 = filter(usedata_4,K!="0")
usedata_5$SS = 0
usedata_5_check$SS = 0

a = str_split(usedata_5$TRANSFLOOR,"[，]")
usedata_5$TRANSFLOOR1 = sapply(a, "[", 1)
usedata_5$TRANSFLOOR2 = sapply(a, "[", 2)
usedata_5$TRANSFLOOR3 = sapply(a, "[", 3)
usedata_5$TRANSFLOOR4 = sapply(a, "[", 4)
usedata_5$TRANSFLOOR5 = sapply(a, "[", 5)
usedata_5$TRANSFLOOR6 = sapply(a, "[", 6)
usedata_5$TRANSFLOOR7 = sapply(a, "[", 7)
usedata_5$TRANSFLOOR8 = sapply(a, "[", 8)
usedata_5$TRANSFLOOR9 = sapply(a, "[", 9)
usedata_5$TRANSFLOOR10 = sapply(a, "[", 10)
usedata_5$TRANSFLOOR11 = sapply(a, "[", 11)
usedata_5$TRANSFLOOR12 = sapply(a, "[", 12)
usedata_5$TRANSFLOOR13 = sapply(a, "[", 13)
usedata_5$TRANSFLOOR14 = sapply(a, "[", 14)
usedata_5$TRANSFLOOR15 = sapply(a, "[", 15)

TRANSFLOOR1 = select(usedata_5,1:80,TRANSFLOOR1)
TRANSFLOOR2 = select(usedata_5,1:80,TRANSFLOOR2)
TRANSFLOOR3 = select(usedata_5,1:80,TRANSFLOOR3)
TRANSFLOOR4 = select(usedata_5,1:80,TRANSFLOOR4)
TRANSFLOOR5 = select(usedata_5,1:80,TRANSFLOOR5)
TRANSFLOOR6 = select(usedata_5,1:80,TRANSFLOOR6)
TRANSFLOOR7 = select(usedata_5,1:80,TRANSFLOOR7)
TRANSFLOOR8  = select(usedata_5,1:80,TRANSFLOOR8)
TRANSFLOOR9  = select(usedata_5,1:80,TRANSFLOOR9)
TRANSFLOOR10 = select(usedata_5,1:80,TRANSFLOOR10)
TRANSFLOOR11 = select(usedata_5,1:80,TRANSFLOOR11)
TRANSFLOOR12 = select(usedata_5,1:80,TRANSFLOOR12)
TRANSFLOOR13 = select(usedata_5,1:80,TRANSFLOOR13)
TRANSFLOOR14 = select(usedata_5,1:80,TRANSFLOOR14)
TRANSFLOOR15 = select(usedata_5,1:80,TRANSFLOOR15)
colnames(TRANSFLOOR1) = c(1:80,"TRANSFLOOR1")
colnames(TRANSFLOOR2) = c(1:80,"TRANSFLOOR1")
colnames(TRANSFLOOR3) = c(1:80,"TRANSFLOOR1")
colnames(TRANSFLOOR4) = c(1:80,"TRANSFLOOR1")
colnames(TRANSFLOOR5) = c(1:80,"TRANSFLOOR1")
colnames(TRANSFLOOR6) = c(1:80,"TRANSFLOOR1")
colnames(TRANSFLOOR7) = c(1:80,"TRANSFLOOR1")
colnames(TRANSFLOOR8 ) = c(1:80,"TRANSFLOOR1")
colnames(TRANSFLOOR9 ) = c(1:80,"TRANSFLOOR1")
colnames(TRANSFLOOR10) = c(1:80,"TRANSFLOOR1")
colnames(TRANSFLOOR11) = c(1:80,"TRANSFLOOR1")
colnames(TRANSFLOOR12) = c(1:80,"TRANSFLOOR1")
colnames(TRANSFLOOR13) = c(1:80,"TRANSFLOOR1")
colnames(TRANSFLOOR14) = c(1:80,"TRANSFLOOR1")
colnames(TRANSFLOOR15) = c(1:80,"TRANSFLOOR1")

usedata_5 = rbind(TRANSFLOOR1,TRANSFLOOR2,TRANSFLOOR3,TRANSFLOOR4,TRANSFLOOR5,TRANSFLOOR6,TRANSFLOOR7
                 ,TRANSFLOOR8,TRANSFLOOR9,TRANSFLOOR10,TRANSFLOOR11,TRANSFLOOR12,TRANSFLOOR13,TRANSFLOOR14,TRANSFLOOR15)
usedata_5 = filter(usedata_5,!(is.na(TRANSFLOOR1)))
usedata_5 = select(usedata_5,c(1:36,TRANSFLOOR1,38:80))
colnames(usedata_5) = c("ID","IMPORT","IMPORT_F","TRADEDATE","TRADEDATE_F","TRADEDATE_YM","TRADEDATE_YMF","TRADEDATE_YQ","TRADEDATE_YQF","BDFNDATE"
                        ,"BDFNDATE_F","BDFNDATE_YM","BDFNDATE_YMF","BDFNDATE_YQ","BDFNDATE_YQF","AGE_Y","AGE_C","PRESALE","COUNTY_F","COUNTY"
                        ,"COUNTY_OLD_F","COUNTY_OLD","LOCATION_NO","LOCATION","ADDRESS","ADDRESS_C","TRADETARGET","BDTYPE","BDTYPE_F","BDTYPE_C"
                        ,"BDTYPE_G","BDTYPE_K","MAINUSE","USETYPE","MAINMT","TOLFLOOR","TRANSFLOOR","TRADEBDNUM","LAND","BUILD"
                        ,"PARK","ROOM","HALL","TOILET","STRUCTURE","MANAGEMENT","PARKTYPE","PARKTYPE_F","MOTO","RURALTYPE"
                        ,"RURALTYPE_DT","LANDAREA","BDAREA","PARKAREA","TOLPRICE","TOLPARKPRICE","PRICE","BUILDPRICE","MIXPRICE","PARKPRICE"
                        ,"OTHER","YCB","YCT","YCC","YCN","SYC","SYN","D2","D4"
                        ,"D5","D8","D9","OUT","BMPRICE","TRANSFLOOR_BAK","LL","K","KK","KC","SS")
usedata_5$K = usedata_5$K-15
usedata_5$KC = ifelse(usedata_5$K<=0,0,usedata_5$K)
usedata_5_check = rbind(usedata_5,usedata_5_check) %>%
                  arrange(KC,ID,SS)

usedata_6 = select(usedata_5_check,1:72,74:75)

usedata_7_out = filter(usedata_6,
                     (((grepl("一", usedata_6$TRANSFLOOR)==F) & (grepl("二", usedata_6$TRANSFLOOR)==F)
                     & (grepl("三", usedata_6$TRANSFLOOR)==F) & (grepl("四", usedata_6$TRANSFLOOR)==F)
                     & (grepl("五", usedata_6$TRANSFLOOR)==F) & (grepl("六", usedata_6$TRANSFLOOR)==F)
                     & (grepl("七", usedata_6$TRANSFLOOR)==F) & (grepl("八", usedata_6$TRANSFLOOR)==F)
                     & (grepl("九", usedata_6$TRANSFLOOR)==F) & (grepl("十", usedata_6$TRANSFLOOR)==F))
                     & TRANSFLOOR != "全" & TRANSFLOOR != "" )
                     | (grepl("地下", usedata_6$TRANSFLOOR)==T)
                     | TRANSFLOOR == "一層" | TOLFLOOR == "一層")
         
usedata_7 = filter(usedata_6,
                   !((((grepl("一", usedata_6$TRANSFLOOR)==F) & (grepl("二", usedata_6$TRANSFLOOR)==F)
                     & (grepl("三", usedata_6$TRANSFLOOR)==F) & (grepl("四", usedata_6$TRANSFLOOR)==F)
                     & (grepl("五", usedata_6$TRANSFLOOR)==F) & (grepl("六", usedata_6$TRANSFLOOR)==F)
                     & (grepl("七", usedata_6$TRANSFLOOR)==F) & (grepl("八", usedata_6$TRANSFLOOR)==F)
                     & (grepl("九", usedata_6$TRANSFLOOR)==F) & (grepl("十", usedata_6$TRANSFLOOR)==F))
                     & TRANSFLOOR != "全" & TRANSFLOOR != "" )
                     | (grepl("地下", usedata_6$TRANSFLOOR)==T)
                     | TRANSFLOOR == "一層" | TOLFLOOR == "一層"))
usedata_7$TRANSFLOOR = usedata_7$TRANSFLOOR_BAK 
usedata_7 = select(usedata_7,1:74)
usedata_7 = arrange_all(usedata_7)
usedata_7 = usedata_7[!duplicated(usedata_7),]

usedata_7_out$TRANSFLOOR = usedata_7_out$TRANSFLOOR_BAK 
usedata_7_out = select(usedata_7_out,1:74)
usedata_7_out = arrange_all(usedata_7_out)
usedata_7_out = usedata_7_out[!duplicated(usedata_7_out),]

usedata_7_check = select(usedata_7,ID)
usedata_7_check$REP = 1
usedata_7_check = usedata_7_check[!duplicated(usedata_7_check),]

usedata_7_out = merge(usedata_7_out, usedata_7_check ,by = "ID" , all.x= T) 

usedata_7_out = filter(usedata_7_out, is.na(REP))
##------------(D1)大批交易-------------
##取平均單價：同地址+同建物型態(原分類)+同建築完成年月+同交易年季



usedata_7_c = select(usedata_7,COUNTY_F,LOCATION,ADDRESS,BDTYPE_F,BDFNDATE_YMF,TRADEDATE_YQF,BMPRICE)%>%
              arrange(COUNTY_F,LOCATION,ADDRESS,BDTYPE_F,BDFNDATE_YMF,TRADEDATE_YQF)
usedata_7_c = group_by(usedata_7_c,COUNTY_F,LOCATION,ADDRESS,BDTYPE_F,BDFNDATE_YMF,TRADEDATE_YQF)
usedata_7_c = summarise(usedata_7_c,BMPRICE_mean=mean(BMPRICE))

usedata_8 = merge(usedata_7,usedata_7_c,by = c("COUNTY_F","LOCATION","ADDRESS","BDTYPE_F","BDFNDATE_YMF","TRADEDATE_YQF"),all = T)

usedata_9_0 = usedata_8 %>%arrange(ID)

usedata_9 = usedata_9_0[!duplicated(usedata_9_0[,c("COUNTY_F","LOCATION","ADDRESS","BDTYPE_F","BDFNDATE_YMF","TRADEDATE_YQF")]),]
usedata_9_out = (usedata_9_0[duplicated(usedata_9_0[,c("COUNTY_F","LOCATION","ADDRESS","BDTYPE_F","BDFNDATE_YMF","TRADEDATE_YQF")]),])

  
usedata_10 = select(usedata_9,
                    "ID","IMPORT","IMPORT_F","TRADEDATE","TRADEDATE_F","TRADEDATE_YM","TRADEDATE_YMF",
                    "TRADEDATE_YQ","TRADEDATE_YQF","BDFNDATE","BDFNDATE_F","BDFNDATE_YM","BDFNDATE_YMF","BDFNDATE_YQ",
                    "BDFNDATE_YQF","AGE_Y","AGE_C","PRESALE","COUNTY_F","COUNTY","COUNTY_OLD_F",
                    "COUNTY_OLD","LOCATION_NO","LOCATION","ADDRESS","ADDRESS_C","TRADETARGET","BDTYPE",
                    "BDTYPE_F","BDTYPE_C","BDTYPE_G","BDTYPE_K","MAINUSE","USETYPE","MAINMT",
                    "TOLFLOOR","TRANSFLOOR","TRADEBDNUM","LAND","BUILD","PARK","ROOM",
                    "HALL","TOILET","STRUCTURE","MANAGEMENT","PARKTYPE","PARKTYPE_F","MOTO",
                    "RURALTYPE","RURALTYPE_DT","LANDAREA","BDAREA","PARKAREA","TOLPRICE","TOLPARKPRICE",
                    "PRICE","BUILDPRICE","MIXPRICE","PARKPRICE","OTHER","YCB","YCT",
                    "YCC","YCN","SYC","SYN","BMPRICE","BMPRICE_mean")%>%
             arrange(COUNTY_F,LOCATION,ADDRESS,BDTYPE_F,BDFNDATE_YMF,TRADEDATE_YQF)

usedata_10$BMPRICE = round(usedata_10$BMPRICE_mean,digits = 0)
usedata_10$BMPRICE_Z = usedata_10$BMPRICE
usedata_10 = select(usedata_10,
                    1:68,"BMPRICE_Z")%>%
  arrange(COUNTY_F,LOCATION,ADDRESS,BDTYPE_F,BDFNDATE_YMF,TRADEDATE_YQF)

##----篩選要使用的縣市區域(永慶)-----
CLB_USE_1 = select((filter(CLB_USE,USE=="Y")),COUNTY_F,LOCATION_NO,USE)%>%
          arrange(COUNTY_F,LOCATION_NO)
CLB_USE_1 = CLB_USE_1[!duplicated(CLB_USE_1),]

usedata_11_0 = merge(usedata_10,CLB_USE_1,by = c("COUNTY_F","LOCATION_NO"), all.x = TRUE)
usedata_11 = filter(usedata_11_0,USE=="Y")
usedata_11_out = filter(usedata_11_0,is.na(USE)|is.na(ID))

##----篩選(縣市區擔保品)每季樣本數均>=30----

dealcount_yq = select(usedata_11,COUNTY_F,LOCATION_NO,BDTYPE_C,TRADEDATE_YQ)
dealcount_yq$N = 1
dealcount_yq = aggregate(N ~., dealcount_yq, FUN = length,na.action = na.omit)%>%
               arrange(COUNTY_F,LOCATION_NO,BDTYPE_C,TRADEDATE_YQ)
a = aggregate(N ~ COUNTY_F+LOCATION_NO+BDTYPE_C, dealcount_yq, FUN = min,na.action = na.omit)%>%
  arrange(COUNTY_F,LOCATION_NO,BDTYPE_C)

dealcount_yq = reshape(dealcount_yq,v.names = c("N")
                        ,idvar = c(1:3),timevar = c("TRADEDATE_YQ")
                        ,direction = "wide", varying = NULL,sep = ".")%>%
               arrange(COUNTY_F,LOCATION_NO,BDTYPE_C)
names(dealcount_yq)[names(dealcount_yq)=="N.10701"]="YQA10701"
names(dealcount_yq)[names(dealcount_yq)=="N.10702"]="YQA10702"
names(dealcount_yq)[names(dealcount_yq)=="N.10703"]="YQA10703"

names(a)[names(a)=="N"]="YQA_MIN"

dealcount_yq = merge(dealcount_yq,a,by =c("COUNTY_F","LOCATION_NO","BDTYPE_C"),all = T)
dealcount_yq$OK = ifelse(dealcount_yq$YQA_MIN>=30,1,0)

use_clb = select(dealcount_yq,COUNTY_F,LOCATION_NO,BDTYPE_C,OK)  

usedata_11 = usedata_11%>% arrange(COUNTY_F,LOCATION_NO,BDTYPE_C)
usedata_12_0 = merge(usedata_11,use_clb,by = c("COUNTY_F","LOCATION_NO","BDTYPE_C"),all = T)
usedata_12 = filter(usedata_12_0,OK==1)
usedata_12_out = filter(usedata_12_0,OK!=1)
usedata_ok = select(usedata_12,
                    "ID","IMPORT","IMPORT_F","TRADEDATE","TRADEDATE_F","TRADEDATE_YM","TRADEDATE_YMF",
                    "TRADEDATE_YQ","TRADEDATE_YQF","BDFNDATE","BDFNDATE_F","BDFNDATE_YM","BDFNDATE_YMF","BDFNDATE_YQ",
                    "BDFNDATE_YQF","AGE_Y","AGE_C","PRESALE","COUNTY_F","COUNTY","COUNTY_OLD_F",
                    "COUNTY_OLD","LOCATION_NO","LOCATION","ADDRESS","ADDRESS_C","TRADETARGET","BDTYPE",
                    "BDTYPE_F","BDTYPE_C","BDTYPE_G","BDTYPE_K","MAINUSE","USETYPE","MAINMT",
                    "TOLFLOOR","TRANSFLOOR","TRADEBDNUM","LAND","BUILD","PARK","ROOM",
                    "HALL","TOILET","STRUCTURE","MANAGEMENT","PARKTYPE","PARKTYPE_F","MOTO",
                    "RURALTYPE","RURALTYPE_DT","LANDAREA","BDAREA","PARKAREA","TOLPRICE","TOLPARKPRICE",
                    "PRICE","BUILDPRICE","MIXPRICE","PARKPRICE","OTHER","YCB","YCT",
                    "YCC","YCN","SYC","SYN","BMPRICE","BMPRICE_Z")%>%
             arrange(COUNTY_F,TRADEDATE_YQF)


##---- 主巨集：資料標準化，排除離群值|Z|>=3，繪製分佈圖 ----
##台北 3、台中 3、高雄 3、新北 3、桃園 3、新竹 2.5
result1 = aggregate(usedata_ok$BMPRICE,usedata_ok[,c("COUNTY_F","TRADEDATE_YQF")],scale)%>%
          arrange(COUNTY_F,TRADEDATE_YQF)
result1 = unlist(result1$x)
result1 = as.data.frame(result1)
usedata_nz_c = cbind(usedata_ok,result1)
usedata_nz_c$result1 = ifelse(usedata_nz_c$result1=="NaN",0,usedata_nz_c$result1)
usedata_yz_c  = filter(usedata_nz_c,abs(result1)<=3)
usedata_nz_c$BMPRICE_Z = usedata_nz_c$result1
usedata_yz_c$BMPRICE_Z = usedata_yz_c$result1
##---儲存資料集---
##未標準化
usedata_nz_c = select(usedata_nz_c,1:69)
saveRDS(usedata_nz_c,"C:/Users/Eric/Desktop/SARA/rds/usedata_nz_c.rds")
##已標準化
usedata_yz_c = select(usedata_yz_c,1:69)
saveRDS(usedata_yz_c,"C:/Users/Eric/Desktop/SARA/rds/usedata_yz_c.rds")


##------ 完成實價登錄指數(OP_PPI_C) ------  
T0 = select(usedata_yz_c,1:69)
T0$TRADEDATE_YQK = paste("YQ",T0$TRADEDATE_YQ,sep ="" )

table_c = select(T0,COUNTY_F,COUNTY,TRADEDATE_YQK,BMPRICE)

table_c = do.call(data.frame,aggregate(BMPRICE ~., table_c, FUN = function(x) c(mean=mean(x),n = length(x)),na.action =na.pass))%>%
          arrange(COUNTY_F,COUNTY,TRADEDATE_YQK)

trans1 = select(table_c,1:4)
trans1$BMPRICE.mean = round(trans1$BMPRICE.mean/10000,digits=1)

trans01 = reshape(trans1,v.names = c("BMPRICE.mean")
                  ,idvar = c(1:2),timevar = c("TRADEDATE_YQK")
                  ,direction = "wide",varying = NULL ,sep = ".")
names(trans01)[names(trans01)=="BMPRICE.mean.YQ10701"]="YQM10701"
names(trans01)[names(trans01)=="BMPRICE.mean.YQ10702"]="YQM10702"
names(trans01)[names(trans01)=="BMPRICE.mean.YQ10703"]="YQM10703"


trans2 = select(table_c,1:3,5)
trans02 = reshape(trans2,v.names = c("BMPRICE.n")
                  ,idvar = c(1:2),timevar = c("TRADEDATE_YQK")
                  ,direction = "wide",varying = NULL ,sep = ".")
names(trans02)[names(trans02)=="BMPRICE.n.YQ10701"]="YQN10701"
names(trans02)[names(trans02)=="BMPRICE.n.YQ10702"]="YQN10702"
names(trans02)[names(trans02)=="BMPRICE.n.YQ10703"]="YQN10703"

OP_PPI_c = merge(trans01,trans02, by = c("COUNTY_F","COUNTY"))%>%
           arrange(COUNTY_F,COUNTY)
export(OP_PPI_c,"C:/Users/Eric/Desktop/SARA/M50/output_r_excel/OP_PPI_c.xlsx")
saveRDS(OP_PPI_c,"C:/Users/Eric/Desktop/SARA/rds/OP_PPI_c.rds")

##------- 完成永慶指數(YC_PPI_c) ------

table_yc_c = select(T0,COUNTY_F,COUNTY,TRADEDATE_YQK,YCC)%>%
             arrange(COUNTY_F,COUNTY,TRADEDATE_YQK)
table_yc_c = aggregate(YCC ~., table_yc_c, FUN = mean)%>%
  arrange(COUNTY_F,COUNTY,TRADEDATE_YQK)
table_yc_c$YCC_mean = round(table_yc_c$YCC / 10000,digits = 1)

table_yc_c = select(table_yc_c,1:3,5)
YC_PPI_c = reshape(table_yc_c,v.names = c("YCC_mean")
                  ,idvar = c(1:2),timevar = c("TRADEDATE_YQK")
                  ,direction = "wide",varying = NULL ,sep = ".")
names(YC_PPI_c)[names(YC_PPI_c)=="YCC_mean.YQ10701"]="YCC10701"
names(YC_PPI_c)[names(YC_PPI_c)=="YCC_mean.YQ10702"]="YCC10702"
names(YC_PPI_c)[names(YC_PPI_c)=="YCC_mean.YQ10703"]="YCC10703"

YC_PPI_c = select(YC_PPI_c,1:length(YC_PPI_c))%>%
           arrange(COUNTY_F,COUNTY)
export(YC_PPI_c,"C:/Users/Eric/Desktop/SARA/M50/output_r_excel/YC_PPI_c.xlsx")
saveRDS(YC_PPI_c,"C:/Users/Eric/Desktop/SARA/rds/YC_PPI_c.rds")

##------- 完成信義指數SY_PPI_c) ------

table_sy_c = select(T0,COUNTY_F,COUNTY,TRADEDATE_YQK,SYC)%>%
  arrange(COUNTY_F,COUNTY,TRADEDATE_YQK)
table_sy_c = aggregate(SYC ~., table_sy_c, FUN = mean,na.action =na.pass)%>%
  arrange(COUNTY_F,COUNTY,TRADEDATE_YQK)
table_sy_c$SYC_mean = round(table_sy_c$SYC / 10000,digits = 1)

table_sy_c = select(table_sy_c,1:3,5)
SY_PPI_c = reshape(table_sy_c,v.names = c("SYC_mean")
                   ,idvar = c(1:2),timevar = c("TRADEDATE_YQK")
                   ,direction = "wide",varying = NULL ,sep = ".")
names(SY_PPI_c)[names(SY_PPI_c)=="SYC_mean.YQ10701"]="SYC10701"
names(SY_PPI_c)[names(SY_PPI_c)=="SYC_mean.YQ10702"]="SYC10702"
names(SY_PPI_c)[names(SY_PPI_c)=="SYC_mean.YQ10703"]="SYC10703"

SY_PPI_c = select(SY_PPI_c,1:length(SY_PPI_c))%>%
  arrange(COUNTY_F,COUNTY)
export(SY_PPI_c,"C:/Users/Eric/Desktop/SARA/M50/output_r_excel/SY_PPI_c.xlsx")
saveRDS(SY_PPI_c,"C:/Users/Eric/Desktop/SARA/rds/SY_PPI_c.rds")

##-------完成指數合併檔(PPI_C)------

PPI_c_1 = merge(OP_PPI_c,YC_PPI_c,by = c("COUNTY_F","COUNTY"))
PPI_c_2 = merge(PPI_c_1,SY_PPI_c,by = c("COUNTY_F","COUNTY"))
PPI_c = merge(PPI_c_2,dealcount_c_table,by =c("COUNTY_F","COUNTY"),all.x = TRUE)
PPI_c = select(PPI_c,1:2,15:17,9:14,3:8)
export(PPI_c,"C:/Users/Eric/Desktop/SARA/M50/output_r_excel/PPI_c.xlsx")
saveRDS(PPI_c,"C:/Users/Eric/Desktop/SARA/rds/PPI_c.rds")


##-----單價分布圖-----
tmp = filter(usedata_yz_c,TRADEDATE_YQF==10701|TRADEDATE_YQF==10702
             |TRADEDATE_YQF==10703)
tmp$BMPRICE_W = round(tmp$BMPRICE/10000,digits = 4)
##----COUNTY_F="A"&TRADEYQF----
ggplot(filter(tmp,COUNTY_F=="A"),aes(BMPRICE_W,stat = "count"))+
  geom_histogram(col = "black",
                 fill = "cornflowerblue",
                 size = 0.5)+
  facet_wrap(TRADEDATE_YQF~.,scales = "free")+
  ylab("樣本數 ( 件 )")+
  xlab("單價 ( 萬 / 坪 )")+
  ggtitle("單價分布圖(縣市=A)")

##----COUNTY_F="F"&TRADEYQF----
ggplot(filter(tmp,COUNTY_F=="F"),aes(BMPRICE_W,stat = "count"))+
  geom_histogram(col = "black",
                 fill = "cornflowerblue",
                 size = 0.5)+
  facet_wrap(TRADEDATE_YQF~.,scales = "free")+
  ylab("樣本數 ( 件 )")+
  xlab("單價 ( 萬 / 坪 )")+
  ggtitle("單價分布圖(縣市=F)")

##----COUNTY_F="D"&TRADEYQF----
ggplot(filter(tmp,COUNTY_F=="D"),aes(BMPRICE_W,stat = "count"))+
  geom_histogram(col = "black",
                 fill = "cornflowerblue",
                 size = 0.5)+
  facet_wrap(TRADEDATE_YQF~.,scales = "free")+
  ylab("樣本數 ( 件 )")+
  xlab("單價 ( 萬 / 坪 )")+
  ggtitle("單價分布圖(縣市=D)")

##----COUNTY_F="B"&TRADEYQF----
ggplot(filter(tmp,COUNTY_F=="B"),aes(BMPRICE_W,stat = "count"))+
  geom_histogram(col = "black",
                 fill = "cornflowerblue",
                 size = 0.5)+
  facet_wrap(TRADEDATE_YQF~.,scales = "free")+
  ylab("樣本數 ( 件 )")+
  xlab("單價 ( 萬 / 坪 )")+
  ggtitle("單價分布圖(縣市=B)")

##----COUNTY_F="E"&TRADEYQF----
ggplot(filter(tmp,COUNTY_F=="E"),aes(BMPRICE_W,stat = "count"))+
  geom_histogram(col = "black",
                 fill = "cornflowerblue",
                 size = 0.5)+
  facet_wrap(TRADEDATE_YQF~.,scales = "free")+
  ylab("樣本數 ( 件 )")+
  xlab("單價 ( 萬 / 坪 )")+
  ggtitle("單價分布圖(縣市=E)")

##----COUNTY_F="O"&TRADEYQF----
ggplot(filter(tmp,COUNTY_F=="O"),aes(BMPRICE_W,stat = "count"))+
  geom_histogram(col = "black",
                 fill = "cornflowerblue",
                 size = 0.5)+
  facet_wrap(TRADEDATE_YQF~.,scales = "free")+
  ylab("樣本數 ( 件 )")+
  xlab("單價 ( 萬 / 坪 )")+
  ggtitle("單價分布圖(縣市=O)")

##----COUNTY_F="H"&TRADEYQF----
ggplot(filter(tmp,COUNTY_F=="H"),aes(BMPRICE_W,stat = "count"))+
  geom_histogram(col = "black",
                 fill = "cornflowerblue",
                 size = 0.5)+
  facet_wrap(TRADEDATE_YQF~.,scales = "free")+
  ylab("樣本數 ( 件 )")+
  xlab("單價 ( 萬 / 坪 )")+
  ggtitle("單價分布圖(縣市=H)")

##----COUNTY_F="C"&TRADEYQF----
ggplot(filter(tmp,COUNTY_F=="C"),aes(BMPRICE_W,stat = "count"))+
  geom_histogram(col = "black",
                 fill = "cornflowerblue",
                 size = 0.5)+
  facet_wrap(TRADEDATE_YQF~.,scales = "free")+
  ylab("樣本數 ( 件 )")+
  xlab("單價 ( 萬 / 坪 )")+
  ggtitle("單價分布圖(縣市=C)")

##----COUNTY_F="G"&TRADEYQF----
ggplot(filter(tmp,COUNTY_F=="G"),aes(BMPRICE_W,stat = "count"))+
  geom_histogram(col = "black",
                 fill = "cornflowerblue",
                 size = 0.5,
                 bins = 7)+
  facet_wrap(TRADEDATE_YQF~.,scales = "free_y")+
  scale_x_continuous(c(0,20))+
  ylab("樣本數 ( 件 )")+
  xlab("單價 ( 萬 / 坪 )")+
  ggtitle("單價分布圖(縣市=G)")
##----------- END -----------##
##----------- END -----------##

##--------- 程式結束 ----------

