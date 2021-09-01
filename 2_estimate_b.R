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

##-----計算每季交易量-----
##篩選資料區間(年季)
usedata_0_1 = filter(alldata_base_1,
                     (TRADEDATE_YQF>=Base_YQ
                     &TRADEDATE_YQF<=End_YQ)) %>%
              arrange(COUNTY_F,LOCATION_NO,BDTYPE_G)

usedata_0_1_out = filter(alldata_base_1,
                         !(TRADEDATE_YQF>=Base_YQ
                         &TRADEDATE_YQF<=End_YQ)) %>%
                  arrange(COUNTY_F,LOCATION_NO,BDTYPE_G)

CLB_USE_1 = select(filter(CLB_USE,USE=="Y"),COUNTY_F,LOCATION_NO,BDTYPE_G,USE)%>%
            arrange(COUNTY_F,LOCATION_NO,BDTYPE_G)

usedata_1_0 = merge(usedata_0_1,CLB_USE_1,by = c("COUNTY_F","LOCATION_NO","BDTYPE_G"),all = T)
usedata_1 = filter(usedata_1_0,USE=="Y"&ID!="")
usedata_1_out = filter(usedata_1_0,is.na(USE)|is.na(ID))

##------交易量統計表(永慶_擔保品)-------
dealcount_b_table = select(usedata_1,COUNTY_F,COUNTY,LOCATION,LOCATION_NO,BDTYPE_G,BDTYPE_C,TRADEDATE_YQ)
dealcount_b_table$YQC = 1

dealcount_b_table = aggregate(YQC ~., dealcount_b_table, FUN = sum,na.action = na.omit) %>%
                    arrange(LOCATION_NO,BDTYPE_C,TRADEDATE_YQ)

dealcount_b_table  = reshape(dealcount_b_table,v.names = c("YQC")
                            ,idvar = c(1:6),timevar = c("TRADEDATE_YQ")
                            ,direction = "wide", varying = NULL,sep = ".") %>%
                     arrange(LOCATION_NO,BDTYPE_C)
names(dealcount_b_table)[names(dealcount_b_table)=="YQC.10701"]="YQC10701"
names(dealcount_b_table)[names(dealcount_b_table)=="YQC.10702"]="YQC10702"
names(dealcount_b_table)[names(dealcount_b_table)=="YQC.10703"]="YQC10703"
names(dealcount_b_table)[names(dealcount_b_table)=="YQC.10704"]="YQC10704"

saveRDS(dealcount_b_table,"C:/Users/Eric/Desktop/SARA/rds/dealcount_b_table.rds")
export(dealcount_b_table,"C:/Users/Eric/Desktop/SARA/M50/output_r_excel/dealcount_b_table.xlsx")

##--------- 計算房價指數 ----------
##篩選資料區間(年季)
usedata_0_2 = filter(alldata_base_2,
                     (TRADEDATE_YQF>=Base_YQ
                     &TRADEDATE_YQF<=End_YQ)) %>%
              arrange(COUNTY_F,LOCATION_NO,BDTYPE_G)

usedata_0_2_out = filter(alldata_base_2,
                         !(TRADEDATE_YQF>=Base_YQ
                          &TRADEDATE_YQF<=End_YQ)) %>%
                  arrange(COUNTY_F,LOCATION_NO,BDTYPE_G)

CLB_USE_2 = select(filter(CLB_USE,USE=="Y"),COUNTY_F,LOCATION_NO,BDTYPE_G,USE)%>%
            arrange(COUNTY_F,LOCATION_NO,BDTYPE_G)

usedata_2_0 = merge(usedata_0_2,CLB_USE_2,by = c("COUNTY_F","LOCATION_NO","BDTYPE_G"),all = T)
usedata_2 = filter(usedata_2_0,USE=="Y"&ID!="")
usedata_2_out = filter(usedata_2_0,is.na(USE)|is.na(ID))

##---------------- 樣本篩選 ----------------

##(D2)特殊關係交易##

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
usedata_2$D4 = ifelse((usedata_2$USETYPE=="")& (usedata_2$RURALTYPE!= ""),1,0)
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
           
usedata_3 = filter(usedata_2,OUT==0) %>%
            arrange(TRANSFLOOR)
usedata_3_out = filter(usedata_2,OUT!=0)
##--------(D3)排除特殊交易樓層---------
usedata_3$TRANSFLOOR_BAK = usedata_3$TRANSFLOOR
usedata_3$LL = nchar(usedata_3$TRANSFLOOR)
usedata_3$K = str_count(usedata_3$TRANSFLOOR_BAK,c("，"))
usedata_3$KK = regexpr("，",usedata_3$TRANSFLOOR_BAK)
usedata_3$KC = usedata_3$K

usedata_3 = select(usedata_3
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
                   SYC,SYN,USE,D2,D4,
                   D5,D8,D9,OUT,BMPRICE,
                   TRANSFLOOR_BAK,LL,K,KK,KC) %>% 
            arrange(K)

usedata_4_check = filter(usedata_3,K=="0")
usedata_4 = filter(usedata_3,K!="0")
usedata_4$SS = 0
usedata_4_check$SS = 0

a = str_split(usedata_4$TRANSFLOOR,"[，]")
usedata_4$TRANSFLOOR1 = sapply(a, "[", 1)
usedata_4$TRANSFLOOR2 = sapply(a, "[", 2)
usedata_4$TRANSFLOOR3 = sapply(a, "[", 3)
usedata_4$TRANSFLOOR4 = sapply(a, "[", 4)
usedata_4$TRANSFLOOR5 = sapply(a, "[", 5)
usedata_4$TRANSFLOOR6 = sapply(a, "[", 6)
usedata_4$TRANSFLOOR7 = sapply(a, "[", 7)

TRANSFLOOR1 = select(usedata_4,1:81,TRANSFLOOR1)
TRANSFLOOR2 = select(usedata_4,1:81,TRANSFLOOR2)
TRANSFLOOR3 = select(usedata_4,1:81,TRANSFLOOR3)
TRANSFLOOR4 = select(usedata_4,1:81,TRANSFLOOR4)
TRANSFLOOR5 = select(usedata_4,1:81,TRANSFLOOR5)
TRANSFLOOR6 = select(usedata_4,1:81,TRANSFLOOR6)
TRANSFLOOR7 = select(usedata_4,1:81,TRANSFLOOR7)
colnames(TRANSFLOOR1) = c(1:81,"TRANSFLOOR1")
colnames(TRANSFLOOR2) = c(1:81,"TRANSFLOOR1")
colnames(TRANSFLOOR3) = c(1:81,"TRANSFLOOR1")
colnames(TRANSFLOOR4) = c(1:81,"TRANSFLOOR1")
colnames(TRANSFLOOR5) = c(1:81,"TRANSFLOOR1")
colnames(TRANSFLOOR6) = c(1:81,"TRANSFLOOR1")
colnames(TRANSFLOOR7) = c(1:81,"TRANSFLOOR1")

usedata_4 = rbind(TRANSFLOOR1,TRANSFLOOR2,TRANSFLOOR3,TRANSFLOOR4,TRANSFLOOR5,TRANSFLOOR6,TRANSFLOOR7)
usedata_4 = filter(usedata_4,!(is.na(TRANSFLOOR1)))
usedata_4 = select(usedata_4,c(1:36,TRANSFLOOR1,38:81))
colnames(usedata_4) = c("ID","IMPORT","IMPORT_F","TRADEDATE","TRADEDATE_F","TRADEDATE_YM","TRADEDATE_YMF","TRADEDATE_YQ","TRADEDATE_YQF","BDFNDATE"
                        ,"BDFNDATE_F","BDFNDATE_YM","BDFNDATE_YMF","BDFNDATE_YQ","BDFNDATE_YQF","AGE_Y","AGE_C","PRESALE","COUNTY_F","COUNTY"
                        ,"COUNTY_OLD_F","COUNTY_OLD","LOCATION_NO","LOCATION","ADDRESS","ADDRESS_C","TRADETARGET","BDTYPE","BDTYPE_F","BDTYPE_C"
                        ,"BDTYPE_G","BDTYPE_K","MAINUSE","USETYPE","MAINMT","TOLFLOOR","TRANSFLOOR","TRADEBDNUM","LAND","BUILD"
                        ,"PARK","ROOM","HALL","TOILET","STRUCTURE","MANAGEMENT","PARKTYPE","PARKTYPE_F","MOTO","RURALTYPE"
                        ,"RURALTYPE_DT","LANDAREA","BDAREA","PARKAREA","TOLPRICE","TOLPARKPRICE","PRICE","BUILDPRICE","MIXPRICE","PARKPRICE"
                        ,"OTHER","YCB","YCT","YCC","YCN","SYC","SYN","USE","D2","D4"
                        ,"D5","D8","D9","OUT","BMPRICE","TRANSFLOOR_BAK","LL","K","KK","KC","SS")
usedata_4$K = usedata_4$K-7
usedata_4$KC = ifelse(usedata_4$K<=0,0,usedata_4$K)
usedata_4_check = rbind(usedata_4,usedata_4_check) %>%
                  arrange(KC,ID,SS)


usedata_5 = select(usedata_4_check,1:73,75:76)

usedata_6_out = filter(usedata_5,
                     (((grepl("一", usedata_5$TRANSFLOOR)==F) & (grepl("二", usedata_5$TRANSFLOOR)==F)
                     & (grepl("三", usedata_5$TRANSFLOOR)==F) & (grepl("四", usedata_5$TRANSFLOOR)==F)
                     & (grepl("五", usedata_5$TRANSFLOOR)==F) & (grepl("六", usedata_5$TRANSFLOOR)==F)
                     & (grepl("七", usedata_5$TRANSFLOOR)==F) & (grepl("八", usedata_5$TRANSFLOOR)==F)
                     & (grepl("九", usedata_5$TRANSFLOOR)==F) & (grepl("十", usedata_5$TRANSFLOOR)==F))
                     & TRANSFLOOR != "全" & TRANSFLOOR != "" )
                     | (grepl("地下", usedata_5$TRANSFLOOR)==T)
                     | TRANSFLOOR == "一層" | TOLFLOOR == "一層")
         
usedata_6 = filter(usedata_5,
                   !((((grepl("一", usedata_5$TRANSFLOOR)==F) & (grepl("二", usedata_5$TRANSFLOOR)==F)
                     & (grepl("三", usedata_5$TRANSFLOOR)==F) & (grepl("四", usedata_5$TRANSFLOOR)==F)
                     & (grepl("五", usedata_5$TRANSFLOOR)==F) & (grepl("六", usedata_5$TRANSFLOOR)==F)
                     & (grepl("七", usedata_5$TRANSFLOOR)==F) & (grepl("八", usedata_5$TRANSFLOOR)==F)
                     & (grepl("九", usedata_5$TRANSFLOOR)==F) & (grepl("十", usedata_5$TRANSFLOOR)==F))
                     & TRANSFLOOR != "全" & TRANSFLOOR != "" )
                     | (grepl("地下", usedata_5$TRANSFLOOR)==T)
                     | TRANSFLOOR == "一層" | TOLFLOOR == "一層"))
usedata_6$TRANSFLOOR = usedata_6$TRANSFLOOR_BAK 
usedata_6 = select(usedata_6,1:74)
usedata_6 = arrange_all(usedata_6)
usedata_6 = usedata_6[!duplicated(usedata_6),]

usedata_6_out$TRANSFLOOR = usedata_6_out$TRANSFLOOR_BAK 
usedata_6_out = select(usedata_6_out,1:74)
usedata_6_out = arrange_all(usedata_6_out)
usedata_6_out = usedata_6_out[!duplicated(usedata_6_out),]

usedata_6_check = select(usedata_6,ID)
usedata_6_check$REP = 1
usedata_6_check = usedata_6_check[!duplicated(usedata_6_check),]

usedata_6_out = merge(usedata_6_out, usedata_6_check ,by = "ID" , all.x= T) 

usedata_6_out = filter(usedata_6_out, is.na(REP))

##------------(D1)大批交易-------------
##取平均單價：同地址+同建物型態(原分類)+同建築完成年月+同交易年季

usedata_6_c = select(usedata_6
                     ,COUNTY_F,LOCATION,ADDRESS,BDTYPE_F,BDFNDATE_YMF,TRADEDATE_YQF,BMPRICE)%>%
              arrange(COUNTY_F,LOCATION,ADDRESS,BDTYPE_F,BDFNDATE_YMF,TRADEDATE_YQF)
usedata_6_c = group_by(usedata_6_c,COUNTY_F,LOCATION,ADDRESS,BDTYPE_F,BDFNDATE_YMF,TRADEDATE_YQF)
usedata_6_c = summarise(usedata_6_c,BMPRICE_mean=mean(BMPRICE))
usedata_6 = usedata_6 %>% arrange(COUNTY_F,LOCATION,ADDRESS,BDTYPE_F,BDFNDATE_YMF,TRADEDATE_YQF)
  
usedata_7 = merge(usedata_6,usedata_6_c,by = c("COUNTY_F","LOCATION","ADDRESS","BDTYPE_F","BDFNDATE_YMF","TRADEDATE_YQF"),all= T)

usedata_8_0 = usedata_7 %>%
              arrange(ID)
usedata_8 = usedata_8_0[!duplicated(usedata_8_0[,c("COUNTY_F","LOCATION","ADDRESS","BDTYPE_F","BDFNDATE_YMF","TRADEDATE_YQF")]),]
usedata_8_out = (usedata_8_0[duplicated(usedata_8_0[,c("COUNTY_F","LOCATION","ADDRESS","BDTYPE_F","BDFNDATE_YMF","TRADEDATE_YQF")]),])

usedata_ok = select(usedata_8,
                    "ID","IMPORT","IMPORT_F","TRADEDATE","TRADEDATE_F","TRADEDATE_YM","TRADEDATE_YMF",
                    "TRADEDATE_YQ","TRADEDATE_YQF","BDFNDATE","BDFNDATE_F","BDFNDATE_YM","BDFNDATE_YMF","BDFNDATE_YQ",
                    "BDFNDATE_YQF","AGE_Y","AGE_C","PRESALE","COUNTY_F","COUNTY","COUNTY_OLD_F",
                    "COUNTY_OLD","LOCATION_NO","LOCATION","ADDRESS","ADDRESS_C","TRADETARGET","BDTYPE",
                    "BDTYPE_F","BDTYPE_C","BDTYPE_G","BDTYPE_K","MAINUSE","USETYPE","MAINMT",
                    "TOLFLOOR","TRANSFLOOR","TRADEBDNUM","LAND","BUILD","PARK","ROOM",
                    "HALL","TOILET","STRUCTURE","MANAGEMENT","PARKTYPE","PARKTYPE_F","MOTO",
                    "RURALTYPE","RURALTYPE_DT","LANDAREA","BDAREA","PARKAREA","TOLPRICE","TOLPARKPRICE",
                    "PRICE","BUILDPRICE","MIXPRICE","PARKPRICE","OTHER","YCB","YCT",
                    "YCC","YCN","SYC","SYN","BMPRICE","BMPRICE_mean")
usedata_ok$BMPRICE_Z = usedata_ok$BMPRICE
usedata_ok$BMPRICE = round(usedata_ok$BMPRICE_mean,digits = 0)
usedata_ok = select(usedata_ok,
                    1:68,"BMPRICE_Z")%>%
             arrange(COUNTY_F,LOCATION_NO,BDTYPE_C,TRADEDATE_YQF)


##---- 主巨集：資料標準化，排除離群值|Z|>=3，繪製分佈圖 ----
##台北A、台中B、基隆C、台南D、高雄E、新北F、宜蘭G、桃園H、新竹市O
##公寓A、大樓B、透天C
result1 = aggregate(usedata_ok$BMPRICE,usedata_ok[,c("COUNTY_F","LOCATION_NO","BDTYPE_C","TRADEDATE_YQF")],scale)%>%
          arrange(COUNTY_F,LOCATION_NO,BDTYPE_C,TRADEDATE_YQF)
result1 = unlist(result1$x)
result1 = as.data.frame(result1)
usedata_nz_b = cbind(usedata_ok,result1)
usedata_nz_b$result1 = ifelse(usedata_nz_b$result1=="NaN",0,usedata_nz_b$result1)
usedata_yz_b  = filter(usedata_nz_b,abs(result1)<=3)

##---儲存資料集---
##未標準化
usedata_nz_b = select(usedata_nz_b,1:70)
saveRDS(usedata_nz_b,"C:/Users/Eric/Desktop/SARA/rds/usedata_nz_b.rds")
##已標準化
usedata_yz_b = select(usedata_yz_b,1:70)
saveRDS(usedata_yz_b,"C:/Users/Eric/Desktop/SARA/rds/usedata_yz_b.rds")



##--------繪製連續直方圖--------
####未標準化

ggplot(usedata_nz_b,aes(BMPRICE))+
  geom_histogram(aes(y=..density..),
                 col = "white",
                 fill = "cornflowerblue",
                 size = 0.1,
                 bins = 32)+
  geom_density()
####已標準化
ggplot(usedata_yz_b,aes(BMPRICE))+
  geom_histogram(aes(y=..density..),
                 col = "white",
                 fill = "cornflowerblue",
                 size = 0.1,
                 bins = 32)+
  geom_density()
##------ 完成實價登錄指數(OP_PPI_b) ------  
T0 = select(usedata_yz_b,1:70)
T0$TRADEDATE_YQK = paste("YQ",T0$TRADEDATE_YQ,sep ="" )

table_b = select(T0,COUNTY_F,COUNTY,LOCATION,LOCATION_NO,BDTYPE_G,BDTYPE_C,TRADEDATE_YQK,BMPRICE)

table_b = do.call(data.frame,aggregate(BMPRICE ~., table_b, FUN = function(x) c(mean=mean(x),n = length(x)),na.action = na.omit))%>%
          arrange(COUNTY_F,COUNTY,LOCATION,LOCATION_NO,BDTYPE_G,BDTYPE_C,TRADEDATE_YQK)

trans1 = select(table_b,1:8)%>%
         arrange(TRADEDATE_YQK)
trans1$BMPRICE.mean = round(trans1$BMPRICE.mean/10000,digits=1)

trans01 = reshape(trans1,v.names = c("BMPRICE.mean")
                  ,idvar = c(1:6),timevar = c("TRADEDATE_YQK")
                  ,direction = "wide",varying = NULL ,sep = ".")
names(trans01)[names(trans01)=="BMPRICE.mean.YQ10701"]="YQM10701"
names(trans01)[names(trans01)=="BMPRICE.mean.YQ10702"]="YQM10702"
names(trans01)[names(trans01)=="BMPRICE.mean.YQ10703"]="YQM10703"
names(trans01)[names(trans01)=="BMPRICE.mean.YQ10704"]="YQM10704"

trans2 = select(table_b,1:7,9)
trans02 = reshape(trans2,v.names = c("BMPRICE.n")
                  ,idvar = c(1:6),timevar = c("TRADEDATE_YQK")
                  ,direction = "wide",varying = NULL ,sep = ".")
names(trans02)[names(trans02)=="BMPRICE.n.YQ10701"]="YQN10701"
names(trans02)[names(trans02)=="BMPRICE.n.YQ10702"]="YQN10702"
names(trans02)[names(trans02)=="BMPRICE.n.YQ10703"]="YQN10703"
names(trans02)[names(trans02)=="BMPRICE.n.YQ10704"]="YQN10704"

OP_PPI_b = merge(trans01,trans02, by = c("COUNTY_F","COUNTY","LOCATION","LOCATION_NO",
                                         "BDTYPE_G","BDTYPE_C"))%>%
           arrange(LOCATION_NO,BDTYPE_C)
export(OP_PPI_b,"C:/Users/Eric/Desktop/SARA/M50/output_r_excel/OP_PPI_b.xlsx")
saveRDS(OP_PPI_b,"C:/Users/Eric/Desktop/SARA/rds/OP_PPI_b.rds")

##------- 完成永慶指數(YC_PPI_b) ------

table_yc_b = select(T0,COUNTY_F,COUNTY,LOCATION,LOCATION_NO,BDTYPE_G,BDTYPE_C,TRADEDATE_YQK,YCB)%>%
             arrange(COUNTY_F,COUNTY,LOCATION,LOCATION_NO,BDTYPE_G,BDTYPE_C,TRADEDATE_YQK)
table_yc_b = aggregate(YCB ~., table_yc_b, FUN = mean,na.action = na.omit)%>%
  arrange(COUNTY_F,COUNTY,LOCATION,LOCATION_NO,BDTYPE_G,BDTYPE_C,TRADEDATE_YQK)
table_yc_b$YCB_mean = round(table_yc_b$YCB / 10000,digits = 1)

table_yc_b = select(table_yc_b,1:7,9)
YC_PPI_b = reshape(table_yc_b,v.names = c("YCB_mean")
                  ,idvar = c(1:6),timevar = c("TRADEDATE_YQK")
                  ,direction = "wide",varying = NULL ,sep = ".")
names(YC_PPI_b)[names(YC_PPI_b)=="YCB_mean.YQ10701"]="YCB10701"
names(YC_PPI_b)[names(YC_PPI_b)=="YCB_mean.YQ10702"]="YCB10702"
names(YC_PPI_b)[names(YC_PPI_b)=="YCB_mean.YQ10703"]="YCB10703"
names(YC_PPI_b)[names(YC_PPI_b)=="YCB_mean.YQ10704"]="YCB10704"

YC_PPI_b = select(YC_PPI_b,1:12)%>%
           arrange(LOCATION_NO,BDTYPE_C)
export(YC_PPI_b,"C:/Users/Eric/Desktop/SARA/M50/output_r_excel/YC_PPI_b.xlsx")
saveRDS(YC_PPI_b,"C:/Users/Eric/Desktop/SARA/rds/YC_PPI_b.rds")

##-------完成指數合併檔(PPI_b)------

PPI_b_1 = merge(OP_PPI_b,YC_PPI_b,by = c("COUNTY_F","COUNTY","LOCATION","LOCATION_NO","BDTYPE_G","BDTYPE_C"))
PPI_b = merge(PPI_b_1,dealcount_b_table,by = c("COUNTY_F","COUNTY","LOCATION","LOCATION_NO","BDTYPE_G","BDTYPE_C"),all.x = TRUE)
PPI_b = PPI_b %>% arrange(LOCATION_NO,BDTYPE_C)
PPI_b = select(PPI_b,1:6,16:18,13:15,7:12)
export(PPI_b,"C:/Users/Eric/Desktop/SARA/M50/output_r_excel/PPI_b.xlsx")
saveRDS(PPI_b,"C:/Users/Eric/Desktop/SARA/rds/PPI_b.rds")

##----------- END -----------##
##----------- END -----------##
##----------- END -----------##
  

##--------- 程式結束 ----------

