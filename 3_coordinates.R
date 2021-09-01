library(xlsx)
library(dplyr)
library(stringr)
library(reshape2)
library(rio)
library(ggplot2)

alldata_base_2 = readRDS("./rds/ALLDATA_BASE_2.rds")
##每月11~15產出一年1月(10612~10711)
START_YM =10610
END_YM = 10709


#### --------------- 篩選樣本 --------------- ####
## 公寓2、大樓3.4、套房5、透天6 
## 台北A、新北F、桃園H、台中B、台南D、高雄E、
## 新竹市O 、基隆C、彰化縣N、嘉義市I、嘉義縣Q
usedata_0 = filter(alldata_base_2,
                   (TRADEDATE_YMF>=START_YM
                    &TRADEDATE_YMF<=END_YM
                    &(COUNTY_F=="A"|COUNTY_F=="F"|COUNTY_F=="H"
                      |COUNTY_F=="B"|COUNTY_F=="D"|COUNTY_F=="E"
                      |COUNTY_F=="O"|COUNTY_F=="C"|COUNTY_F=="N"
                      |COUNTY_F=="I"|COUNTY_F=="Q")))

## (D1)特殊關係交易 ##
usedata_0$D1 = ifelse(( (grepl("親", usedata_0$OTHER)) | (grepl("夫", usedata_0$OTHER))
                        | (grepl("妻", usedata_0$OTHER)) | (grepl("配偶", usedata_0$OTHER))
                        | (grepl("婆", usedata_0$OTHER)) | (grepl("媳", usedata_0$OTHER))
                        | (grepl("叔", usedata_0$OTHER)) | (grepl("侄", usedata_0$OTHER))
                        | (grepl("姪", usedata_0$OTHER)) | (grepl("姑", usedata_0$OTHER))
                        | (grepl("祖", usedata_0$OTHER)) | (grepl("孫", usedata_0$OTHER))
                        | (grepl("父", usedata_0$OTHER)) | (grepl("母", usedata_0$OTHER))
                        | (grepl("子", usedata_0$OTHER)) | (grepl("女", usedata_0$OTHER))
                        | (grepl("兒", usedata_0$OTHER)) | (grepl("哥", usedata_0$OTHER))
                        | (grepl("兄", usedata_0$OTHER)) | (grepl("姊", usedata_0$OTHER))
                        | (grepl("姐", usedata_0$OTHER)) | (grepl("弟", usedata_0$OTHER))
                        | (grepl("妹", usedata_0$OTHER)) | (grepl("友", usedata_0$OTHER))
                        | (grepl("員工", usedata_0$OTHER)) | (grepl("關係", usedata_0$OTHER))
                        | (grepl("特殊交易", usedata_0$OTHER))) == T ,1,0)
## (D2)非都市用地 ##
usedata_0$D2 = ifelse((usedata_0$USETYPE=="")& (usedata_0$RURALTYPE!= ""),1,0)

## (D3)車位/房/廳/衛 < 100 ##
usedata_0$D3 = ifelse((usedata_0$PARK>=100)|(usedata_0$ROOM>=100)
                      |(usedata_0$HALL>=100)|(usedata_0$TOILET>=100),1,0)

## (D4)特殊案件 ## 
usedata_0$D4 = ifelse(((grepl("土地", usedata_0$OTHER))&(grepl("租", usedata_0$OTHER))
                       |((grepl("債務", usedata_0$OTHER))|(grepl("地上權", usedata_0$OTHER))
                         |(grepl("身故", usedata_0$OTHER))|(grepl("死", usedata_0$OTHER))
                         |(grepl("兇", usedata_0$OTHER))|(grepl("凶", usedata_0$OTHER))
                         |(grepl("喪", usedata_0$OTHER))|(grepl("仁愛之家", usedata_0$OTHER))
                         |(grepl("國有", usedata_0$OTHER))|(grepl("共有人之間的買賣", usedata_0$OTHER))
                         |(grepl("國防部", usedata_0$OTHER))|(grepl("地主向建設公司承購", usedata_0$OTHER))
                         |(grepl("合建分屋", usedata_0$OTHER))|(grepl("安置住宅", usedata_0$OTHER))
                         |(grepl("政府", usedata_0$OTHER)) )) == T,1,0)

## (D5)無地址 ##
usedata_0$D5 = ifelse(usedata_0$ADDRESS=="",1,0)

usedata_0$OUT = (usedata_0$D1+usedata_0$D2+usedata_0$D3
                +usedata_0$D4+usedata_0$D5)

usedata_0_out = filter(usedata_0,usedata_0$OUT!=0)
usedata_0 = filter(usedata_0,usedata_0$OUT==0)

#### --------- 處理樓層欄位 ---------- ####
##TOLFLOOR 總樓層數
##TRANSFLOOR 移轉層次
usedata_1 = usedata_0 %>% arrange(TRANSFLOOR)

usedata_1$TRANSFLOOR_BAK = usedata_1$TRANSFLOOR
usedata_1$LL = nchar(usedata_1$TRANSFLOOR)
usedata_1$K = str_count(usedata_1$TRANSFLOOR_BAK,c("，"))
usedata_1$KK = regexpr("，",usedata_1$TRANSFLOOR_BAK)
usedata_1$KC = usedata_1$K
usedata_1 = usedata_1 %>% arrange(K)

usedata_2 = filter(usedata_1,K=="0")
usedata_2_out = filter(usedata_1,K!="0")
usedata_2$SS = 0
usedata_2_out$SS = 0

a = str_split(usedata_2_out$TRANSFLOOR,"[，]")
usedata_2_out$TRANSFLOOR1 = sapply(a, "[", 1)
usedata_2_out$TRANSFLOOR2 = sapply(a, "[", 2)
usedata_2_out$TRANSFLOOR3 = sapply(a, "[", 3)
usedata_2_out$TRANSFLOOR4 = sapply(a, "[", 4)
usedata_2_out$TRANSFLOOR5 = sapply(a, "[", 5)
usedata_2_out$TRANSFLOOR6 = sapply(a, "[", 6)
usedata_2_out$TRANSFLOOR7 = sapply(a, "[", 7)
usedata_2_out$TRANSFLOOR8 = sapply(a, "[", 8)
usedata_2_out$TRANSFLOOR9 = sapply(a, "[", 9)
usedata_2_out$TRANSFLOOR10 = sapply(a, "[", 10)
usedata_2_out$TRANSFLOOR11 = sapply(a, "[", 11)
usedata_2_out$TRANSFLOOR12 = sapply(a, "[", 12)
usedata_2_out$TRANSFLOOR13 = sapply(a, "[", 13)
usedata_2_out$TRANSFLOOR14 = sapply(a, "[", 14)
usedata_2_out$TRANSFLOOR15 = sapply(a, "[", 15)

TRANSFLOOR1 = select(usedata_2_out,1:79,TRANSFLOOR1)
TRANSFLOOR2 = select(usedata_2_out,1:79,TRANSFLOOR2)
TRANSFLOOR3 = select(usedata_2_out,1:79,TRANSFLOOR3)
TRANSFLOOR4 = select(usedata_2_out,1:79,TRANSFLOOR4)
TRANSFLOOR5 = select(usedata_2_out,1:79,TRANSFLOOR5)
TRANSFLOOR6 = select(usedata_2_out,1:79,TRANSFLOOR6)
TRANSFLOOR7 = select(usedata_2_out,1:79,TRANSFLOOR7)
TRANSFLOOR8  = select(usedata_2_out,1:79,TRANSFLOOR8)
TRANSFLOOR9  = select(usedata_2_out,1:79,TRANSFLOOR9)
TRANSFLOOR10 = select(usedata_2_out,1:79,TRANSFLOOR10)
TRANSFLOOR11 = select(usedata_2_out,1:79,TRANSFLOOR11)
TRANSFLOOR12 = select(usedata_2_out,1:79,TRANSFLOOR12)
TRANSFLOOR13 = select(usedata_2_out,1:79,TRANSFLOOR13)
TRANSFLOOR14 = select(usedata_2_out,1:79,TRANSFLOOR14)
TRANSFLOOR15 = select(usedata_2_out,1:79,TRANSFLOOR15)
colnames(TRANSFLOOR1) = c(1:79,"TRANSFLOOR1")
colnames(TRANSFLOOR2) = c(1:79,"TRANSFLOOR1")
colnames(TRANSFLOOR3) = c(1:79,"TRANSFLOOR1")
colnames(TRANSFLOOR4) = c(1:79,"TRANSFLOOR1")
colnames(TRANSFLOOR5) = c(1:79,"TRANSFLOOR1")
colnames(TRANSFLOOR6) = c(1:79,"TRANSFLOOR1")
colnames(TRANSFLOOR7) = c(1:79,"TRANSFLOOR1")
colnames(TRANSFLOOR8 ) = c(1:79,"TRANSFLOOR1")
colnames(TRANSFLOOR9 ) = c(1:79,"TRANSFLOOR1")
colnames(TRANSFLOOR10) = c(1:79,"TRANSFLOOR1")
colnames(TRANSFLOOR11) = c(1:79,"TRANSFLOOR1")
colnames(TRANSFLOOR12) = c(1:79,"TRANSFLOOR1")
colnames(TRANSFLOOR13) = c(1:79,"TRANSFLOOR1")
colnames(TRANSFLOOR14) = c(1:79,"TRANSFLOOR1")
colnames(TRANSFLOOR15) = c(1:79,"TRANSFLOOR1")

usedata_2_out= rbind(TRANSFLOOR1,TRANSFLOOR2,TRANSFLOOR3,TRANSFLOOR4,TRANSFLOOR5,TRANSFLOOR6,TRANSFLOOR7
                  ,TRANSFLOOR8,TRANSFLOOR9,TRANSFLOOR10,TRANSFLOOR11,TRANSFLOOR12,TRANSFLOOR13,TRANSFLOOR14,TRANSFLOOR15)
usedata_2_out = filter(usedata_2_out,!(is.na(TRANSFLOOR1)))
usedata_2_out = select(usedata_2_out,c(1:36,TRANSFLOOR1,38:80))

colnames(usedata_2_out) = c("ID","IMPORT","IMPORT_F","TRADEDATE","TRADEDATE_F","TRADEDATE_YM","TRADEDATE_YMF","TRADEDATE_YQ"
                            ,"TRADEDATE_YQF","BDFNDATE","BDFNDATE_F","BDFNDATE_YM","BDFNDATE_YMF","BDFNDATE_YQ","BDFNDATE_YQF","AGE_Y"
                            ,"AGE_C","PRESALE","COUNTY_F","COUNTY","COUNTY_OLD_F","COUNTY_OLD","LOCATION_NO","LOCATION","ADDRESS","ADDRESS_C"
                            ,"TRADETARGET","BDTYPE","BDTYPE_F","BDTYPE_C","BDTYPE_G","BDTYPE_K","MAINUSE","USETYPE"
                            ,"MAINMT","TOLFLOOR","TRANSFLOOR","TRADEBDNUM","LAND","BUILD","PARK","ROOM"
                            ,"HALL","TOILET","STRUCTURE","MANAGEMENT","PARKTYPE","PARKTYPE_F","MOTO","RURALTYPE"
                            ,"RURALTYPE_DT","LANDAREA","BDAREA","PARKAREA","TOLPRICE","TOLPARKPRICE","PRICE","BUILDPRICE"
                            ,"MIXPRICE","PARKPRICE","OTHER","YCB","YCT","YCC","YCN","SYC"
                            ,"SYN","D1","D2","D3","D4","D5","OUT","TRANSFLOOR_BAK"
                            ,"LL","K","KK","KC","SS")


usedata_2_out$K = usedata_2_out$K-15
usedata_2_out$KC = ifelse(usedata_2_out$K<=0,0,usedata_2_out$K)
usedata_2 = rbind(usedata_2,usedata_2_out) %>%
  arrange(KC,ID,SS)

usedata_3_0 = usedata_2
usedata_3_0$a1 = ifelse((grepl("一層", usedata_3_0$TRANSFLOOR))== T , 1,
                   ifelse((grepl("二層", usedata_3_0$TRANSFLOOR))== T, 2,
                   ifelse((grepl("三層", usedata_3_0$TRANSFLOOR))== T, 3,       
                   ifelse((grepl("四層", usedata_3_0$TRANSFLOOR))== T, 4,
                   ifelse((grepl("五層", usedata_3_0$TRANSFLOOR))== T, 5,
                   ifelse((grepl("六層", usedata_3_0$TRANSFLOOR))== T, 6,
                   ifelse((grepl("七層", usedata_3_0$TRANSFLOOR))== T, 7,
                   ifelse((grepl("八層", usedata_3_0$TRANSFLOOR))== T, 8,
                   ifelse((grepl("九層", usedata_3_0$TRANSFLOOR))== T, 9,
                   ifelse((grepl("十層", usedata_3_0$TRANSFLOOR))== T, 0,
                          99999))))))))))
usedata_3_0$a2 = ifelse((grepl("二十",usedata_3_0$TRANSFLOOR))== T, 2,
                   ifelse((grepl("三十",usedata_3_0$TRANSFLOOR))== T, 3,
                   ifelse((grepl("四十",usedata_3_0$TRANSFLOOR))== T, 4,
                   ifelse((grepl("五十",usedata_3_0$TRANSFLOOR))== T, 5,
                   ifelse((grepl("六十",usedata_3_0$TRANSFLOOR))== T, 6,
                   ifelse((grepl("七十",usedata_3_0$TRANSFLOOR))== T, 7,
                   ifelse((grepl("八十",usedata_3_0$TRANSFLOOR))== T, 8,
                   ifelse((grepl("九十",usedata_3_0$TRANSFLOOR))== T, 9,
                   ifelse((grepl("十",usedata_3_0$TRANSFLOOR))== T, 1,
                          0)))))))))

usedata_3_0$a3 = ifelse((grepl("地下",usedata_3_0$TRANSFLOOR))== T, 1, 0)
usedata_3_0$TRANSFLOOR_F = (usedata_3_0$a3*1000+usedata_3_0$a2*10+usedata_3_0$a1)
usedata_3_0$TRANSFLOOR_F = ifelse((grepl("地下層",usedata_3_0$TRANSFLOOR))== T ,1001 ,usedata_3_0$TRANSFLOOR_F)
usedata_3_0$TRANSFLOOR_F = ifelse((grepl("全",usedata_3_0$TRANSFLOOR))== T ,0 ,usedata_3_0$TRANSFLOOR_F)
usedata_3_0$TRANSFLOOR_F = ifelse(usedata_3_0$TRANSFLOOR=="" ,0 ,usedata_3_0$TRANSFLOOR_F)

usedata_3_0$a1 = ifelse((grepl("一層", usedata_3_0$TOLFLOOR))== T , 1,
                   ifelse((grepl("二層", usedata_3_0$TOLFLOOR))== T, 2,
                   ifelse((grepl("三層", usedata_3_0$TOLFLOOR))== T, 3,       
                   ifelse((grepl("四層", usedata_3_0$TOLFLOOR))== T, 4,
                   ifelse((grepl("五層", usedata_3_0$TOLFLOOR))== T, 5,
                   ifelse((grepl("六層", usedata_3_0$TOLFLOOR))== T, 6,
                   ifelse((grepl("七層", usedata_3_0$TOLFLOOR))== T, 7,
                   ifelse((grepl("八層", usedata_3_0$TOLFLOOR))== T, 8,
                   ifelse((grepl("九層", usedata_3_0$TOLFLOOR))== T, 9,
                   ifelse((grepl("十層", usedata_3_0$TOLFLOOR))== T, 0,
                          99999))))))))))
usedata_3_0$a2 = ifelse((grepl("二十",usedata_3_0$TOLFLOOR))== T, 2,
                   ifelse((grepl("三十",usedata_3_0$TOLFLOOR))== T, 3,
                   ifelse((grepl("四十",usedata_3_0$TOLFLOOR))== T, 4,
                   ifelse((grepl("五十",usedata_3_0$TOLFLOOR))== T, 5,
                   ifelse((grepl("六十",usedata_3_0$TOLFLOOR))== T, 6,
                   ifelse((grepl("七十",usedata_3_0$TOLFLOOR))== T, 7,
                   ifelse((grepl("八十",usedata_3_0$TOLFLOOR))== T, 8,
                   ifelse((grepl("九十",usedata_3_0$TOLFLOOR))== T, 9,
                   ifelse((grepl("十",usedata_3_0$TOLFLOOR))== T, 1,
                           0)))))))))

usedata_3_0$TOLFLOOR_F = (usedata_3_0$a2*10+usedata_3_0$a1)
usedata_3_0$TOLFLOOR_F = ifelse(usedata_3_0$TOLFLOOR=="", 0, usedata_3_0$TOLFLOOR_F)

usedata_3_0 = select(usedata_3_0,1:74,83:84)
usedata_3 = filter(usedata_3_0,TOLFLOOR_F!=99999 & TRANSFLOOR_F!=99999)
usedata_3_out = filter(usedata_3_0,!(TOLFLOOR_F!=99999 & TRANSFLOOR_F!=99999))
                  
usedata_3_table = select(usedata_3,ID,TRANSFLOOR_F)           
usedata_3_table = aggregate(TRANSFLOOR_F ~., usedata_3_table, FUN = min)%>%
                  arrange(ID)
colnames(usedata_3_table) = c("ID","TRANSFLOOR_F_MIN")

usedata_3 = usedata_3 %>% arrange(ID)                                

usedata_4_0 = merge(usedata_3, usedata_3_table, by ="ID", all = T)
usedata_4_0$TRANSFLOOR = usedata_4_0$TRANSFLOOR_BAK

usedata_4 = filter(usedata_4_0,(TRANSFLOOR_F==TRANSFLOOR_F_MIN))%>%
            arrange(ID)
usedata_4_out =filter(usedata_4_0,!(TRANSFLOOR_F==TRANSFLOOR_F_MIN))

#### ------ 整理資料 ------  ####
## 公寓2、大樓3.4、套房5、透天6

usedata_5_0 = usedata_4
usedata_5_0$TRANSFLOOR_F = ifelse(usedata_5_0$TRANSFLOOR_F_MIN > 1000, 0-(usedata_5_0$TRANSFLOOR_F_MIN -1000),
                             ifelse(usedata_5_0$TRANSFLOOR_F_MIN == 0, "",usedata_5_0$TRANSFLOOR_F))
usedata_5_0$TOLFLOOR_F = ifelse(usedata_5_0$TOLFLOOR_F == 0, "", usedata_5_0$TOLFLOOR_F)
usedata_5_0$TRANSFLOOR_F = ifelse(usedata_5_0$BDTYPE_F == 6, "", usedata_5_0$TRANSFLOOR_F)

## 排除:非透天，移轉樓層=總樓層，備註有增建OR頂加
## 排除:非透天，移轉樓層=1 OR 總樓層=1
usedata_5_out = filter(usedata_5_0,
                       (((BDTYPE_F!=6)&(TRANSFLOOR_F==TOLFLOOR_F)&(((grepl("增建",usedata_5_0$OTHER))|(grepl("頂樓加蓋",usedata_5_0$OTHER)))== T))
                        |((BDTYPE_F!=6)&((TRANSFLOOR_F==1)|(TOLFLOOR_F==1)))))
usedata_5 = filter(usedata_5_0,
                   (!(((BDTYPE_F!=6)&(TRANSFLOOR_F==TOLFLOOR_F)&(((grepl("增建",usedata_5_0$OTHER))|(grepl("頂樓加蓋",usedata_5_0$OTHER)))== T))
                     |((BDTYPE_F!=6)&((TRANSFLOOR_F==1)|(TOLFLOOR_F==1))))))
usedata_5 = select(usedata_5,1:67,75:77)
usedata_5_out = select(usedata_5_out,1:67,75:77)

#### ----- 排除高價住宅 ----- ####

usedata_6_0 = usedata_5

usedata_6_out = filter(usedata_6_0,
                       (((COUNTY_F=="A")&(TOLPRICE>=70000000))
                         |((COUNTY_F=="F")&(TOLPRICE>=60000000))
                         |(!((COUNTY_F=="A")|(COUNTY_F=="F"))&(TOLPRICE>=40000000))))
usedata_6 = filter(usedata_6_0,
                   (!(((COUNTY_F=="A")&(TOLPRICE>=70000000))
                    |((COUNTY_F=="F")&(TOLPRICE>=60000000))
                    |(!((COUNTY_F=="A")|(COUNTY_F=="F"))&(TOLPRICE>=40000000)))))

#### ----- 表單欄位整理 ----- ####

usedata_7_0 = usedata_6
usedata_7_0$Z1 = ""
usedata_7_0$Z2 = usedata_7_0$ADDRESS
usedata_7_0$Z3 = usedata_7_0$ID
## format Z4 Z5 YYMMDDS10.
## Z4 = MDY(MM,DD,YY)
usedata_7_0$Z4 = paste((as.numeric(substr(usedata_7_0$TRADEDATE,1,3))+1911),(substr(usedata_7_0$TRADEDATE,4,5)),(substr(usedata_7_0$TRADEDATE,6,7)),sep = "/")
## Z5 = MDY(MM,DD,YY)
usedata_7_0$Z5 = ifelse(nchar(usedata_7_0$BDFNDATE)==5,(paste((as.numeric(substr(usedata_7_0$BDFNDATE,1,3))+1911),(substr(usedata_7_0$BDFNDATE,4,5)),"01",sep = "/")),
                   ifelse((substr(usedata_7_0$BDFNDATE,4,7))=="0229",(paste((as.numeric(substr(usedata_7_0$BDFNDATE,1,3))+1911),(substr(usedata_7_0$BDFNDATE,4,5)),"01",sep = "/")),
                          (paste((as.numeric(substr(usedata_7_0$BDFNDATE,1,3))+1911),(substr(usedata_7_0$BDFNDATE,4,5)),(substr(usedata_7_0$BDFNDATE,6,7)),sep = "/"))))
usedata_7_0$Z6 = ifelse(usedata_7_0$AGE_Y=="","",
                   ifelse(usedata_7_0$AGE_Y<0,0,usedata_7_0$AGE_Y))
usedata_7_0$Z7 = usedata_7_0$COUNTY
usedata_7_0$Z8 = usedata_7_0$LOCATION_NO
usedata_7_0$Z9 = usedata_7_0$LOCATION
## 公寓2、大樓3.4、套房5、透天6
usedata_7_0$Z10 = ifelse(usedata_7_0$BDTYPE_F==2,"R1",
                    ifelse(usedata_7_0$BDTYPE_F==3,"R2",
                    ifelse(usedata_7_0$BDTYPE_F==4,"R2",
                    ifelse(usedata_7_0$BDTYPE_F==6,"R5",
                    "R3"))))
usedata_7_0$Z10 = ifelse((usedata_7_0$BDAREA)*0.3025<15,"R3",usedata_7_0$Z10)
usedata_7_0$Z11 = usedata_7_0$MAINUSE
usedata_7_0$Z12 = usedata_7_0$USETYPE
usedata_7_0$Z13 = usedata_7_0$MAINMT
usedata_7_0$Z14 = usedata_7_0$TOLFLOOR_F
usedata_7_0$Z15 = usedata_7_0$TRANSFLOOR_F
usedata_7_0$Z16 = usedata_7_0$PARK
usedata_7_0$Z17 = usedata_7_0$ROOM
usedata_7_0$Z18 = usedata_7_0$HALL
usedata_7_0$Z19 = usedata_7_0$TOILET
usedata_7_0$Z20 = usedata_7_0$STRUCTURE

usedata_7_0$Z21 = ifelse(usedata_7_0$PARKTYPE=="塔式車位","塔式機械循環",
                    ifelse(usedata_7_0$PARKTYPE=="升降機械","機械機械",
                    ifelse(usedata_7_0$PARKTYPE=="升降平面","機械平面",
                    ifelse(usedata_7_0$PARKTYPE=="一樓平面","坡道平面",
                           usedata_7_0$PARKTYPE))))

usedata_7_0$Z22 = usedata_7_0$RURALTYPE
usedata_7_0$Z23 = usedata_7_0$RURALTYPE_DT
usedata_7_0$Z24 = usedata_7_0$LANDAREA
usedata_7_0$Z25 = usedata_7_0$BDAREA
usedata_7_0$Z26 = usedata_7_0$PARKAREA
usedata_7_0$Z27 = usedata_7_0$TOLPRICE
usedata_7_0$Z28 = usedata_7_0$TOLPARKPRICE
usedata_7_0$Z29 = usedata_7_0$PRICE
usedata_7_0$Z30 = usedata_7_0$OTHER

usedata_8 = select(usedata_7_0,TOLFLOOR,TRANSFLOOR,71:100)
usedata_8$TOLFLOOR = ifelse(usedata_8$TOLFLOOR=="",NA,usedata_8$TOLFLOOR)
usedata_8$TRANSFLOOR = ifelse(usedata_8$TRANSFLOOR=="",NA,usedata_8$TRANSFLOOR)

usedata_8 = usedata_8%>% arrange(Z7,Z8,Z4,Z2,Z10)
##----匯出資料----
saveRDS(usedata_8,"C:/Users/Eric/Desktop/SARA/rds/M50_20171000_20180902.rds")
export(usedata_8,"C:/Users/Eric/Desktop/SARA/M50/output_r_excel/M50_20171000_20180902.xlsx")
