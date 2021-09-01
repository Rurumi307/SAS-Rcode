library(xlsx)
library(dplyr)
library(stringr)
library(rio)



Start_YQ =10701
##aaa 依月分更改
aaa = readRDS("./rds/ALLDATA_BASE.rds")
SY_data = readRDS("./rds/SY_data.rds")
YC_data = readRDS("./rds/YC_data.rds")
CLB_USE = readRDS("./rds/CLB_USE.rds")
COUNTY_OLD = readRDS("./rds/County_old.rds")

x = aaa
SY = SY_data
YC = YC_data
USE = CLB_USE
OLD = COUNTY_OLD

#--------------最新縣市分類-------------

x$COUNTY_F = ifelse(substr(x$ADDRESS,1,3)=="臺北市","A",
            ifelse(substr(x$ADDRESS,1,3)=="臺中市","B",      
            ifelse(substr(x$ADDRESS,1,3)=="基隆市","C",      
            ifelse(substr(x$ADDRESS,1,3)=="臺南市","D",      
            ifelse(substr(x$ADDRESS,1,3)=="高雄市","E",      
            ifelse(substr(x$ADDRESS,1,3)=="新北市","F",      
            ifelse(substr(x$ADDRESS,1,3)=="宜蘭縣","G",      
            ifelse(substr(x$ADDRESS,1,3)=="桃園市","H",
            ifelse(substr(x$ADDRESS,1,3)=="嘉義市","I",      
            ifelse(substr(x$ADDRESS,1,3)=="新竹縣","J",      
            ifelse(substr(x$ADDRESS,1,3)=="苗栗縣","K",      
            ifelse(substr(x$ADDRESS,1,3)=="南投縣","M",      
            ifelse(substr(x$ADDRESS,1,3)=="彰化縣","N",
            ifelse(substr(x$ADDRESS,1,3)=="新竹市","O",      
            ifelse(substr(x$ADDRESS,1,3)=="雲林縣","P",      
            ifelse(substr(x$ADDRESS,1,3)=="嘉義縣","Q",
            ifelse(substr(x$ADDRESS,1,3)=="屏東縣","T",      
            ifelse(substr(x$ADDRESS,1,3)=="花蓮縣","U",      
            ifelse(substr(x$ADDRESS,1,3)=="臺東縣","V",
            ifelse(substr(x$ADDRESS,1,3)=="金門縣","W",      
            ifelse(substr(x$ADDRESS,1,3)=="澎湖縣","X",      
            ifelse(substr(x$ADDRESS,1,3)=="連江縣","Z",
                   x$COUNTY_F))))))))))))))))))))))

x$COUNTY = ifelse(x$COUNTY_F =="A","台北市",
             ifelse(x$COUNTY_F =="B","台中市",
             ifelse(x$COUNTY_F =="C","基隆市",
             ifelse(x$COUNTY_F =="D","台南市",
             ifelse(x$COUNTY_F =="E","高雄市",
             ifelse(x$COUNTY_F =="F","新北市",
             ifelse(x$COUNTY_F =="G","宜蘭縣",
             ifelse(x$COUNTY_F =="H","桃園市",
             ifelse(x$COUNTY_F =="I","嘉義市",
             ifelse(x$COUNTY_F =="J","新竹縣",
             ifelse(x$COUNTY_F =="K","苗栗縣",
             ifelse(x$COUNTY_F =="M","南投縣",
             ifelse(x$COUNTY_F =="N","彰化縣",
             ifelse(x$COUNTY_F =="O","新竹市",
             ifelse(x$COUNTY_F =="P","雲林縣",
             ifelse(x$COUNTY_F =="Q","嘉義縣",
             ifelse(x$COUNTY_F =="T","屏東縣",
             ifelse(x$COUNTY_F =="U","花蓮縣",
             ifelse(x$COUNTY_F =="V","台東縣",
             ifelse(x$COUNTY_F =="W","金門縣",
             ifelse(x$COUNTY_F =="X","澎湖縣",
             ifelse(x$COUNTY_F =="Z","連江縣",
             x$COUNTY_F))))))))))))))))))))))

#----土地區段位置/建物區段門牌_分類-----

x$ADDRESS_C = 
  ifelse((grepl("地號", x$ADDRESS)) == T, substr(x$ADDRESS,1,regexpr("地號",x$ADDRESS)+1)
       ,ifelse((grepl("新村", x$ADDRESS)) == T, substr(x$ADDRESS,1,regexpr("新村",x$ADDRESS)+1)
       ,ifelse((grepl("一村", x$ADDRESS)) == T, substr(x$ADDRESS,1,regexpr("一村",x$ADDRESS)+1)
       ,ifelse((grepl("二村", x$ADDRESS)) == T, substr(x$ADDRESS,1,regexpr("二村",x$ADDRESS)+1) 
       ,ifelse((grepl("衖", x$ADDRESS)) == T, substr(x$ADDRESS,1,regexpr("衖",x$ADDRESS)) 
       ,ifelse((grepl("弄", x$ADDRESS)) == T, substr(x$ADDRESS,1,regexpr("弄",x$ADDRESS)) 
       ,ifelse((grepl("巷", x$ADDRESS)) == T, substr(x$ADDRESS,1,regexpr("巷",x$ADDRESS)) 
       ,ifelse((grepl("段", x$ADDRESS)) == T, substr(x$ADDRESS,1,regexpr("段",x$ADDRESS)) 
       ,ifelse((grepl("街", x$ADDRESS)) == T, substr(x$ADDRESS,1,regexpr("街",x$ADDRESS)) 
       ,ifelse((grepl("路", x$ADDRESS)) == T, substr(x$ADDRESS,1,regexpr("路",x$ADDRESS)) 
       ,ifelse((grepl("鄰", x$ADDRESS)) == T, substr(x$ADDRESS,1,regexpr("鄰",x$ADDRESS))
       ,ifelse((grepl("大道", x$ADDRESS)) == T, substr(x$ADDRESS,1,regexpr("大道",x$ADDRESS)+1)
       ,ifelse((grepl("新城", x$ADDRESS)) == T, substr(x$ADDRESS,1,regexpr("新城",x$ADDRESS)+1)
                ,x$ADDRESS)))))))))))))

###x$address_c =
###  case_when(
###    (grepl("地號", x$ADDRESS)) == T ~ substr(x$ADDRESS,1,regexpr("地號",x$ADDRESS)+1),
###    (grepl("新村", x$ADDRESS)) == T ~ substr(x$ADDRESS,1,regexpr("新村",x$ADDRESS)+1),
###    (grepl("一村", x$ADDRESS)) == T ~ substr(x$ADDRESS,1,regexpr("一村",x$ADDRESS)+1),
###    (grepl("二村", x$ADDRESS)) == T ~ substr(x$ADDRESS,1,regexpr("二村",x$ADDRESS)+1),
###    (grepl("衖", x$ADDRESS)) == T ~ substr(x$ADDRESS,1,regexpr("衖",x$ADDRESS)),
###    (grepl("弄", x$ADDRESS)) == T ~ substr(x$ADDRESS,1,regexpr("弄",x$ADDRESS)),
###    (grepl("巷", x$ADDRESS)) == T ~ substr(x$ADDRESS,1,regexpr("巷",x$ADDRESS)),
###    (grepl("段", x$ADDRESS)) == T ~ substr(x$ADDRESS,1,regexpr("段",x$ADDRESS)),
###    (grepl("街", x$ADDRESS)) == T ~ substr(x$ADDRESS,1,regexpr("街",x$ADDRESS)),
###    (grepl("路", x$ADDRESS)) == T ~ substr(x$ADDRESS,1,regexpr("路",x$ADDRESS)),
###    (grepl("鄰", x$ADDRESS)) == T ~ substr(x$ADDRESS,1,regexpr("鄰",x$ADDRESS)),
###    (grepl("大道", x$ADDRESS)) == T ~ substr(x$ADDRESS,1,regexpr("大道",x$ADDRESS)+1),
###    (grepl("新城", x$ADDRESS)) == T ~ substr(x$ADDRESS,1,regexpr("新城",x$ADDRESS)+1)
###    )
###View(x$address_c)

#----------填補鄉鎮市區空白格-----------

x$LOCATION = str_squish( x$LOCATION)

#--------------日期變數整理--------------
##交易日期
x$TRADEDATE = ifelse((nchar(x$TRADEDATE) == 6),(paste("0",x$TRADEDATE,sep = "")),
                (ifelse((nchar(x$TRADEDATE) == 5),paste("00",x$TRADEDATE,sep = ""),x$TRADEDATE)))
##交易年月日_連續
x$TRADEDATE_F = as.numeric(x$TRADEDATE)
##交易年月_類別
x$TRADEDATE_YM = substr(x$TRADEDATE,1,5)
##交易年月_連續
x$TRADEDATE_YMF = as.numeric(x$TRADEDATE_YM)
Tradedate_q =  ceiling((as.numeric(substr(x$TRADEDATE,4,5)))/3)
##交易年季_類別
x$TRADEDATE_YQ = paste(substr(x$TRADEDATE,1,3),"0",Tradedate_q,sep ="" )
##交易年季_連續
x$TRADEDATE_YQF = as.numeric(x$TRADEDATE_YQ)

x$OTHER1 = ifelse(((as.numeric(substr(x$TRADEDATE,1,3)))<50),1,"")  
##建築完成年月日
##建築完成年月日_連續
x$BDFNDATE_F = ifelse((as.numeric(substr(x$BDFNDATE,1,3))<20)
                      ,NA,as.numeric(x$BDFNDATE))
##建築完成年月_類別
x$BDFNDATE_YM = ifelse(as.numeric(substr(x$BDFNDATE,1,3))<20
                       ,NA,substr(x$BDFNDATE,1,5))
##建築完成年月_連續
x$BDFNDATE_YMF = ifelse(as.numeric(substr(x$BDFNDATE,1,3))<20
                        ,NA,as.numeric(x$BDFNDATE_YM))
Bdfndate_q = ifelse(as.numeric(substr(x$BDFNDATE,1,3))<20
                    ,NA,ceiling((as.numeric(substr(x$BDFNDATE,4,5)))/3))
##建築完成年季_類別
x$BDFNDATE_YQ = ifelse(as.numeric(substr(x$BDFNDATE,1,3))<20
                       ,NA,paste(substr(x$BDFNDATE,1,3),"0",Bdfndate_q,sep ="" ))
##建築完成年季_連續
x$BDFNDATE_YQF = ifelse(as.numeric(substr(x$BDFNDATE,1,3))<20
                        ,NA,as.numeric(x$BDFNDATE_YQ))
x$BDFNDATE_YQ = gsub("0NA","",x$BDFNDATE_YQ)  

#屋齡(年)
##交易年-建築年+((交易月-建築月)/12)
x$AGE_Y = ((as.numeric(substr(x$TRADEDATE,1,3))
                  - (ifelse((as.numeric(substr(x$BDFNDATE,1,3))<20)
                            ,NA,as.numeric(substr(x$BDFNDATE,1,3)))))
                 +(((as.numeric(substr(x$TRADEDATE, 4, 5)))
                   -(ifelse((as.numeric(substr(x$BDFNDATE,1,3))<20)
                            ,NA,as.numeric(substr(x$BDFNDATE, 4, 5)))))/12))
x$AGE_Y = round((ifelse(x$AGE_Y<0,x$AGE_Y-0.01,x$AGE_Y+0.01)),digits = 1)

x$AGE_Y = as.numeric(ifelse((is.na(x$AGE_Y))==T,"NA",x$AGE_Y))

#--------------屋齡分群(年)--------------
x$AGE_C = ifelse(is.na(x$AGE_Y)==T , "99_no_age",
  ifelse(x$AGE_Y < 0 , "999_presale",
  ifelse(x$AGE_Y < 10 & x$AGE_Y >= 0, "00_10-",
  ifelse(x$AGE_Y < 20 & x$AGE_Y >= 10, "10_20-",
  ifelse(x$AGE_Y < 30 & x$AGE_Y >= 20, "20_30-",
  ifelse(x$AGE_Y < 40 & x$AGE_Y >= 30, "30_40-",
  ifelse(x$AGE_Y < 50 & x$AGE_Y >= 40, "40_50-",     
  ifelse(x$AGE_Y >= 50, "50+", x$AGE_Y))))))))

#-------------土地、建物、車位------------
land_build_park = str_split(x$TRADEBDNUM,pattern = "土地|建物|車位")
x$LAND = as.numeric(sapply(land_build_park, "[", 2))
x$BUILD = as.numeric(sapply(land_build_park, "[", 3))
x$PARK = as.numeric(sapply(land_build_park, "[", 4))

#---------------判別機車車位--------------
x$MOTO = ifelse((grepl("機車", x$OTHER)) == 1, "1", "0")

#-----------------判別預售----------------
x$PRESALE = ifelse(((!is.na(x$AGE_Y)&(x$AGE_Y < 0))), "1",
       (ifelse((grepl("預售", x$OTHER)) == 1, "2", "0")))

#------建築型態編號：2、(3.4)、5、6-------
x$BDTYPE_F = ifelse(x$BDTYPE == "工廠" , 1,
               ifelse(x$BDTYPE == "公寓(5樓含以下無電梯)" ,2,
               ifelse(x$BDTYPE == "華廈(10層含以下有電梯)",3,
               ifelse(x$BDTYPE == "住宅大樓(11層含以上有電梯)",4 ,
               ifelse(x$BDTYPE == "套房(1房1廳1衛)",5 ,
               ifelse(x$BDTYPE == "透天厝",6 ,     
               ifelse(x$BDTYPE == "店面(店鋪)",7 ,
               ifelse(x$BDTYPE == "倉庫",8 ,
               ifelse(x$BDTYPE == "農舍",9 ,
               ifelse(x$BDTYPE == "廠辦",10 ,     
               ifelse(x$BDTYPE == "辦公商業大樓",11 ,
               ifelse(x$BDTYPE == "其他",12       
                      , 0))))))))))))

#----------------安泰分類------------------
x$BDTYPE_K = ifelse(x$BDTYPE_F == 1, "廠辦",
              ifelse(x$BDTYPE_F == 2, "公寓",
              ifelse(x$BDTYPE_F == 3, "大樓",
              ifelse(x$BDTYPE_F == 4, "大樓",
              ifelse(x$BDTYPE_F == 5, "套房",
              ifelse(x$BDTYPE_F == 6, "透天",
              ifelse(x$BDTYPE_F == 7, "店面",
              ifelse(x$BDTYPE_F == 8, "其他",
              ifelse(x$BDTYPE_F == 9, "農舍",
              ifelse(x$BDTYPE_F == 10, "廠辦",
              ifelse(x$BDTYPE_F == 11, "辦公室",
              ifelse(x$BDTYPE_F == 12, "其他"
                     ,"未分類"))))))))))))

#-----------------永慶分類------------------
x$BDTYPE_C = ifelse(x$BDTYPE_F == 2, "A",
              ifelse(x$BDTYPE_F == 3, "B",
              ifelse(x$BDTYPE_F == 4, "B",
              ifelse(x$BDTYPE_F == 6, "C"
                     ,"O"))))
x$BDTYPE_G = ifelse(x$BDTYPE_F == 2, "公寓",
              ifelse(x$BDTYPE_F == 3, "大樓",
              ifelse(x$BDTYPE_F == 4, "大樓",
              ifelse(x$BDTYPE_F == 6, "透天"
                     ,"其他"))))

#---------------車位型態編號---------------
x$PARKTYPE_F = ifelse(x$PARKTYPE == "一樓平面", 1,
                 ifelse(x$PARKTYPE == "升降平面", 2,
                 ifelse(x$PARKTYPE == "升降機械", 3,
                 ifelse(x$PARKTYPE == "坡道平面", 4,
                 ifelse(x$PARKTYPE == "坡道機械", 5,
                 ifelse(x$PARKTYPE == "塔式車位", 6,
                 ifelse(x$PARKTYPE == "其他", 7
                       ,0)))))))

#----------------轉數字格式----------------
x$ROOM = as.numeric(x$ROOM)
x$HALL = as.numeric(x$HALL)
x$TOILET = as.numeric(x$TOILET)
x$IMPORT_F = as.numeric(x$IMPORT)

#-----------------單價變換-----------------
x$PRICE = as.numeric(ifelse(x$PRICE == "NaN" ,-1,x$PRICE))

#---篩選建物型態:公寓2、大樓3.4、套房5、透天6---
alldata_1 = filter(x,BDTYPE_F >= 2 & BDTYPE_F <=6)
alldata_1_out = filter(x,BDTYPE_F >6 | BDTYPE_F <2)

##---排除交易日期無法辨識之案件(排除other1=1)---
alldata_2_out = filter(alldata_1, OTHER1 ==1)
##建物23456和交易日期(other1=="")
alldata_2 = filter(alldata_1, OTHER1 =="")

#--------產出無法辨識之交易日期明細---------
dealdate_out_table = filter(alldata_2_out,is.na(alldata_2_out$TRADEDATE))
saveRDS(dealdate_out_table,"C:/Users/Eric/Desktop/SARA/rds/DEALDATE_OUT_TABLE.rds")
export(dealdate_out_table, "C:/Users/Eric/Desktop/SARA/M50/output_r_excel/DEALDATE_OUT_TABLE.xlsx")

#-----------篩選資料起始期(年季)-----------
alldata_31 = subset(alldata_2, select = -OTHER1 )
alldata_3 = filter(alldata_31, TRADEDATE_YQF >=10701)
alldata_3_out = filter(alldata_31, TRADEDATE_YQF <10701)

#---------------增加舊縣市變數----------------
##特殊縣市區:台東縣_金fa4b##
alldata_41 = merge(alldata_3, OLD, by.x = c("COUNTY_F","COUNTY","LOCATION"),by.y = c("COUNTY_F","COUNTY","LOCATION"), all = T)
alldata_41$COUNTY_OLD = ifelse(alldata_41$COUNTY =="台東縣","台東縣",alldata_41$COUNTY_OLD)
alldata_41$COUNTY_OLD_F = ifelse(alldata_41$COUNTY =="台東縣","V",alldata_41$COUNTY_OLD_F)                              

alldata_4_out = filter(alldata_41, is.na(ID))
alldata_4 = filter(alldata_41, ID!="" )
alldata_4_check = filter(alldata_41, COUNTY_OLD=="")

#--------產出無match到的舊縣市區明細----------
oldcounty_no_match = filter(alldata_4_check,COUNTY_F,COUNTY,LOCATION)
saveRDS(oldcounty_no_match, "C:/Users/Eric/Desktop/SARA/rds/OLDCOUNTY_NO_MATCH.rds")
export(oldcounty_no_match, "C:/Users/Eric/Desktop/SARA/M50/output_r_excel/OLDCOUNTY_NO_MATCH.xlsx")

#------增加永慶資料:YCB、YCT、YCC、YCN--------
YC = YC_data
YC_1 = select(YC, COUNTY_F,LOCATION,BDTYPE_G,COUNTY_OLD_F,TRADEDATE_YQF,YCB,YCT,YCC,YCN)%>%
       arrange(COUNTY_F,LOCATION,BDTYPE_G,COUNTY_OLD_F,TRADEDATE_YQF)
alldata_4 = alldata_4 %>% arrange(COUNTY_F,LOCATION,BDTYPE_G,COUNTY_OLD_F,TRADEDATE_YQF)

alldata_71 = merge(alldata_4, YC_1, by = c("COUNTY_F","LOCATION","BDTYPE_G","COUNTY_OLD_F","TRADEDATE_YQF"),all = TRUE)
alldata_7_out = filter(alldata_71, is.na(ID))
alldata_7 = filter(alldata_71, ID!="" )

#------------增加鄉鎮市區郵政代碼---------------
USE = CLB_USE
USE_1 = select(USE, COUNTY_F,LOCATION,LOCATION_NO) %>%
        arrange(COUNTY_F,LOCATION)
USE_1 = USE_1[!duplicated(USE_1),]
alldata_7 = alldata_7 %>% arrange(COUNTY_F,LOCATION)

alldata_81 = merge(alldata_7, USE_1, by = c("COUNTY_F","LOCATION"), all =T)
alldata_8 = filter(alldata_81, ID!="" )
alldata_8_check = filter(alldata_81, is.na(ID)|is.na(LOCATION))

location_no_no_match = alldata_8_check
saveRDS(location_no_no_match, "C:/Users/Eric/Desktop/SARA/rds/LOCATION_NO_NO_MATCH.rds")
export(location_no_no_match, "C:/Users/Eric/Desktop/SARA/M50/output_r_excel/LOCATION_NO_NO_MATCH.xlsx")

#----------增加信義資料:SYC、SYN---------------
SY = SY_data
SY_1 = select(SY, COUNTY_F,TRADEDATE_YQF,SYC) %>%
       arrange(COUNTY_F,TRADEDATE_YQF)
alldata_8 = alldata_8 %>% arrange(COUNTY_F,TRADEDATE_YQF)
alldata_91 =merge(alldata_8, SY_1, by = c("COUNTY_F","TRADEDATE_YQF"),all = T)
alldata_9 = filter(alldata_91, ID!="")
alldata_9_out = filter(alldata_91, is.na(ID))
SY_2 = select(SY, COUNTY_F,TRADEDATE_YQF,SYN)%>%
       arrange(TRADEDATE_YQF)
alldata_9 = alldata_9%>% arrange(TRADEDATE_YQF)
alldata_101 =merge(alldata_9, SY_2, by = c("COUNTY_F","TRADEDATE_YQF"),all = T)
alldata_10 = filter(alldata_101, ID!="")
alldata_10_out = filter(alldata_101, is.na(ID)) 


#-----產生BASE(1)檔，用於計算每季交易量-----
alldata_base_1 = select(alldata_10,"ID","IMPORT","IMPORT_F","TRADEDATE"
                        ,"TRADEDATE_F","TRADEDATE_YM","TRADEDATE_YMF","TRADEDATE_YQ"
                        ,"TRADEDATE_YQF","BDFNDATE","BDFNDATE_F","BDFNDATE_YM"
                        ,"BDFNDATE_YMF","BDFNDATE_YQ","BDFNDATE_YQF","AGE_Y"
                        ,"AGE_C","PRESALE","COUNTY_F","COUNTY"
                        ,"COUNTY_OLD_F","COUNTY_OLD","LOCATION_NO","LOCATION"
                        ,"ADDRESS","ADDRESS_C","TRADETARGET","BDTYPE"
                        ,"BDTYPE_F","BDTYPE_C","BDTYPE_G","BDTYPE_K"
                        ,"MAINUSE","USETYPE","MAINMT","TOLFLOOR"
                        ,"TRANSFLOOR","TRADEBDNUM","LAND","BUILD"
                        ,"PARK","ROOM","HALL","TOILET"
                        ,"STRUCTURE","MANAGEMENT","PARKTYPE","PARKTYPE_F"
                        ,"MOTO","RURALTYPE","RURALTYPE_DT","LANDAREA"
                        ,"BDAREA","PARKAREA","TOLPRICE","TOLPARKPRICE"
                        ,"PRICE","OTHER","YCB","YCT"
                        ,"YCC","YCN","SYC","SYN")
alldata_base_1 =arrange(alldata_base_1,TRADEDATE_YQF)

###  Id						/*編號(KEY值)*/
###  import				/*申報年月_離散*/
###  import_f 			/*申報年月_連續*/
###  Tradedate			/*交易年月日_離散*/
###  Tradedate_f		/*交易年月日_連續*/
###  Tradedate_ym 	/*交易年月_離散*/
###  Tradedate_ymf	/*交易年月_連續*/
###  Tradedate_yq		/*交易年季_離散*/
###  Tradedate_yqf	/*交易年季_連續*/
###  Bdfndate			/*建築完成年月日_離散*/
###  Bdfndate_f			/*建築完成年月日_連續*/
###  Bdfndate_ym 		/*建築完成年月_離散*/
###  Bdfndate_ymf		/*建築完成年月_連續*/
###  Bdfndate_yq		/*建築完成年季_離散*/
###  Bdfndate_yqf		/*建築完成年季_連續*/
###  age_y				/*屋齡(年)*/
###  age_c				/*屋齡(年)分組*/
###  presale				/*預售屋*/
###  County_f 			/*新縣市代號*/
###  County 				/*新縣市*/
###  County_old_f 		/*舊縣市代號*/	
###  County_old 		/*舊縣市*/	
###  location_no		/*鄉鎮市區郵政代碼*/
###  Location 			/*鄉鎮市區*/
###  Address				/*土地區段位置/建物區段門牌*/
###  Address_c			/*土地區段位置/建物區段門牌_分類*/
###  Tradetarget 		/*交易標的*/
###  Bdtype				/*建物型態*/
###  Bdtype_f 			/*建物型態_編號*/
###  Bdtype_c			/*建物型態_永慶分類_編號*/
###  Bdtype_g			/*建物型態_永慶分類*/
###  Bdtype_k			/*建物型態_安泰分類*/
###  Mainuse				/*主要用途*/ 
###  Usetype 			/*使用分區或編定/都市土地使用分區*/
###  Mainmt 				/*主要建材*/
###  Tolfloor				/*總樓層數*/
###  Transfloor			/*移轉層次*/
###  Tradebdnum		/*交易筆棟數*/
###  land	 				/*土地*/
###  build 					/*建物*/
###  park	 				/*車位*/
###  Room_f 			/*建物現況格局-房_連續*/
###  Hall_f 				/*建物現況格局-廳_連續*/
###  Toilet_f 				/*建物現況格局-衛_連續*/
###  Structure	 		/*建物現況格局-隔間*/
###  Management	 	/*有無管理組織*/
###  Parktype 			/*車位類別*/
###  Parktype_f			/*車位類別_編號*/
###  moto	 				/*機車車位*/
###  Ruraltype 			/*非都市土地使用分區*/
###  Ruraltype_Dt	 	/*非都市土地使用地*/
###  Landarea 			/*土地移轉總面積(平方公尺)*/
###  Bdarea 				/*建物移轉總面積(平方公尺)*/
###  Parkarea	 		/*車位移轉總面積(平方公尺)*/
###  Tolprice 			/*總價(元)*/
###  Tolparkprice		/*車位總價(元)*/
###  price					/*單價(元/平方公尺)*/
###  Other 				/*備註*/
###  YCB					/*永慶_擔保品*/
###  YCT					/*永慶_舊縣市*/
###  YCC					/*永慶_新縣市*/
###  YCN					/*永慶_全國*/
###  SYC					/*信義_新縣市*/
###  SYN					/*信義_全國*/

#----資料分群，確認[單價]符合官方計算公式----
#####資料不完整案件
delete = filter(alldata_10, (((TOLPRICE==0)|(TOLPARKPRICE>TOLPRICE))
                |((!((TOLPRICE==0)|(TOLPARKPRICE>TOLPRICE)))&((LANDAREA < 1)&(BDAREA < 1)&(PARKAREA < 1)))))
lll = filter(alldata_10, !(((TOLPRICE==0)|(TOLPARKPRICE>TOLPRICE))
                          |((!((TOLPRICE==0)|(TOLPARKPRICE>TOLPRICE)))&((LANDAREA < 1)&(BDAREA < 1)&(PARKAREA < 1)))))
####土地買賣案件，未細分，不使用
land = filter(lll,((TRADETARGET=="土地")))
lll2 = filter(lll,(!(TRADETARGET=="土地")))

##車位買賣案件(無異常)

park1 = filter(lll2,((TOLPRICE==TOLPARKPRICE)
                      |(((TRADETARGET=="車位")
                         |(MAINUSE=="停車空間")
                         |(TRANSFLOOR=="停車場")
                         |(grepl("單獨車位交易", lll2$OTHER)==T)
                         |(grepl("僅購買車位", lll2$OTHER)==T)
                         |(grepl("純車位", lll2$OTHER)==T))
                        &(TOLPARKPRICE==0))
                     |(PARKAREA>=1&BDAREA==PARKAREA&TOLPARKPRICE==0)))
lll3 = filter(lll2,!((TOLPRICE==TOLPARKPRICE)
                     |(((TRADETARGET=="車位")
                        |(MAINUSE=="停車空間")
                        |(TRANSFLOOR=="停車場")
                        |(grepl("單獨車位交易", lll2$OTHER)==T)
                        |(grepl("僅購買車位", lll2$OTHER)==T)
                        |(grepl("純車位", lll2$OTHER)==T))
                       &(TOLPARKPRICE==0))
                     |(PARKAREA>=1&BDAREA==PARKAREA&TOLPARKPRICE==0)))

####車位買賣案件(異常)
park2 = filter(lll3,((TRADETARGET=="車位")
                     |(MAINUSE=="停車空間")
                     |(TRANSFLOOR=="停車場")
                     |(grepl("單獨車位交易", lll3$OTHER)==T)
                     |(grepl("僅購買車位", lll3$OTHER)==T)
                     |(grepl("純車位", lll3$OTHER)==T)
                     &(TOLPARKPRICE!=0)))
lll4 = filter(lll3,!((TRADETARGET=="車位")
                     |(MAINUSE=="停車空間")
                     |(TRANSFLOOR=="停車場")
                     |(grepl("單獨車位交易", lll3$OTHER)==T)
                     |(grepl("僅購買車位", lll3$OTHER)==T)
                     |(grepl("純車位", lll3$OTHER)==T)
                     &(TOLPARKPRICE!=0)))
##特殊擔保品(無異常)
##符合計算公式，建物和車位可分開計算(無異常)
best1 = filter(lll4,(((TRADETARGET=="房地(土地+建物)+車位")
                     |(TRADETARGET=="房地(土地+建物)")
                     |(TRADETARGET=="建物"))
                     &((PARKAREA>=1)&(BDAREA-PARKAREA>=1)&(TOLPARKPRICE>0))
                     &((((TOLPRICE - TOLPARKPRICE)/(BDAREA - PARKAREA)) + 150)>=PRICE)
                     &(PRICE>=(((TOLPRICE - TOLPARKPRICE)/(BDAREA - PARKAREA)) - 150))))
lll5 = filter(lll4,!(((TRADETARGET=="房地(土地+建物)+車位")
                     |(TRADETARGET=="房地(土地+建物)")
                     |(TRADETARGET=="建物"))
                    &((PARKAREA>=1)&(BDAREA-PARKAREA>=1)&(TOLPARKPRICE>0))
                    &((((TOLPRICE - TOLPARKPRICE)/(BDAREA - PARKAREA)) + 150)>=PRICE)
                    &(PRICE>=(((TOLPRICE - TOLPARKPRICE)/(BDAREA - PARKAREA)) - 150))))
##特殊擔保品(無異常)
other1 = filter(lll5,((TRADETARGET=="房地(土地+建物)+車位")
                      |(TRADETARGET=="房地(土地+建物)")
                      |(TRADETARGET=="建物"))
                      &((PARKAREA>=1)&(BDAREA-PARKAREA>=1)&(TOLPARKPRICE>0))
                      &(!((((TOLPRICE - TOLPARKPRICE)/(BDAREA - PARKAREA)) + 150)>=PRICE)
                        &(PRICE>=(((TOLPRICE - TOLPARKPRICE)/(BDAREA - PARKAREA)) - 150)))
                      &(BDTYPE_G=="其他"))
lll6 = filter(lll5,!(((TRADETARGET=="房地(土地+建物)+車位")
                      |(TRADETARGET=="房地(土地+建物)")
                      |(TRADETARGET=="建物"))
                     &((PARKAREA>=1)&(BDAREA-PARKAREA>=1)&(TOLPARKPRICE>0))
                     &(!((((TOLPRICE - TOLPARKPRICE)/(BDAREA - PARKAREA)) + 150)>=PRICE)
                       &(PRICE>=(((TOLPRICE - TOLPARKPRICE)/(BDAREA - PARKAREA)) - 150)))
                     &(BDTYPE_G=="其他")))
##不符合計算公式(異常)
best2 = filter(lll6,(((TRADETARGET=="房地(土地+建物)+車位")
                      |(TRADETARGET=="房地(土地+建物)")
                      |(TRADETARGET=="建物"))
                     &((PARKAREA>=1)&(BDAREA-PARKAREA>=1)&(TOLPARKPRICE>0))
                     &(!((((TOLPRICE - TOLPARKPRICE)/(BDAREA - PARKAREA)) + 150)>=PRICE)
                       &(PRICE>=(((TOLPRICE - TOLPARKPRICE)/(BDAREA - PARKAREA)) - 150)))
                     &(BDTYPE_G!="其他")))
lll7 = filter(lll6,!(((TRADETARGET=="房地(土地+建物)+車位")
                      |(TRADETARGET=="房地(土地+建物)")
                      |(TRADETARGET=="建物"))
                     &((PARKAREA>=1)&(BDAREA-PARKAREA>=1)&(TOLPARKPRICE>0))
                     &(!((((TOLPRICE - TOLPARKPRICE)/(BDAREA - PARKAREA)) + 150)>=PRICE)
                       &(PRICE>=(((TOLPRICE - TOLPARKPRICE)/(BDAREA - PARKAREA)) - 150)))
                     &(BDTYPE_G!="其他")))
##符合計算公式，純建物交易(無異常)
build1 = filter(lll7,(((TRADETARGET=="房地(土地+建物)+車位")
                       |(TRADETARGET=="房地(土地+建物)")
                       |(TRADETARGET=="建物"))
                      &(BDAREA>=1)
                      &((((TOLPRICE/BDAREA) + 150) >= PRICE)
                        &(PRICE >= ((TOLPRICE/BDAREA) - 150)))
                      &((TRADETARGET!="房地(土地+建物)+車位")
                        &(PARKTYPE=="")&(PARK==0)&(TOLPARKPRICE==0)&(PARKAREA==0))))
lll8 = filter(lll7,!(((TRADETARGET=="房地(土地+建物)+車位")
                      |(TRADETARGET=="房地(土地+建物)")
                      |(TRADETARGET=="建物"))
                     &(BDAREA>=1)
                     &((((TOLPRICE/BDAREA) + 150) >= PRICE)
                       &(PRICE >= ((TOLPRICE/BDAREA) - 150)))
                     &((TRADETARGET!="房地(土地+建物)+車位")
                       &(PARKTYPE=="")&(PARK==0)&(TOLPARKPRICE==0)&(PARKAREA==0))))
##符合計算公式，建物和車位無法分開計算(無異常)
mix1 = filter(lll8,(((TRADETARGET=="房地(土地+建物)+車位")
                     |(TRADETARGET=="房地(土地+建物)")
                     |(TRADETARGET=="建物"))
                    &(BDAREA>=1)
                    &((((TOLPRICE/BDAREA) + 150) >= PRICE)
                      &(PRICE >= ((TOLPRICE/BDAREA) - 150)))
                    &(!((TRADETARGET!="房地(土地+建物)+車位")
                      &(PARKTYPE=="")&(PARK==0)&(TOLPARKPRICE==0)&(PARKAREA==0)))))
lll9 = filter(lll8,!(((TRADETARGET=="房地(土地+建物)+車位")
                     |(TRADETARGET=="房地(土地+建物)")
                     |(TRADETARGET=="建物"))
                    &(BDAREA>=1)
                    &((((TOLPRICE/BDAREA) + 150) >= PRICE)
                      &(PRICE >= ((TOLPRICE/BDAREA) - 150)))
                    &(!((TRADETARGET!="房地(土地+建物)+車位")
                        &(PARKTYPE=="")&(PARK==0)&(TOLPARKPRICE==0)&(PARKAREA==0)))))
other2 = filter(lll9,(((TRADETARGET=="房地(土地+建物)+車位")
                       |(TRADETARGET=="房地(土地+建物)")
                       |(TRADETARGET=="建物"))
                      &(BDAREA>=1)
                      &(BDTYPE_G=="其他")))
lll10 = filter(lll9,(!(((TRADETARGET=="房地(土地+建物)+車位")
                        |(TRADETARGET=="房地(土地+建物)")
                        |(TRADETARGET=="建物"))
                       &(BDAREA>=1)
                       &(BDTYPE_G=="其他"))))
##不符合計算公式(異常) 
build21 = filter(lll10,(((TRADETARGET=="房地(土地+建物)+車位")
                        |(TRADETARGET=="房地(土地+建物)")
                        |(TRADETARGET=="建物"))
                       &(BDAREA>=1)
                       &((TRADETARGET!="房地(土地+建物)+車位")
                         &(PARKTYPE=="")&(PARK==0)&(TOLPARKPRICE==0)&(PARKAREA==0))))
lll11 = filter(lll10,(!(((TRADETARGET=="房地(土地+建物)+車位")
                         |(TRADETARGET=="房地(土地+建物)")
                         |(TRADETARGET=="建物"))
                        &(BDAREA>=1)
                        &((TRADETARGET!="房地(土地+建物)+車位")
                          &(PARKTYPE=="")&(PARK==0)&(TOLPARKPRICE==0)&(PARKAREA==0)))))
mix2 = filter(lll11,(((TRADETARGET=="房地(土地+建物)+車位")
                      |(TRADETARGET=="房地(土地+建物)")
                      |(TRADETARGET=="建物"))
                     &(BDAREA>=1)
                     &(!((((TOLPRICE/BDAREA) + 150) >= PRICE)
                         &(PRICE >= ((TOLPRICE/BDAREA) - 150))))
                     &(BDTYPE_G!="其他")
                     &(!((TRADETARGET!="房地(土地+建物)+車位")
                       &(PARKTYPE=="")&(PARK==0)&(TOLPARKPRICE==0)&(PARKAREA==0)))))
lll12 = filter(lll11,(!(((TRADETARGET=="房地(土地+建物)+車位")
                         |(TRADETARGET=="房地(土地+建物)")
                         |(TRADETARGET=="建物"))
                        &(BDAREA>=1)
                        &(!((((TOLPRICE/BDAREA) + 150) >= PRICE)
                            &(PRICE >= ((TOLPRICE/BDAREA) - 150))))
                        &(BDTYPE_G!="其他")
                        &(!((TRADETARGET!="房地(土地+建物)+車位")
                            &(PARKTYPE=="")&(PARK==0)&(TOLPARKPRICE==0)&(PARKAREA==0))))))

other3 = filter(lll12,(((TRADETARGET=="房地(土地+建物)+車位")
                       |(TRADETARGET=="房地(土地+建物)")
                       |(TRADETARGET=="建物"))
                      &(BDTYPE_G=="其他")))
lll13 = filter(lll12,(!(((TRADETARGET=="房地(土地+建物)+車位")
                        |(TRADETARGET=="房地(土地+建物)")
                        |(TRADETARGET=="建物"))
                       &(BDTYPE_G=="其他"))))
##不符合計算公式(異常) 
build22 = filter(lll13,(((TRADETARGET=="房地(土地+建物)+車位")
                         |(TRADETARGET=="房地(土地+建物)")
                         |(TRADETARGET=="建物"))
                        &((TRADETARGET!="房地(土地+建物)+車位")
                          &(PARKTYPE=="")&(PARK==0)&(TOLPARKPRICE==0)&(PARKAREA==0))))
build2 = rbind(build21,build22)

check = filter(lll13,(!(((TRADETARGET=="房地(土地+建物)+車位")
                         |(TRADETARGET=="房地(土地+建物)")
                         |(TRADETARGET=="建物"))
                        &((TRADETARGET!="房地(土地+建物)+車位")
                          &(PARKTYPE=="")&(PARK==0)&(TOLPARKPRICE==0)&(PARKAREA==0)))))
other = rbind(other1,other2,other3)
#------------------------------------------
best = best1
best$BUILDPRICE = as.numeric(round((best$PRICE)/(0.3025)))
best$MIXPRICE = as.numeric(round((best$TOLPRICE/best$BDAREA)/(0.3025)))
best$PARKPRICE = as.numeric(round((best$TOLPARKPRICE/best$PARKAREA)/(0.3025)))

mix = mix1
mix$BUILDPRICE = 0
mix$MIXPRICE = as.numeric(round(mix1$PRICE/(0.3025)))
mix$PARKPRICE = 0

build = build1
build$BUILDPRICE = as.numeric(round(build1$PRICE/(0.3025)))
build$MIXPRICE = 0
build$PARKPRICE = 0

park = park1
park$BUILDPRICE = 0
park$MIXPRICE = 0
park$PARKPRICE = -1

##export(best,"C:/Users/Eric/Desktop/SARA/M50/output_r_excel/best.xlsx")
##export(park,"C:/Users/Eric/Desktop/SARA/M50/output_r_excel/park.xlsx")
##export(build,"C:/Users/Eric/Desktop/SARA/M50/output_r_excel/build.xlsx")
##export(mix,"C:/Users/Eric/Desktop/SARA/M50/output_r_excel/mix.xlsx")

##-----彙總best、mix、build(含建物交易之有效樣本)，產出資料BASE(2)檔-----
alldata_base_2 = rbind(build,mix,best)

alldata_base_2 = select(alldata_base_2
                        ,ID,IMPORT,IMPORT_F,TRADEDATE,TRADEDATE_F,TRADEDATE_YM,TRADEDATE_YMF,TRADEDATE_YQ
                        ,TRADEDATE_YQF,BDFNDATE,BDFNDATE_F,BDFNDATE_YM,BDFNDATE_YMF,BDFNDATE_YQ,BDFNDATE_YQF,AGE_Y
                        ,AGE_C,PRESALE,COUNTY_F,COUNTY,COUNTY_OLD_F,COUNTY_OLD,LOCATION_NO,LOCATION
                        ,ADDRESS,ADDRESS_C,TRADETARGET,BDTYPE,BDTYPE_F,BDTYPE_C,BDTYPE_G,BDTYPE_K
                        ,MAINUSE,USETYPE,MAINMT,TOLFLOOR,TRANSFLOOR,TRADEBDNUM,LAND,BUILD
                        ,PARK,ROOM,HALL,TOILET,STRUCTURE,MANAGEMENT,PARKTYPE,PARKTYPE_F
                        ,MOTO,RURALTYPE,RURALTYPE_DT,LANDAREA,BDAREA,PARKAREA,TOLPRICE,TOLPARKPRICE
                        ,PRICE,BUILDPRICE,MIXPRICE,PARKPRICE,OTHER,YCB,YCT,YCC
                        ,YCN,SYC,SYN)
alldata_base_2 = arrange(alldata_base_2,TRADEDATE_YQF)

saveRDS(alldata_base_1,"C:/Users/Eric/Desktop/SARA/rds/ALLDATA_BASE_1.rds")
saveRDS(alldata_base_2,"C:/Users/Eric/Desktop/SARA/rds/ALLDATA_BASE_2.rds")

##-------------------- 程式結束 --------------------
  





