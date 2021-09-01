
library(xlsx)
library(dplyr)
library(stringr)
  
## 讀入xls & 輸出saveRDS
importFt = function(filename){
  #filename = "./opendata/data_20180202/b_lvr_land_a.xls"
  A_lvr_land_A = read.xlsx2(filename 
                            ,3 , startRow = 3, header = F 
                            ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                            ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                            ,stringsAsFactors= F )
  #欄位名稱
  colnames(A_lvr_land_A) = c("LOCATION","TRADETARGET","ADDRESS","LANDAREA","USETYPE","RURALTYPE","RURALTYPE_DT","TRADEDATE","TRADEBDNUM","TRANSFLOOR",
                             "TOLFLOOR","BDTYPE","MAINUSE","MAINMT","BDFNDATE","BDAREA","ROOM","HALL","TOILET","STRUCTURE",
                             "MANAGEMENT","TOLPRICE","PRICE","PARKTYPE","PARKAREA","TOLPARKPRICE","OTHER","ID")
  A_lvr_land_A$IMPORT = substr(filename, 17, 24)
  A_lvr_land_A$COUNTY_F = toupper(substr(filename, 26, 26))
    
  #save data 
  dataarea = substr(filename, 26, 26)
  datadate = substr(filename, 17, 24)
  filename = paste("./rds/read/", dataarea, "_", datadate, ".rds", sep="")
  saveRDS(A_lvr_land_A, file = filename)
 
}

#使用函數
aaa = dir("./opendata/data_20180200/",pattern = "_a.xls")
aa = dir("./opendata/")
a = merge(aa,aaa)
S = filter(a,substr(a$y,1,1)=="a"|substr(a$y,1,1)=="b"|substr(a$y,1,1)=="c"|substr(a$y,1,1)=="d"
           |substr(a$y,1,1)=="e"|substr(a$y,1,1)=="f"|substr(a$y,1,1)=="g"|substr(a$y,1,1)=="h"
           |substr(a$y,1,1)=="i"|substr(a$y,1,1)=="j"|substr(a$y,1,1)=="n"|substr(a$y,1,1)=="o"|substr(a$y,1,1)=="q")
A = paste("./opendata/",S$x,"/",S$y,sep = "")
for (i in 1:length(A)){
filename = A[i]
importFt(filename)
}

## ---- A縣市合併 ----
path = "./rds/read"
files = list.files(path = path, pattern = "a_")

DATA_A = data.frame()
ALLDATA_A = data.frame()
for(file in files) {
  DATA_A = readRDS(paste(path,"/", file, sep=""))
  ALLDATA_A = rbind(ALLDATA_A, DATA_A)
}
  ALLDATA_A = ALLDATA_A%>% arrange(ID,desc(IMPORT))
  ALLDATA_A = ALLDATA_A[!duplicated(ALLDATA_A[,c("ID")]),]

saveRDS(ALLDATA_A,"C:/Users/Eric/Desktop/SARA/rds/merge/ALLDATA_A.rds")

## ---- B縣市合併 ----
path = "./rds/read"
files = list.files(path = path, pattern = "b_")

DATA_B = data.frame()
ALLDATA_B = data.frame()
for(file in files) {
  DATA_B = readRDS(paste(path,"/", file, sep=""))
  ALLDATA_B = rbind(ALLDATA_B, DATA_B)
}
  ALLDATA_B = ALLDATA_B%>%arrange(ID,desc(IMPORT))
  ALLDATA_B = ALLDATA_B[!duplicated(ALLDATA_B[,c("ID")]),]

saveRDS(ALLDATA_B,"C:/Users/Eric/Desktop/SARA/rds/merge/ALLDATA_B.rds")

## ---- C縣市合併 ----
path = "./rds/read"
files = list.files(path = path, pattern = "c_")

DATA_C = data.frame()
ALLDATA_C = data.frame()
for(file in files) {
  DATA_C = readRDS(paste(path,"/", file, sep=""))
  ALLDATA_C = rbind(ALLDATA_C, DATA_C)
}
  ALLDATA_C = ALLDATA_C%>%arrange(ID,desc(IMPORT))
  ALLDATA_C = ALLDATA_C[!duplicated(ALLDATA_C[,c("ID")]),]
saveRDS(ALLDATA_C,"C:/Users/Eric/Desktop/SARA/rds/merge/ALLDATA_C.rds")

## ---- D縣市合併 ----
path = "./rds/read"
files = list.files(path = path, pattern = "d_")

DATA_D = data.frame()
ALLDATA_D = data.frame()
for(file in files) {
  DATA_D = readRDS(paste(path,"/", file, sep=""))
  ALLDATA_D = rbind(ALLDATA_D, DATA_D)
}
  ALLDATA_D = ALLDATA_D%>%arrange(ID,desc(IMPORT))
  ALLDATA_D = ALLDATA_D[!duplicated(ALLDATA_D[,c("ID")]),]
saveRDS(ALLDATA_D,"C:/Users/Eric/Desktop/SARA/rds/merge/ALLDATA_D.rds")

## ---- E縣市合併 ----
path = "./rds/read"
files = list.files(path = path, pattern = "e_")

DATA_E = data.frame()
ALLDATA_E = data.frame()
for(file in files) {
  DATA_E = readRDS(paste(path,"/", file, sep=""))
  ALLDATA_E = rbind(ALLDATA_E, DATA_E)
}
  ALLDATA_E = ALLDATA_E%>%arrange(ID,desc(IMPORT))
  ALLDATA_E = ALLDATA_E[!duplicated(ALLDATA_E[,c("ID")]),]

saveRDS(ALLDATA_E,"C:/Users/Eric/Desktop/SARA/rds/merge/ALLDATA_E.rds")

## ---- F縣市合併 ----
path = "./rds/read"
files = list.files(path = path, pattern = "f_")

DATA_F = data.frame()
ALLDATA_F = data.frame()
for(file in files) {
  DATA_F = readRDS(paste(path,"/", file, sep=""))
  ALLDATA_F = rbind(ALLDATA_F, DATA_F)
} 
  ALLDATA_F = ALLDATA_F%>%arrange(ID,desc(IMPORT))
  ALLDATA_F = ALLDATA_F[!duplicated(ALLDATA_F[,c("ID")]),]

saveRDS(ALLDATA_F,"C:/Users/Eric/Desktop/SARA/rds/merge/ALLDATA_F.rds")

## ---- G縣市合併 ----
path = "./rds/read"
files = list.files(path = path, pattern = "g_")

DATA_G = data.frame()
ALLDATA_G = data.frame()
for(file in files) {
  DATA_G = readRDS(paste(path,"/", file, sep=""))
  ALLDATA_G = rbind(ALLDATA_G, DATA_G)
}
  ALLDATA_G = ALLDATA_G%>%arrange(ID,desc(IMPORT))
  ALLDATA_G = ALLDATA_G[!duplicated(ALLDATA_G[,c("ID")]),]

saveRDS(ALLDATA_G,"C:/Users/Eric/Desktop/SARA/rds/merge/ALLDATA_G.rds")

## ---- H縣市合併 ----
path = "./rds/read"
files = list.files(path = path, pattern = "h_")

DATA_H = data.frame()
ALLDATA_H = data.frame()
for(file in files) {
  DATA_H = readRDS(paste(path,"/", file, sep=""))
  ALLDATA_H = rbind(ALLDATA_H, DATA_H)
}  
  ALLDATA_H = ALLDATA_H%>%arrange(ID,desc(IMPORT))
  ALLDATA_H = ALLDATA_H[!duplicated(ALLDATA_H[,c("ID")]),]

saveRDS(ALLDATA_H,"C:/Users/Eric/Desktop/SARA/rds/merge/ALLDATA_H.rds")

## ---- I縣市合併 ----
path = "./rds/read"
files = list.files(path = path, pattern = "i_")

DATA_I = data.frame()
ALLDATA_I = data.frame()
for(file in files) {
  DATA_I = readRDS(paste(path,"/", file, sep=""))
  ALLDATA_I = rbind(ALLDATA_I, DATA_I)
}
  ALLDATA_I = ALLDATA_I %>% arrange(ID,desc(IMPORT))
  ALLDATA_I = ALLDATA_I[!duplicated(ALLDATA_I[,c("ID")]),]

saveRDS(ALLDATA_I,"C:/Users/Eric/Desktop/SARA/rds/merge/ALLDATA_I.rds")

## ---- J縣市合併 ----
path = "./rds/read"
files = list.files(path = path, pattern = "j_")

DATA_J = data.frame()
ALLDATA_J = data.frame()
for(file in files) {
  DATA_J = readRDS(paste(path,"/", file, sep=""))
  ALLDATA_J = rbind(ALLDATA_J, DATA_J)
}
  ALLDATA_J = ALLDATA_J%>%arrange(ID,desc(IMPORT))
  ALLDATA_J = ALLDATA_J[!duplicated(ALLDATA_J[,c("ID")]),]

saveRDS(ALLDATA_J,"C:/Users/Eric/Desktop/SARA/rds/merge/ALLDATA_J.rds")

## ---- N縣市合併 ----
path = "./rds/read"
files = list.files(path = path, pattern = "n_")

DATA_N = data.frame()
ALLDATA_N = data.frame()
for(file in files) {
  DATA_N = readRDS(paste(path,"/", file, sep=""))
  ALLDATA_N = rbind(ALLDATA_N, DATA_N)
}
  ALLDATA_N = ALLDATA_N%>%arrange(ID,desc(IMPORT))
  ALLDATA_N = ALLDATA_N[!duplicated(ALLDATA_N[,c("ID")]),]

saveRDS(ALLDATA_N,"C:/Users/Eric/Desktop/SARA/rds/merge/ALLDATA_N.rds")


## ---- O縣市合併 ----
path = "./rds/read"
files = list.files(path = path, pattern = "o_")

DATA_O = data.frame()
ALLDATA_O = data.frame()
for(file in files) {
  DATA_O = readRDS(paste(path,"/", file, sep=""))
  ALLDATA_O = rbind(ALLDATA_O, DATA_O)
}
  ALLDATA_O = ALLDATA_O%>%arrange(ID,desc(IMPORT))
  ALLDATA_O = ALLDATA_O[!duplicated(ALLDATA_O[,c("ID")]),]
saveRDS(ALLDATA_O,"C:/Users/Eric/Desktop/SARA/rds/merge/ALLDATA_O.rds")

## ---- Q縣市合併 ----
path = "./rds/read"
files = list.files(path = path, pattern = "q_")

DATA_Q = data.frame()
ALLDATA_Q = data.frame()
for(file in files) {
  DATA_Q = readRDS(paste(path,"/", file, sep=""))
  ALLDATA_Q = rbind(ALLDATA_Q, DATA_Q)
} 
  ALLDATA_Q = ALLDATA_Q %>% arrange(ID,desc(IMPORT))
  ALLDATA_Q = ALLDATA_Q[!duplicated(ALLDATA_Q[,c("ID")]),]

saveRDS(ALLDATA_Q,"C:/Users/Eric/Desktop/SARA/rds/merge/ALLDATA_Q.rds")

## ---- 全部縣市合併 ----
path = "./rds/merge"
files = list.files(path = path, pattern = "ALLDATA_")

ALLDATA = data.frame()
ALLDATA_BASE = data.frame()
for(file in files) {
  ALLDATA = readRDS(paste(path,"/", file, sep=""))
  ALLDATA_BASE = rbind(ALLDATA_BASE, ALLDATA)
}
  ALLDATA_BASE = ALLDATA_BASE %>% arrange(ID,desc(IMPORT))
  ALLDATA_BASE = ALLDATA_BASE[!duplicated(ALLDATA_BASE[,c("ID")]),]

saveRDS(ALLDATA_BASE,"C:/Users/Eric/Desktop/SARA/rds/ALLDATA_BASE.rds")

