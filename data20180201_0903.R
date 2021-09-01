#install.packages("StatMeasures")
library(xlsx)
library(dplyr)
library(StatMeasures)

#----匯入資料----
#---------A縣市---------
A_20180201 = read.xlsx2("./opendata/data_20180201/A_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180201" ,COUNTY_F="A")
A_20180202 = read.xlsx2("./opendata/data_20180202/A_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180202" ,COUNTY_F="A")
A_20180502 = read.xlsx2("./opendata/data_20180502/A_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180502" ,COUNTY_F="A")
A_20180802 = read.xlsx2("./opendata/data_20180802/A_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180802" ,COUNTY_F="A")
A_20180803 = read.xlsx2("./opendata/data_20180803/A_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180803" ,COUNTY_F="A")
A_20180901 = read.xlsx2("./opendata/data_20180901/A_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180901" ,COUNTY_F="A")
A_20180902 = read.xlsx2("./opendata/data_20180902/A_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180902" ,COUNTY_F="A")
A_20180903 = read.xlsx2("./opendata/data_20180903/A_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180903" ,COUNTY_F="A")

#---------B縣市---------
B_20180201 = read.xlsx2("./opendata/data_20180201/B_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180201" ,COUNTY_F="B")
B_20180202 = read.xlsx2("./opendata/data_20180202/B_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180202" ,COUNTY_F="B")
B_20180502 = read.xlsx2("./opendata/data_20180502/B_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180502" ,COUNTY_F="B")
B_20180802 = read.xlsx2("./opendata/data_20180802/B_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180802" ,COUNTY_F="B")
B_20180803 = read.xlsx2("./opendata/data_20180803/B_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180803" ,COUNTY_F="B")
B_20180901 = read.xlsx2("./opendata/data_20180901/B_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180901" ,COUNTY_F="B")
B_20180902 = read.xlsx2("./opendata/data_20180902/B_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180902" ,COUNTY_F="B")
B_20180903 = read.xlsx2("./opendata/data_20180903/B_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180903" ,COUNTY_F="B")

#---------C縣市---------
C_20180201 = read.xlsx2("./opendata/data_20180201/C_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180201" ,COUNTY_F="C")
C_20180202 = read.xlsx2("./opendata/data_20180202/C_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180202" ,COUNTY_F="C")
C_20180502 = read.xlsx2("./opendata/data_20180502/C_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180502" ,COUNTY_F="C")
C_20180802 = read.xlsx2("./opendata/data_20180802/C_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180802" ,COUNTY_F="C")
C_20180803 = read.xlsx2("./opendata/data_20180803/C_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180803" ,COUNTY_F="C")
C_20180901 = read.xlsx2("./opendata/data_20180901/C_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180901" ,COUNTY_F="C")
C_20180902 = read.xlsx2("./opendata/data_20180902/C_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180902" ,COUNTY_F="C")
C_20180903 = read.xlsx2("./opendata/data_20180903/C_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180903" ,COUNTY_F="C")
#---------D縣市---------
D_20180201 = read.xlsx2("./opendata/data_20180201/D_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180201" ,COUNTY_F="D")
D_20180202 = read.xlsx2("./opendata/data_20180202/D_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180202" ,COUNTY_F="D")
D_20180502 = read.xlsx2("./opendata/data_20180502/D_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180502" ,COUNTY_F="D")
D_20180802 = read.xlsx2("./opendata/data_20180802/D_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180802" ,COUNTY_F="D")
D_20180803 = read.xlsx2("./opendata/data_20180803/D_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180803" ,COUNTY_F="D")
D_20180901 = read.xlsx2("./opendata/data_20180901/D_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180901" ,COUNTY_F="D")
D_20180902 = read.xlsx2("./opendata/data_20180902/D_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180902" ,COUNTY_F="D")
D_20180903 = read.xlsx2("./opendata/data_20180903/D_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180903" ,COUNTY_F="D")
#---------E縣市---------
E_20180201 = read.xlsx2("./opendata/data_20180201/E_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180201" ,COUNTY_F="E")
E_20180202 = read.xlsx2("./opendata/data_20180202/E_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180202" ,COUNTY_F="E")
E_20180502 = read.xlsx2("./opendata/data_20180502/E_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180502" ,COUNTY_F="E")
E_20180802 = read.xlsx2("./opendata/data_20180802/E_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180802" ,COUNTY_F="E")
E_20180803 = read.xlsx2("./opendata/data_20180803/E_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180803" ,COUNTY_F="E")
E_20180901 = read.xlsx2("./opendata/data_20180901/E_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180901" ,COUNTY_F="E")
E_20180902 = read.xlsx2("./opendata/data_20180902/E_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180902" ,COUNTY_F="E")
E_20180903 = read.xlsx2("./opendata/data_20180903/E_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180903" ,COUNTY_F="E")
#---------F縣市---------
F_20180201 = read.xlsx2("./opendata/data_20180201/F_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180201" ,COUNTY_F="F")
F_20180202 = read.xlsx2("./opendata/data_20180202/F_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180202" ,COUNTY_F="F")
F_20180502 = read.xlsx2("./opendata/data_20180502/F_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180502" ,COUNTY_F="F")
F_20180802 = read.xlsx2("./opendata/data_20180802/F_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180802" ,COUNTY_F="F")
F_20180803 = read.xlsx2("./opendata/data_20180803/F_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180803" ,COUNTY_F="F")
F_20180901 = read.xlsx2("./opendata/data_20180901/F_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180901" ,COUNTY_F="F")
F_20180902 = read.xlsx2("./opendata/data_20180902/F_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180902" ,COUNTY_F="F")
F_20180903 = read.xlsx2("./opendata/data_20180903/F_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180903" ,COUNTY_F="F")
#---------G縣市---------
G_20180201 = read.xlsx2("./opendata/data_20180201/G_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180201" ,COUNTY_F="G")
G_20180202 = read.xlsx2("./opendata/data_20180202/G_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180202" ,COUNTY_F="G")
G_20180502 = read.xlsx2("./opendata/data_20180502/G_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180502" ,COUNTY_F="G")
G_20180802 = read.xlsx2("./opendata/data_20180802/G_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180802" ,COUNTY_F="G")
G_20180803 = read.xlsx2("./opendata/data_20180803/G_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180803" ,COUNTY_F="G")
G_20180901 = read.xlsx2("./opendata/data_20180901/G_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180901" ,COUNTY_F="G")
G_20180902 = read.xlsx2("./opendata/data_20180902/G_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180902" ,COUNTY_F="G")
G_20180903 = read.xlsx2("./opendata/data_20180903/G_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180903" ,COUNTY_F="G")
#---------H縣市---------
H_20180201 = read.xlsx2("./opendata/data_20180201/H_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180201" ,COUNTY_F="H")
H_20180202 = read.xlsx2("./opendata/data_20180202/H_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180202" ,COUNTY_F="H")
H_20180502 = read.xlsx2("./opendata/data_20180502/H_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180502" ,COUNTY_F="H")
H_20180802 = read.xlsx2("./opendata/data_20180802/H_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180802" ,COUNTY_F="H")
H_20180803 = read.xlsx2("./opendata/data_20180803/H_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180803" ,COUNTY_F="H")
H_20180901 = read.xlsx2("./opendata/data_20180901/H_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180901" ,COUNTY_F="H")
H_20180902 = read.xlsx2("./opendata/data_20180902/H_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180902" ,COUNTY_F="H")
H_20180903 = read.xlsx2("./opendata/data_20180903/H_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180903" ,COUNTY_F="H")
#---------I縣市---------
I_20180201 = read.xlsx2("./opendata/data_20180201/I_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180201" ,COUNTY_F="I")
I_20180202 = read.xlsx2("./opendata/data_20180202/I_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180202" ,COUNTY_F="I")
I_20180502 = read.xlsx2("./opendata/data_20180502/I_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180502" ,COUNTY_F="I")
I_20180802 = read.xlsx2("./opendata/data_20180802/I_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180802" ,COUNTY_F="I")
I_20180803 = read.xlsx2("./opendata/data_20180803/I_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180803" ,COUNTY_F="I")
I_20180901 = read.xlsx2("./opendata/data_20180901/I_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180901" ,COUNTY_F="I")
I_20180902 = read.xlsx2("./opendata/data_20180902/I_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180902" ,COUNTY_F="I")
I_20180903 = read.xlsx2("./opendata/data_20180903/I_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180903" ,COUNTY_F="I")
#---------J縣市---------
J_20180201 = read.xlsx2("./opendata/data_20180201/J_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180201" ,COUNTY_F="J")
J_20180202 = read.xlsx2("./opendata/data_20180202/J_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180202" ,COUNTY_F="J")
J_20180502 = read.xlsx2("./opendata/data_20180502/J_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180502" ,COUNTY_F="J")
J_20180802 = read.xlsx2("./opendata/data_20180802/J_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180802" ,COUNTY_F="J")
J_20180803 = read.xlsx2("./opendata/data_20180803/J_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180803" ,COUNTY_F="J")
J_20180901 = read.xlsx2("./opendata/data_20180901/J_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180901" ,COUNTY_F="J")
J_20180902 = read.xlsx2("./opendata/data_20180902/J_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180902" ,COUNTY_F="J")
J_20180903 = read.xlsx2("./opendata/data_20180903/J_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180903" ,COUNTY_F="J")
#---------N縣市---------
N_20180201 = read.xlsx2("./opendata/data_20180201/N_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180201" ,COUNTY_F="N")
N_20180202 = read.xlsx2("./opendata/data_20180202/N_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180202" ,COUNTY_F="N")
N_20180502 = read.xlsx2("./opendata/data_20180502/N_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180502" ,COUNTY_F="N")
N_20180802 = read.xlsx2("./opendata/data_20180802/N_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180802" ,COUNTY_F="N")
N_20180803 = read.xlsx2("./opendata/data_20180803/N_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180803" ,COUNTY_F="N")
N_20180901 = read.xlsx2("./opendata/data_20180901/N_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180901" ,COUNTY_F="N")
N_20180902 = read.xlsx2("./opendata/data_20180902/N_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180902" ,COUNTY_F="N")
N_20180903 = read.xlsx2("./opendata/data_20180903/N_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180903" ,COUNTY_F="N")
#---------O縣市---------

O_20180201 = read.xlsx2("./opendata/data_20180201/O_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180201" ,COUNTY_F="O")
O_20180202 = read.xlsx2("./opendata/data_20180202/O_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180202" ,COUNTY_F="O")
O_20180502 = read.xlsx2("./opendata/data_20180502/O_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180502" ,COUNTY_F="O")
O_20180802 = read.xlsx2("./opendata/data_20180802/O_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180802" ,COUNTY_F="O")
O_20180803 = read.xlsx2("./opendata/data_20180803/O_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180803" ,COUNTY_F="O")
O_20180901 = read.xlsx2("./opendata/data_20180901/O_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180901" ,COUNTY_F="O")
O_20180902 = read.xlsx2("./opendata/data_20180902/O_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180902" ,COUNTY_F="O")
O_20180903 = read.xlsx2("./opendata/data_20180903/O_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180903" ,COUNTY_F="O")
#---------Q縣市---------
Q_20180201 = read.xlsx2("./opendata/data_20180201/Q_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180201" ,COUNTY_F="Q")
Q_20180202 = read.xlsx2("./opendata/data_20180202/Q_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180202" ,COUNTY_F="Q")
Q_20180502 = read.xlsx2("./opendata/data_20180502/Q_LVR_LAND_A.XLS" 
                        ,3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180502" ,COUNTY_F="Q")
Q_20180802 = read.xlsx2("./opendata/data_20180802/Q_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180802" ,COUNTY_F="Q")
Q_20180803 = read.xlsx2("./opendata/data_20180803/Q_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180803" ,COUNTY_F="Q")
Q_20180901 = read.xlsx2("./opendata/data_20180901/Q_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180901" ,COUNTY_F="Q")
Q_20180902 = read.xlsx2("./opendata/data_20180902/Q_LVR_LAND_A.XLS" 
                        , 3 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F,IMPORT="20180902" ,COUNTY_F="Q")
Q_20180903 = read.xlsx2("./opendata/data_20180903/Q_LVR_LAND_A.XLS" 
                        , 2 , startRow = 3, header = F 
                        ,colClasses = c("character","character","character","numeric","character","character","character","character","character","character","character","character","character","character","character","numeric"
                                        ,"numeric","numeric","numeric","character","character","numeric","numeric","character","numeric","numeric","character","character")
                        ,stringsAsFactors= F ,IMPORT="20180903" ,COUNTY_F="Q")

#----合併A縣市資料----

A_20180201_0903 = rbind(A_20180201,A_20180202,A_20180502,A_20180802,A_20180803,A_20180901,A_20180902,A_20180903)
                          
colnames(A_20180201_0903) = c("LOCATION","TRADETARGET","ADDRESS","LANDAREA","USETYPE","RURALTYPE","RURALTYPE_DT","TRADEDATE","TRADEBDNUM","TRANSFLOOR",
                              "TOLFLOOR","BDTYPE","MAINUSE","MAINMT","BDFNDATE","BDAREA","ROOM","HALL","TOILET","STRUCTURE",
                              "MANAGEMENT","TOLPRICE","PRICE","PARKTYPE","PARKAREA","TOLPARKPRICE","OTHER","ID","IMPORT","COUNTY_F")

A_20180201_0903 = arrange(A_20180201_0903,ID,desc(IMPORT))
A_20180201_0903 = A_20180201_0903[!duplicated(A_20180201_0903[,c("ID")]),]

saveRDS(A_20180201_0903,"C:/Users/Eric/Desktop/SARA/rds/A_20180201_0903.rds")

#----合併B縣市資料----
B_20180201_0903 = rbind(B_20180201,B_20180202,B_20180502,B_20180802,B_20180803,B_20180901,B_20180902,B_20180903)

colnames(B_20180201_0903) = c("LOCATION","TRADETARGET","ADDRESS","LANDAREA","USETYPE","RURALTYPE","RURALTYPE_DT","TRADEDATE","TRADEBDNUM","TRANSFLOOR",
                              "TOLFLOOR","BDTYPE","MAINUSE","MAINMT","BDFNDATE","BDAREA","ROOM","HALL","TOILET","STRUCTURE",
                              "MANAGEMENT","TOLPRICE","PRICE","PARKTYPE","PARKAREA","TOLPARKPRICE","OTHER","ID","IMPORT","COUNTY_F")
B_20180201_0903 = arrange(B_20180201_0903,ID,desc(IMPORT))
B_20180201_0903 = B_20180201_0903[!duplicated(B_20180201_0903[,c("ID")]),]

saveRDS(B_20180201_0903,"C:/Users/Eric/Desktop/SARA/rds/B_20180201_0903.rds")

#----合併c縣市資料----
C_20180201_0903 = rbind(C_20180201,C_20180202,C_20180502,C_20180802,C_20180803,C_20180901,C_20180902,C_20180903)

colnames(C_20180201_0903) = c("LOCATION","TRADETARGET","ADDRESS","LANDAREA","USETYPE","RURALTYPE","RURALTYPE_DT","TRADEDATE","TRADEBDNUM","TRANSFLOOR",
                              "TOLFLOOR","BDTYPE","MAINUSE","MAINMT","BDFNDATE","BDAREA","ROOM","HALL","TOILET","STRUCTURE",
                              "MANAGEMENT","TOLPRICE","PRICE","PARKTYPE","PARKAREA","TOLPARKPRICE","OTHER","ID","IMPORT","COUNTY_F")
C_20180201_0903 = arrange(C_20180201_0903,ID,desc(IMPORT))
C_20180201_0903 = C_20180201_0903[!duplicated(C_20180201_0903[,c("ID")]),]

saveRDS(C_20180201_0903,"C:/Users/Eric/Desktop/SARA/rds/C_20180201_0903.rds")

#----合併D縣市資料----
D_20180201_0903 = rbind(D_20180201,D_20180202,D_20180502,D_20180802,D_20180803,D_20180901,D_20180902,D_20180903)

colnames(D_20180201_0903) = c("LOCATION","TRADETARGET","ADDRESS","LANDAREA","USETYPE","RURALTYPE","RURALTYPE_DT","TRADEDATE","TRADEBDNUM","TRANSFLOOR",
                              "TOLFLOOR","BDTYPE","MAINUSE","MAINMT","BDFNDATE","BDAREA","ROOM","HALL","TOILET","STRUCTURE",
                              "MANAGEMENT","TOLPRICE","PRICE","PARKTYPE","PARKAREA","TOLPARKPRICE","OTHER","ID","IMPORT","COUNTY_F")
D_20180201_0903 = arrange(D_20180201_0903,ID,desc(IMPORT))
D_20180201_0903 = D_20180201_0903[!duplicated(D_20180201_0903[,c("ID")]),]


saveRDS(D_20180201_0903,"C:/Users/Eric/Desktop/SARA/rds/D_20180201_0903.rds")

#----合併E縣市資料----
E_20180201_0903 = rbind(E_20180201,E_20180202,E_20180502,E_20180802,E_20180803,E_20180901,E_20180902,E_20180903)

colnames(E_20180201_0903) = c("LOCATION","TRADETARGET","ADDRESS","LANDAREA","USETYPE","RURALTYPE","RURALTYPE_DT","TRADEDATE","TRADEBDNUM","TRANSFLOOR",
                              "TOLFLOOR","BDTYPE","MAINUSE","MAINMT","BDFNDATE","BDAREA","ROOM","HALL","TOILET","STRUCTURE",
                              "MANAGEMENT","TOLPRICE","PRICE","PARKTYPE","PARKAREA","TOLPARKPRICE","OTHER","ID","IMPORT","COUNTY_F")
E_20180201_0903 = arrange(E_20180201_0903,ID,desc(IMPORT))
E_20180201_0903 = E_20180201_0903[!duplicated(E_20180201_0903[,c("ID")]),]


saveRDS(E_20180201_0903,"C:/Users/Eric/Desktop/SARA/rds/E_20180201_0903.rds")
#----合併F縣市資料----
F_20180201_0903 = rbind(F_20180201,F_20180202,F_20180502,F_20180802,F_20180803,F_20180901,F_20180902,F_20180903)

colnames(F_20180201_0903) = c("LOCATION","TRADETARGET","ADDRESS","LANDAREA","USETYPE","RURALTYPE","RURALTYPE_DT","TRADEDATE","TRADEBDNUM","TRANSFLOOR",
                              "TOLFLOOR","BDTYPE","MAINUSE","MAINMT","BDFNDATE","BDAREA","ROOM","HALL","TOILET","STRUCTURE",
                              "MANAGEMENT","TOLPRICE","PRICE","PARKTYPE","PARKAREA","TOLPARKPRICE","OTHER","ID","IMPORT","COUNTY_F")
F_20180201_0903 = arrange(F_20180201_0903,ID,desc(IMPORT))
F_20180201_0903 = F_20180201_0903[!duplicated(F_20180201_0903[,c("ID")]),]


saveRDS(F_20180201_0903,"C:/Users/Eric/Desktop/SARA/rds/F_20180201_0903.rds")
#----合併G縣市資料----
G_20180201_0903 = rbind(G_20180201,G_20180202,G_20180502,G_20180802,G_20180803,G_20180901,G_20180902,G_20180903)

colnames(G_20180201_0903) = c("LOCATION","TRADETARGET","ADDRESS","LANDAREA","USETYPE","RURALTYPE","RURALTYPE_DT","TRADEDATE","TRADEBDNUM","TRANSFLOOR",
                              "TOLFLOOR","BDTYPE","MAINUSE","MAINMT","BDFNDATE","BDAREA","ROOM","HALL","TOILET","STRUCTURE",
                              "MANAGEMENT","TOLPRICE","PRICE","PARKTYPE","PARKAREA","TOLPARKPRICE","OTHER","ID","IMPORT","COUNTY_F")
G_20180201_0903 = arrange(G_20180201_0903,ID,desc(IMPORT))

G_20180201_0903 = G_20180201_0903[!duplicated(G_20180201_0903[,c("ID")]),]

saveRDS(G_20180201_0903,"C:/Users/Eric/Desktop/SARA/rds/G_20180201_0903.rds")
#----合併H縣市資料----
H_20180201_0903 = rbind(H_20180201,H_20180202,H_20180502,H_20180802,H_20180803,H_20180901,H_20180902,H_20180903)

colnames(H_20180201_0903) = c("LOCATION","TRADETARGET","ADDRESS","LANDAREA","USETYPE","RURALTYPE","RURALTYPE_DT","TRADEDATE","TRADEBDNUM","TRANSFLOOR",
                              "TOLFLOOR","BDTYPE","MAINUSE","MAINMT","BDFNDATE","BDAREA","ROOM","HALL","TOILET","STRUCTURE",
                              "MANAGEMENT","TOLPRICE","PRICE","PARKTYPE","PARKAREA","TOLPARKPRICE","OTHER","ID","IMPORT","COUNTY_F")
H_20180201_0903 = arrange(H_20180201_0903,ID,desc(IMPORT))

H_20180201_0903 = H_20180201_0903[!duplicated(H_20180201_0903[,c("ID")]),]

saveRDS(H_20180201_0903,"C:/Users/Eric/Desktop/SARA/rds/H_20180201_0903.rds")

#----合併I縣市資料----
I_20180201_0903 = rbind(I_20180201,I_20180202,I_20180502,I_20180802,I_20180803,I_20180901,I_20180902,I_20180903)

colnames(I_20180201_0903) = c("LOCATION","TRADETARGET","ADDRESS","LANDAREA","USETYPE","RURALTYPE","RURALTYPE_DT","TRADEDATE","TRADEBDNUM","TRANSFLOOR",
                              "TOLFLOOR","BDTYPE","MAINUSE","MAINMT","BDFNDATE","BDAREA","ROOM","HALL","TOILET","STRUCTURE",
                              "MANAGEMENT","TOLPRICE","PRICE","PARKTYPE","PARKAREA","TOLPARKPRICE","OTHER","ID","IMPORT","COUNTY_F")
I_20180201_0903 = arrange(I_20180201_0903,ID,desc(IMPORT))

I_20180201_0903 = I_20180201_0903[!duplicated(I_20180201_0903[,c("ID")]),]

saveRDS(I_20180201_0903,"C:/Users/Eric/Desktop/SARA/rds/I_20180201_0903.rds")
#----合併J縣市資料----
J_20180201_0903 = rbind(J_20180201,J_20180202,J_20180502,J_20180802,J_20180803,J_20180901,J_20180902,J_20180903)

colnames(J_20180201_0903) = c("LOCATION","TRADETARGET","ADDRESS","LANDAREA","USETYPE","RURALTYPE","RURALTYPE_DT","TRADEDATE","TRADEBDNUM","TRANSFLOOR",
                              "TOLFLOOR","BDTYPE","MAINUSE","MAINMT","BDFNDATE","BDAREA","ROOM","HALL","TOILET","STRUCTURE",
                              "MANAGEMENT","TOLPRICE","PRICE","PARKTYPE","PARKAREA","TOLPARKPRICE","OTHER","ID","IMPORT","COUNTY_F")
J_20180201_0903 = arrange(J_20180201_0903,ID,desc(IMPORT))

J_20180201_0903 = J_20180201_0903[!duplicated(J_20180201_0903[,c("ID")]),]

saveRDS(J_20180201_0903,"C:/Users/Eric/Desktop/SARA/rds/J_20180201_0903.rds")
#----合併N縣市資料----
N_20180201_0903 = rbind(N_20180201,N_20180202,N_20180502,N_20180802,N_20180803,N_20180901,N_20180902,N_20180903)

colnames(N_20180201_0903) = c("LOCATION","TRADETARGET","ADDRESS","LANDAREA","USETYPE","RURALTYPE","RURALTYPE_DT","TRADEDATE","TRADEBDNUM","TRANSFLOOR",
                              "TOLFLOOR","BDTYPE","MAINUSE","MAINMT","BDFNDATE","BDAREA","ROOM","HALL","TOILET","STRUCTURE",
                              "MANAGEMENT","TOLPRICE","PRICE","PARKTYPE","PARKAREA","TOLPARKPRICE","OTHER","ID","IMPORT","COUNTY_F")
N_20180201_0903 = arrange(N_20180201_0903,ID,desc(IMPORT))

N_20180201_0903 = N_20180201_0903[!duplicated(N_20180201_0903[,c("ID")]),]

saveRDS(N_20180201_0903,"C:/Users/Eric/Desktop/SARA/rds/N_20180201_0903.rds")
#----合併O縣市資料----
O_20180201_0903 = rbind(O_20180201,O_20180202,O_20180502,O_20180802,O_20180803,O_20180901,O_20180902,O_20180903)

colnames(O_20180201_0903) = c("LOCATION","TRADETARGET","ADDRESS","LANDAREA","USETYPE","RURALTYPE","RURALTYPE_DT","TRADEDATE","TRADEBDNUM","TRANSFLOOR",
                              "TOLFLOOR","BDTYPE","MAINUSE","MAINMT","BDFNDATE","BDAREA","ROOM","HALL","TOILET","STRUCTURE",
                              "MANAGEMENT","TOLPRICE","PRICE","PARKTYPE","PARKAREA","TOLPARKPRICE","OTHER","ID","IMPORT","COUNTY_F")
O_20180201_0903 = arrange(O_20180201_0903,ID,desc(IMPORT))

O_20180201_0903 = O_20180201_0903[!duplicated(O_20180201_0903[,c("ID")]),]

saveRDS(O_20180201_0903,"C:/Users/Eric/Desktop/SARA/rds/O_20180201_0903.rds")
#----合併Q縣市資料----
Q_20180201_0903 = rbind(Q_20180201,Q_20180202,Q_20180502,Q_20180802,Q_20180803,Q_20180901,Q_20180902,Q_20180903)

colnames(Q_20180201_0903) = c("LOCATION","TRADETARGET","ADDRESS","LANDAREA","USETYPE","RURALTYPE","RURALTYPE_DT","TRADEDATE","TRADEBDNUM","TRANSFLOOR",
                              "TOLFLOOR","BDTYPE","MAINUSE","MAINMT","BDFNDATE","BDAREA","ROOM","HALL","TOILET","STRUCTURE",
                              "MANAGEMENT","TOLPRICE","PRICE","PARKTYPE","PARKAREA","TOLPARKPRICE","OTHER","ID","IMPORT","COUNTY_F")
Q_20180201_0903 = arrange(Q_20180201_0903,ID,desc(IMPORT))

Q_20180201_0903 = Q_20180201_0903[!duplicated(Q_20180201_0903[,c("ID")]),]

saveRDS(Q_20180201_0903,"C:/Users/Eric/Desktop/SARA/rds/Q_20180201_0903.rds")

#----合併alldata----
alldata_20180201_0903 = rbind(A_20180201_0903,B_20180201_0903,C_20180201_0903,D_20180201_0903
                              ,E_20180201_0903,F_20180201_0903,G_20180201_0903,H_20180201_0903
                              ,I_20180201_0903,J_20180201_0903,N_20180201_0903,O_20180201_0903
                              ,Q_20180201_0903)

saveRDS(alldata_20180201_0903,"C:/Users/Eric/Desktop/SARA/rds/ALLDATA_20180201_0903.rds")

