library(dplyr)
library(readxl)
sas = read.xlsx2("./M50/output_excel/M50_20171000_20180902.xlsx",1)
sas = sas%>% arrange_all()
r_8 = usedata_8 %>% arrange_all()

test = which(r_8!=sas,arr.ind=T)