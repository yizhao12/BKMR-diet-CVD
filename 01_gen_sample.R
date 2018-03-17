#Read in data
library(haven)
setwd("/Users/Yi/Box Sync/BKNR/WHIOS/WHI_OS_2014b/WHI_OS_2014a/data/whi/sas_data")
nutrients<-read_sas("f60_nutr_os_pub.sas7bdat")
ffq<-read_sas("f60_item_os_pub.sas7bdat")
outcome<-read_sas("f80_os_pub.sas7bdat") #CVD outcome: outc_adj_os_pub; physical measuremnet outcome: f80_os_pub
demo<-read_sas("dem_os_pub.sas7bdat")


#Clean FFQ, only keep food group data
ffq<-ffq[ffq$F60VTYP==3 & ffq$F60VY==3 & ffq$F60VCLO==1 & ffq$STATUS==3,] #anual visit, visit year=3 year, closest to designed visit time, status=600 kcal <= Energy <= 5000 kcal
foodgroup<-c("ID","FRUITS", "VEGTABLS", "FRUVEG", "FISH", "REDMEAT","POULTRY", "SOY", "NUTS","GRAINS","WHLGRNS", "MILKS","DAIRY","WINE","BEER", "LIQUOR") 
ffq<-ffq[foodgroup]
#Clean nutrients
nutrients<-nutrients[nutrients$F60VTYP==3 & nutrients$F60VY==3 & nutrients$F60VCLO==1,]
#Clean outcome dataset
outcome<-outcome[outcome$F80VTYP==3 & outcome$F80VY==3 & outcome$F80VCLO==1,]

#Merge datasets
#outcome + demographical data
combine1<-merge(demo,outcome, by="ID")
#merge with nutrients dataset
combine2<-merge(nutrients,combine1, by="ID")
#merge with ffq dataset
combine3<-merge(ffq,combine2, by="ID")

#delete missing value
combine_nomiss<-na.omit(combine3)

#take a random sample from the dataset
set.seed(123)
sample<-combine_nomiss[sample(1:nrow(combine_nomiss),200, replace=FALSE), ]

#Export
write.csv(sample,"/Users/Yi/Box Sync/BKNR/WHIOS/WHIOS_test/sample.csv")

