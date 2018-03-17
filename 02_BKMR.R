#read in data
setwd("/Users/Yi/Box Sync/BKNR/WHIOS/WHIOS_test")
sample<-read.csv("sample.csv")

#Exposure
####select exposures####
  #exp_var<-c("FRUITS","VEGTABLS","FISH","REDMEAT","POULTRY","SOY","NUTS","GRAINS","WHLGRNS","DAIRY", "WINE") 
  exp_var<-c("FISH","POULTRY","SOY","NUTS", "WHLGRNS") 
########################
exposure<-sample[exp_var]

#Outcome
####select outcome####
outcome<-sample["SYST"]
########################
outcome.v<-as.vector(outcome[,1])

#Covariate
####select covariates####
covariate<-sample[c("AGE","INCOME","BMIX")] #can only include numeric covariates?
#########################


#Checkpoint 
#correlation among the exposures-need to improve
coll<-cor(exposure)
coll[lower.tri(coll,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
coll=as.data.frame(as.table(coll))  #Turn into a 3-column table
coll=na.omit(coll)  #Get rid of the junk we flagged above
coll=coll[order(-abs(coll$Freq)),] 
coll
#Correlogram
library(corrplot)
corrplot(cor(exposure), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#Linear regression model
summary(lm(SYST~FISH + POULTRY + SOY + NUTS +  WHLGRNS +AGE+INCOME+BMIX ,data=sample))

#RUN BKMR
library(bkmr)
set.seed(111)
fitkm <- kmbayes(y = outcome.v, Z = exposure, X=covariate,  iter = 10000, verbose = FALSE, varsel =TRUE) #,groups = c(1,2,3,4,5,6,7,8,8,9,10)

#########################
save.image("/Users/Yi/Box Sync/BKNR/WHIOS/WHIOS_test/model/model_6.RData")
#########################

