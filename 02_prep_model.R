library(ggplot2)
library(corrplot)
library(reshape2)


#read in data
setwd("/Users/Yi/Box Sync/BKNR/WHIOS/WHIOS_test")
sample<-read.csv("sample_600.csv")

# set.seed(123)
# sample<-sample[sample(1:nrow(sample),400, replace=FALSE), ]


#Exposure
###select exposures####
#set exposure as Medi diet
sample$alcohol<-sample$BEER+sample$WINE+sample$LIQUOR
#sample$fat_ratio<-sample$F60MFA/sample$F60SFA
sample$F60SFA_0.1<-sample$F60SFA/10
sample$F60MFA_0.1<-sample$F60MFA/10
exp_var<-c("FRUITS", "VEGTABLS", "SOY", "NUTS", "WHLGRNS", "FISH",  "alcohol", "REDMEAT", "F60SFA_0.1", "F60MFA_0.1")

# #Set Exposure as DASH diet
# #1) fruits 2) vegetables 3) nuts and legumes 4) low-fat dairy 5) whole grains 6) sodium 7) SSB 8) red and processed meats
# sample$nut_soy<-sample$NUTS+sample$SOY
# sample$sodium_0.001<-sample$F60SODUM/1000
# exp_var<-c("FRUITS", "VEGTABLS","nut_soy", "WHLGRNS","DAIRY", "sodium_0.001", "POP", "REDMEAT" ) 

exposure<-sample[exp_var]

#Outcome
####select outcome####
outcome<-sample["SYST"]
########################
outcome.v<-as.vector(outcome[,1])

#Covariate
####select covariates####
covariate<-sample[c("AGE","BMIX", "F60ENRGY","INCOME", "EDUC")] 
#########################


###########Visualization################
#1. Correlation between exposures
corrplot(cor(exposure), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
# coll<-cor(exposure)
# coll[lower.tri(coll,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
# coll=as.data.frame(as.table(coll))  #Turn into a 3-column table
# coll=na.omit(coll)  #Get rid of the junk we flagged above
# coll=coll[order(-abs(coll$Freq)),]
# coll


#2. distribution of variables
#exposures
ggplot(data = melt(exposure), mapping = aes(x = value)) + geom_histogram(bins = 15) + facet_wrap(~variable, scales = 'free_x') + 
  labs(title="distribution of exposure", x="servings per day; for SFA, MFA: 10*g/d")
summary(exposure)
#outcome
ggplot(data = melt(sample[c("SYST")]), mapping = aes(x = value)) + 
  geom_histogram(bins = 20) + facet_wrap(~variable, scales = 'free_x') + 
  labs(title="Distribution of Outcome",x="Systolic BP")
#Covariates
ggplot(data = melt(sample[c("AGE","BMIX","F60ENRGY","INCOME","EDUC")]), mapping = aes(x = value)) + 
  geom_histogram(bins = 20) + facet_wrap(~variable, scales = 'free_x') + 
  ggtitle("Distribution of covariates")

#3. Multiple Linear regression-use consonlas font in Word
summary(lm(SYST~FRUITS +  VEGTABLS +  SOY +  NUTS +  WHLGRNS +  FISH +  REDMEAT +  alcohol + F60SFA_0.1+F60MFA_0.1
         + AGE + BMIX+F60ENRGY+INCOME+EDUC, data=sample))

# summary(lm(SYST~FRUITS +  VEGTABLS + nut_soy +  WHLGRNS + DAIRY +  sodium_0.001 +  POP +  REDMEAT+
#              + AGE + BMIX+F60ENRGY+INCOME+EDUC,data=sample))
