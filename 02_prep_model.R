library(ggplot2)
library(corrplot)
library(reshape2)


# read in dataset ---------------------------------------------------------
setwd("/Users/Yi/Box Sync/BKNR/WHIOS/WHIOS_test/dataset")
df<-read.csv("v1_clean.csv")

####choose which dietary pattern as exposure####
diet<-(c("MEDI")) #DASH
run<-data.frame(diet)

# Select exposure ---------------------------------------------------------
if (run$diet=="MEDI"){
df$alcohol<-df$BEER+df$WINE+df$LIQUOR
#df$fat_ratio<-df$F60MFA/df$F60SFA
df$F60SFA_0.1<-df$F60SFA/10
df$F60MFA_0.1<-df$F60MFA/10
exp_var<-c("FRUITS", "VEGTABLS", "SOY", "NUTS", "WHLGRNS", "FISH",  "alcohol", "REDMEAT", "F60SFA_0.1", "F60MFA_0.1")
}

if (run$diet=="DASH") {
#Set Exposure as DASH diet
#1) fruits 2) vegetables 3) nuts and legumes 4) low-fat dairy 5) whole grains 6) sodium 7) SSB 8) red and processed meats
df$nut_soy<-df$NUTS+df$SOY
df$sodium_0.001<-df$F60SODUM/1000
exp_var<-c("FRUITS", "VEGTABLS","nut_soy", "WHLGRNS","DAIRY", "sodium_0.001", "POP", "REDMEAT" )
}

exposure<-df[exp_var]

# Select outcome ----------------------------------------------------------
outcome<-df["SYST"]
outcome.v<-as.vector(outcome[,1])

# Select covariates -------------------------------------------------------
covariate<-df[c("AGE","BMIX_ln", "F60ENRGY","INCOME", "EDUC")] 


# descriptive if random df------------------------------------------------------------
#1. Correlation between exposures
#corrplot(cor(exposure), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
# coll<-cor(exposure)
# coll[lower.tri(coll,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
# coll=as.data.frame(as.table(coll))  #Turn into a 3-column table
# coll=na.omit(coll)  #Get rid of the junk we flagged above
# coll=coll[order(-abs(coll$Freq)),]
# coll


# #2. distribution of variables
# #exposures
# ggplot(data = melt(exposure), mapping = aes(x = value)) + geom_histogram(bins = 15) + facet_wrap(~variable, scales = 'free_x') + 
#   labs(title="distribution of exposure", x="servings per day; for SFA, MFA: 10*g/d")
# summary(exposure)
# #outcome
# ggplot(data = melt(df[c("SYST")]), mapping = aes(x = value)) + 
#   geom_histogram(bins = 20) + facet_wrap(~variable, scales = 'free_x') + 
#   labs(title="Distribution of Outcome",x="Systolic BP")
# #Covariates
# ggplot(data = melt(df[c("AGE","BMIX","F60ENRGY","INCOME","EDUC")]), mapping = aes(x = value)) + 
#   geom_histogram(bins = 20) + facet_wrap(~variable, scales = 'free_x') + 
#   ggtitle("Distribution of covariates")
write.csv(df,"/Users/Yi/Box Sync/BKNR/WHIOS/WHIOS_test/dataset/v1_prep.csv")

