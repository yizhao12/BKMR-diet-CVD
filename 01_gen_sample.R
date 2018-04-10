library(haven)
library(reshape2)
library(ggplot2)
library(corrplot)
library(plyr)

#Read in data
setwd("/Users/Yi/Box Sync/BKNR/WHIOS/WHI_OS_2014b/WHI_OS_2014a/data/whi/sas_data")
nutrients<-read_sas("f60_nutr_os_pub.sas7bdat")
ffq<-read_sas("f60_item_os_pub.sas7bdat")
outcome<-read_sas("f80_os_pub.sas7bdat") #CVD outcome: outc_adj_os_pub; physical measuremnet outcome: f80_os_pub
demo<-read_sas("dem_os_pub.sas7bdat")

##################STEP ONE: Clean each dataset################
#FFQ Cleanning
# Only keep baseline (F60VTYP==1) or year3 (F60VTYP==3), closest to designed visit time (F60VY==3), status=600 kcal <= Energy <= 5000 kcal
#baseline
ffq_y0<-ffq[ffq$F60VTYP==1 & ffq$STATUS==3, ]
#year3
ffq_y3<-ffq[ffq$F60VTYP==3 & ffq$F60VY==3 & ffq$F60VCLO==1 & ffq$STATUS==3,] 

#define and keep selected food groups
foodgroup<-c("ID","FRUITS", "VEGTABLS", "FRUVEG", "FISH", "REDMEAT","POULTRY", "SOY", "NUTS","GRAINS","WHLGRNS", "MILKS","DAIRY","WINE","BEER", "LIQUOR", "POP","COFFEE") 
ffq_y0<-ffq_y0[foodgroup]
ffq_y3<-ffq_y3[foodgroup]
  
#rename variables for each year 
names(ffq_y0)<-paste0("y0_",foodgroup)
names(ffq_y3)<-paste0("y3_",foodgroup)
ffq_y0<-rename(ffq_y0,c("y0_ID"="ID"))
ffq_y3<-rename(ffq_y3,c("y3_ID"="ID"))

#merge 2 years together
ffq<-merge(ffq_y0,ffq_y3,by="ID")

# #calculate a difference baseline and year3 ffq
# for (i in 2:18) { # 1 is ID
#   diff<-data.frame(lapply(i,function(x){ffq[,x+17]-ffq[,x]}))
#   names(diff)<-paste0("diff_",foodgroup[i])
#   print(names(c(ffq[i],ffq[i+17],diff))) #check if matching correctly
#   ffq<-cbind(ffq,diff)
# }

# calculate an average between baseline and year3 ffq
for (i in 2:18) { # 1 is ID
  avg<-data.frame(lapply(i,function(x){(ffq[,x+17]+ffq[,x])/2}))
  names(avg)<-paste0("avg_",foodgroup[i])
  print(names(c(ffq[i],ffq[i+17],avg))) #check if matching correctly
  ffq<-cbind(ffq,avg)
}

#################################################
#Clean nutrients dataset
#keep sleected observations for baseline and year3
nutr_y0<-nutrients[nutrients$F60VTYP==1,]
nutr_y3<-nutrients[nutrients$F60VTYP==3 & nutrients$F60VY==3 & nutrients$F60VCLO==1,]
#rename nutrients variables
nutrient_name<-names(nutrients)
names(nutr_y0)<-paste0("y0_",nutrient_name)
names(nutr_y3)<-paste0("y3_",nutrient_name)
nutr_y0<-rename(nutr_y0,c("y0_ID"="ID"))
nutr_y3<-rename(nutr_y3,c("y3_ID"="ID"))
nutrients<-merge(nutr_y0,nutr_y3,by="ID")

# #calculate a difference baseline and year3 nutrients (8-146 vs 153-291)
# for (i in 8:146) {
#   diff<-data.frame(lapply(i,function(x){nutrients[,x+145]-nutrients[,x]}))
#   names(diff)<-paste0("diff_",nutrient_name[i])
#   print(names(c(nutrients[i],nutrients[i+145],diff))) #check if matching correctly
#   nutrients<-cbind(nutrients,diff)
# }

#calculate an average baseline and year3 nutrients (8-146 vs 153-291)
for (i in 8:146) {
  avg<-data.frame(lapply(i,function(x){(nutrients[,x+145]+nutrients[,x])/2}))
  names(avg)<-paste0("avg_",nutrient_name[i])
  print(names(c(nutrients[i],nutrients[i+145],avg))) #check if matching correctly
  nutrients<-cbind(nutrients,avg)
}

#################################################
#Clean outcome dataset
  # keep baseline and year 3 measurement
out_y0<-outcome[outcome$F80VTYP==1,]
out_y3<-outcome[outcome$F80VTYP==3 & outcome$F80VY==3 & outcome$F80VCLO==1,]
#rename outcome variables
outcome_names<-names(outcome)
names(out_y0)<-paste0("y0_",outcome_names)
names(out_y3)<-paste0("y3_",outcome_names)
out_y0<-rename(out_y0,c("y0_ID"="ID"))
out_y3<-rename(out_y3,c("y3_ID"="ID"))
outcome<-merge(out_y0,out_y3,by="ID")


# #calculate a DIFF baseline and year3 outcome (7-23 vs 29-45)
# for (i in 7:23) {
#   diff<-data.frame(lapply(i,function(x){outcome[,x+22]-outcome[,x]}))
#   names(diff)<-paste0("diff_",outcome_names[i])
#   print(names(c(outcome[i],outcome[i+22],diff))) #check if matching correctly
#   outcome<-cbind(outcome,diff)
# }

#calculate AVERAGE between baseline and year3 (7-23 vs 29-45)
for (i in 7:23) {
  avg<-data.frame(lapply(i,function(x){(outcome[,x+22]+outcome[,x])/2}))
  names(avg)<-paste0("avg_",outcome_names[i])
  print(names(c(outcome[i],outcome[i+22],avg))) #check if matching correctly
  outcome<-cbind(outcome,avg)
}

#transform BMIX
outcome$avg_BMIX_ln<-log(outcome$avg_BMIX)

#################################################
#Clean covariate dataset
  #income=9 -> unknown, set to missing
demo$INCOME[demo$INCOME==9]<-NA

#Merge datasets
#outcome + demographical data
combine<-merge(demo,outcome, by="ID")
# + nutrients dataset
combine<-merge(nutrients,combine, by="ID")
# + ffq dataset
combine<-merge(ffq,combine, by="ID")

#delete missing value
combine_nomiss<-na.omit(combine)

# #########STEP TWO: descriptive analysis of the dataset #########
# #distribution of the EXPOSURE
# #baseline
# ggplot(data = melt(combine_nomiss[names(ffq_y0[-1])]), mapping = aes(x = value)) + geom_histogram(bins = 15) + facet_wrap(~variable, scales = 'free_x') + 
#   labs(title="distribution of exposure (baseline)", x="servings per day")
# summary(combine_nomiss[names(ffq_y0[-1])])
# #Year 3
# ggplot(data = melt(combine_nomiss[names(ffq_y3[-1])]), mapping = aes(x = value)) + geom_histogram(bins = 15) + facet_wrap(~variable, scales = 'free_x') + 
#   labs(title="distribution of exposure (Year 3)", x="servings per day")
# summary(combine_nomiss[names(ffq_y3[-1])])
# 
# # #Difference
# # ggplot(data = melt(combine_nomiss[grep("diff", names(ffq), value=TRUE)]), mapping = aes(x = value)) + geom_histogram(bins = 15) + facet_wrap(~variable, scales = 'free_x') + 
# #   labs(title="distribution of exposure (Difference)", x="servings per day")
# # summary(combine_nomiss[grep("diff", names(ffq), value=TRUE)])
# 
# #Average
# ggplot(data = melt(combine_nomiss[grep("avg", names(ffq), value=TRUE)]), mapping = aes(x = value)) + geom_histogram(bins = 15) + facet_wrap(~variable, scales = 'free_x') + 
#   labs(title="distribution of exposure (Average)", x="servings per day")
# summary(combine_nomiss[grep("avg", names(ffq), value=TRUE)])
# 
# #check correlation of food groups between baseline and y3
# corr<-data.frame(foodgroup[-1])
# for (i in 2:18) {
#   corr$corr[i-1]<-cor(ffq[i],ffq[i+17])
#   print(c(i,names(ffq[i]),names(ffq[i+17])))
# }
# print("correlation of exposure between baseline and exposure")
# print(corr)
# 
# # #distribution of ln_exposure
# # log_exp<-data.frame(lapply(foodgroup[-1],function(x){log(combine_nomiss[,x])}))
# # names(log_exp)<-paste0("log_",foodgroup[-1])
# # combine_nomiss<-cbind(combine_nomiss,log_exp) 
# # 
# # #add value 1 to each exposure
# # plus_exp<-data.frame(lapply(foodgroup[-1],function(x){combine_nomiss[,x]+1}))
# # names(plus_exp)<-paste0("plus_",foodgroup[-1])
# # combine_nomiss<-cbind(combine_nomiss,plus_exp)
# # 
# # 
# # #ln-transform all EXPOSURE variables
# # log_plus_exp<-data.frame(lapply(names(plus_exp),function(x){log(combine_nomiss[,x])}))
# # names(log_plus_exp)<-paste0("log_",names(plus_exp))
# # combine_nomiss<-cbind(combine_nomiss,log_plus_exp)
# # 
# # #distribution of the log-transformed exposures
# # ggplot(data = melt(log_plus_exp), mapping = aes(x = value))  + geom_histogram(bins = 15) + facet_wrap(~variable, scales = 'free_x')  + 
# #   labs(title="Distribution of log_(exposure+1)", x="log_(servings+1 per day)") 
# 
# #Correlation of the EXPOSURE
# corrplot(cor(combine_nomiss[names(ffq_y0[-1])]), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, main="corr between exposures at baseline")
# corrplot(cor(combine_nomiss[names(ffq_y3[-1])]), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, main="corr between exposures at Year3")
# 
# #quantitatively present correlation and rank them
# # coll<-cor(ffq)
# # coll[lower.tri(coll,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
# # coll=as.data.frame(as.table(coll))  #Turn into a 3-column table
# # coll=na.omit(coll)  #Get rid of the junk we flagged above
# # coll=coll[order(-abs(coll$Freq)),]
# # coll
# 
# #distribution of the OUTCOMES 
# #baseline
# ggplot(data = melt(combine_nomiss[c("y0_SYST")]), mapping = aes(x = value)) + 
#   geom_histogram(bins = 20) + facet_wrap(~variable, scales = 'free_x') + 
#   labs(title="Distribution of Outcome at Baseline",x="Systolic BP")
# #Year 3
# ggplot(data = melt(combine_nomiss[c("y3_SYST")]), mapping = aes(x = value)) + 
#   geom_histogram(bins = 20) + facet_wrap(~variable, scales = 'free_x') + 
#   labs(title="Distribution of Outcome at Year 3",x="Systolic BP")
# # #Diff
# # ggplot(data = melt(combine_nomiss[c("diff_SYST")]), mapping = aes(x = value)) + 
# #   geom_histogram(bins = 20) + facet_wrap(~variable, scales = 'free_x') + 
# #   labs(title="Distribution of Difference in Outcome ",x="Systolic BP")
# 
# #distribution of the COVARIATES
# ggplot(data = melt(combine_nomiss[c("AGE","avg_BMIX_ln","INCOME", "EDUC")]), mapping = aes(x = value)) + 
#   geom_histogram(bins = 20) + facet_wrap(~variable, scales = 'free_x') + 
#   ggtitle("Distribution of covariates")
# 
# # #linearity between COVARIATES and OUTCOME (scatterplot+linear regression+lowess)---maybe do it in a loop
# # combine_nomiss$f.INCOME<-as.factor(combine_nomiss$INCOME)
# # combine_nomiss$f.EDUC<-as.factor(combine_nomiss$EDUC)
# # 
# # #age
# # plot(combine_nomiss$SYST~combine_nomiss$AGE,main="SBP~Age", col="grey")
# # abline(lm(SYST~AGE,data=combine_nomiss),col="pink")
# # lines(lowess(combine_nomiss$AGE,combine_nomiss$SYST),col="green")
# # legend(67,210,legend=c("regression","lowess"),col=c("pink","green"),lty = c(1,1))
# # #BMI
# # plot(combine_nomiss$SYST~combine_nomiss$BMIX,main="SBP~BMI", col="grey")
# # abline(lm(SYST~BMIX,data=combine_nomiss),col="pink")
# # lines(lowess(combine_nomiss$BMIX,combine_nomiss$SYST),col="green")
# # legend(40,210,legend=c("regression","lowess"),col=c("pink","green"),lty = c(1,1))
# # #income, educ(categorical)
# # plot(combine_nomiss$SYST~combine_nomiss$f.INCOME,main="SBP~INCOME", col="pink")
# # plot(combine_nomiss$SYST~combine_nomiss$f.EDUC,main="SBP~Educ", col="pink")
# # #income,educ (treating categories as continuous)
# # plot(combine_nomiss$SYST~combine_nomiss$INCOME,main="SBP~INCOME(cat)", col="grey")
# # abline(lm(SYST~INCOME,data=combine_nomiss),col="pink")
# # lines(lowess(combine_nomiss$INCOME,combine_nomiss$SYST),col="green")
# # legend(5.5,210,legend=c("regression","lowess"),col=c("pink","green"),lty = c(1,1))
# # 
# # plot(combine_nomiss$SYST~combine_nomiss$EDUC,main="SBP~Educ(cat)", col="grey")
# # abline(lm(SYST~EDUC,data=combine_nomiss),col="pink")
# # lines(lowess(combine_nomiss$EDUC,combine_nomiss$SYST),col="green")
# # legend(5.5,210,legend=c("regression","lowess"),col=c("pink","green"),lty = c(1,1))
# 
# 
# #Run a multivarable regression on full dataset
# #summary(lm(SYST~FRUITS+VEGTABLS+FISH+REDMEAT+POULTRY+SOY+NUTS+GRAINS+WHLGRNS+MILKS+DAIRY+WINE+BEER+LIQUOR+POP+COFFEE
#       #     +AGE+BMIX+INCOME+EDUC,data=combine_nomiss))
# 
# 
# 
# #########STEP THREE: prepare for sample dataset#########
# #take a random sample from the dataset
# # set.seed(123)
# # sample<-combine_nomiss[sample(1:nrow(combine_nomiss),600, replace=FALSE), ]
# 
# #sample regression
# # summary(lm(SYST~FRUITS+VEGTABLS+FISH+REDMEAT+POULTRY+SOY+NUTS+GRAINS+WHLGRNS+MILKS+DAIRY+WINE+BEER+LIQUOR+POP+COFFEE
# #            +AGE+BMIX+INCOME+EDUC,data=sample)) #exclude the fruit and veggie combined group as high collinarity
# # 
# #Export
# write.csv(sample,"/Users/Yi/Box Sync/BKNR/WHIOS/WHIOS_test/sample_600.csv")
## Export
write.csv(combine_nomiss,"/Users/Yi/Box Sync/BKNR/WHIOS/WHIOS_test/dataset/v1_clean.csv")
