#RUN BKMR
library(bkmr)
set.seed(111)
fitkm <- kmbayes(y = outcome.v, Z = exposure, X=covariate,  iter = 10000, verbose = FALSE, varsel =TRUE, groups = c(1,2,3,4,5,6,7,8,8,8))

#########################
save.image("/Users/Yi/Box Sync/BKNR/WHIOS/WHIOS_test/model/model_10.RData")
#########################
