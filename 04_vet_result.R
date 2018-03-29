library(bkmr)
library(ggplot2)

setwd("/Users/Yi/Box Sync/BKNR/WHIOS/WHIOS_test/model")
load("model_9.RData")

##############Extract summary statistics##############
ExtractEsts(fitkm)
#model convergence
TracePlot(fit = fitkm, par = "beta")
TracePlot(fit = fitkm, par = "sigsq.eps")
TracePlot(fit = fitkm, par = "r", comp =3)
#posterior inclusion probabilities
ExtractPIPs(fitkm)


##############Visualize output##############
#Overall effect 
X<-as.matrix(covariate)
Z<-as.matrix(exposure)
risks.overall <- OverallRiskSummaries(fit = fitkm, y = outcome.v, Z = Z, X = X,
                                      qs = seq(0.25, 0.75, by = 0.05),
                                      q.fixed = 0.5, method = "exact")
risks.overall
ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) + geom_pointrange()


#univariate (fix other exposures)
pred.resp.univar <- PredictorResponseUnivar(fit = fitkm) #defalut fix at 0.5
ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se, ymax = est + 1.96*se)) + geom_smooth(stat = "identity") + facet_wrap(~ variable) + ylab("h(z)")


# single-predictor health risks
#fixed other predictors at certain levels (different colors), the effect of the single predictor (from 25th-75th) on outcome
#potential interaction
risks.singvar <- SingVarRiskSummaries(fit = fitkm,
                                      qs.diff = c(0.25, 0.75),
                                      q.fixed = c(0.25, 0.50, 0.75),
                                      method = "exact")
ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd,
                          ymax = est + 1.96*sd, col = q.fixed)) +
  geom_pointrange(position = position_dodge(width = 0.75)) +
  coord_flip()


#bivariates 
#contour
pairs<-data.frame(c(1,2,3,1,2,3,1,2,3),c(3,4,5,4,5,3,5,4,3)) #pair selection to display
pred.resp.bivar <- PredictorResponseBivar(fit = fitkm, z.pairs = pairs, min.plot.dist = 1)
ggplot(pred.resp.bivar, aes(z1, z2, fill = est)) +
  geom_raster() +
  facet_grid(variable2 ~ variable1) +
  scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF")) +
  xlab("expos1") +
  ylab("expos2") +
  ggtitle("h(expos1, expos2)")

#quantiles
pred.resp.bivar.levels <- PredictorResponseBivarLevels( pred.resp.df = pred.resp.bivar, Z = exposure, qs = c(0.1, 0.5, 0.9), z.names = c(8,9,7))
ggplot(pred.resp.bivar.levels, aes(z1, est)) +
  geom_smooth(aes(col = quantile), stat = "identity") +
  facet_grid(variable2 ~ variable1) +
  ggtitle("h(expos1 | quantiles of expos2)") +
  xlab("expos1")
