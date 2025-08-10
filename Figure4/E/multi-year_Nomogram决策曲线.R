
####################????????????
library(devtools)
#install_github("mdbrown/DecisionCurve")
library(rmda) 
library(export)
# 5 years
rt=read.table("riskOutput.txt",sep="\t",header=T,row.names=1,check.names=F) 


model2<- decision_curve(group ~  riskScore ,
                        data = rt,
                        thresholds = seq(0, 1, by = .005),
                        bootstraps = 10)



plot_decision_curve(list(model2),confidence.intervals=FALSE,
                    curve.names = c("CA199"),
                    col = c("red"),
                    cost.benefit.axis=FALSE,
                    lwd = 2,
                    lty = 2,
                    standardize=FALSE,
                    legend.position = "none")
graph2ppt(file="CA199 DCA.pptx", width=10, height=6)


CA<-summary(model2,measure= "NB")

write.table(CA199,"CA199.txt", sep= "\t")

######3¸ö
model1 <- decision_curve(group ~ CA199,
                         data = rt,
                         thresholds = seq(0, 1, by = .005),
                         bootstraps = 10)


model2<- decision_curve(group ~  circRNA ,
                       data = rt,
                       thresholds = seq(0, 1, by = .005),
                        bootstraps = 10)


model3<- decision_curve(group ~  riskScore ,
                        data = rt,
                        thresholds = seq(0, 1, by = .005),
                        bootstraps = 10)

plot_decision_curve(list(model1, model2, model3),confidence.intervals=FALSE,
                   curve.names = c("CA199", "circRNA", "combination"),
                   col = c("cornflowerblue","green","red"),
                   cost.benefit.axis=FALSE,
                   lwd = 2:2:2,
                   lty = 2:2:2,
                    standardize=FALSE,
                    legend.position = "none")
graph2ppt(file="effect plot.pptx", width=10, height=6)





