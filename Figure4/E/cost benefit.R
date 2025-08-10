library(rmda)
library(export)

rt=read.table("input.txt",header=T,sep="\t",row.names = 1)
rt<- as.data.frame(rt)
model1<- decision_curve(group~ CA199,
                        data = rt,family = binomial(link ='logit'),
                        thresholds= seq(0,1, by = 0.01),
                        confidence.intervals = 0.95,
                        study.design = 'case-control',
                        population.prevalence = 0.3)

model2<-decision_curve(group~circRNA,
                        data = rt,family = binomial(link ='logit'),
                        thresholds = seq(0,1, by = 0.01),
                        confidence.intervals= 0.95,
                        study.design = 'case-control',
                        population.prevalence= 0.3)

model3<-decision_curve(group~riskScore,
                       data = rt,family = binomial(link ='logit'),
                       thresholds = seq(0,1, by = 0.01),
                       confidence.intervals= 0.95,
                       study.design = 'case-control',
                       population.prevalence= 0.3)


###########Clinical Impact Curve#############

tiff(file="clinical impact curve.tiff",height = 2500,width = 2500,res=300)
plot_clinical_impact(model1,population.size= 1000,
                     cost.benefit.axis = T,
                     n.cost.benefits= 8,
                     col =c('red','blue'),
                     confidence.intervals= T,
                     ylim=c(0,1000))

plot_clinical_impact(model2,population.size= 1000,
                     cost.benefit.axis = T,
                     n.cost.benefits= 8,
                     col =c('red','blue'),
                     confidence.intervals= T,
                     ylim=c(0,1000))

plot_clinical_impact(model3,population.size= 1000,
                     cost.benefit.axis = T,
                     n.cost.benefits= 8,
                     col =c('red','blue'),
                     confidence.intervals= T,
                     ylim=c(0,1000))
dev.off()
graph2ppt(file="clinical impact curve.pptx", width=10, height=8)




