#install.packages("OptimalCutpoints")

library(OptimalCutpoints)

rt = read.table(file = "riskScore_Input_new.txt",sep = "\t",row.names = 1,header = T)
optimal.cutpoints.Youden <- optimal.cutpoints(X = "riskScore", status = "group", tag.healthy = 0,methods = "Youden", data = rt, pop.prev = NULL,control = control.cutpoints(), ci.fit = FALSE, conf.level = 0.95, trace = FALSE)
cutpointsYouden = summary(optimal.cutpoints.Youden)
#plot(optimal.cutpoints.Youden)
cutpoint=cutpointsYouden$Youden$Global$optimal.cutoff$cutoff
riskScore=rt$riskScore
risk=as.vector(ifelse(riskScore>cutpoint,"high","low"))
Gene="riskScore"
outTab=c("group",Gene)
write.table(cbind(id=rownames(cbind(rt[,outTab],risk)),cbind(rt[,outTab],risk)),
            file = "riskOutput.txt",sep="\t",quote = F,
            row.names = F)
cutpoint <- data.frame('cutpoint'=cutpoint)
write.table(cutpoint,file = 'cutpoint.txt',row.names = FALSE)

