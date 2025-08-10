library(pROC)
library(ROCR)
library(export)
rt <- read.table("riskOutput.txt",header = T,sep = "\t",check.names = F,row.names = 1)
rocobj <- roc(rt$group, rt$riskScore,auc = TRUE,
              
              ci=TRUE, # compute AUC (of AUC by default)
              
              print.auc=TRUE) # print the AUC (will contain the CI)

ciobj <- ci.se(rocobj, # CI of sensitivity
               
               specificities=seq(0, 1, 0.01)) # over a select set of specificities

auc<-auc(rocobj)[1]
auc_low<-ci(rocobj,of="auc")[1]
auc_high<-ci(rocobj,of="auc")[3]
auc_full<-paste("AUC:",round(auc,digits = 3),"(",
                round(auc_low,digits = 3),",",round(auc_high,digits = 3),")",sep = "")

data_ci<-ciobj[1:101,1:3]
data_ci<-as.data.frame(data_ci)
x=as.numeric(rownames(data_ci))
data_ci<-data.frame(x,data_ci)
library(ggplot2)
ggroc(rocobj,color="red",size=1)+theme_bw()+
  geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  geom_ribbon(data = data_ci,aes(x=x,ymin=X2.5.,ymax=X97.5.), fill = 'lightblue',alpha=0.5)+
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        #legend.title=title, 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))+
  labs(x="Specificity",y="Sensitivity")
graph2ppt(file="ESCC.pptx", width=7, height=6)

