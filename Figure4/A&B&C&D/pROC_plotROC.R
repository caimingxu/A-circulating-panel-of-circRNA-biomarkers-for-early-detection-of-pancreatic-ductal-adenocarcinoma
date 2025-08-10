
# install.packages("plotROC")

library(plotROC)
library(pROC)
rt=read.table("riskOutput.txt",header=T,sep="\t",check.names=F,row.names=1)
basicplot=ggplot(rt,aes(d=group,m=riskScore,col="red"))+geom_roc(n.cuts = 0)
#pdf('plotROC.pdf',width = 10,height = 8)
tiff(filename = "PlotROC.tif", width = 3500, height = 3000, bg ="white", res = 600)
basicplot+style_roc()
dev.off()
#styleplot          #??ROC
roc=roc(rt$group,rt$riskScore,plot=FALSE,ci=TRUE)
auc=auc(roc)          #????AUCֵ 
CI=as.numeric(roc$ci)
LL <- CI[1]
HH <- CI[3]
v <- var(roc)
b <- roc$auc - .5
se <- sqrt(v)
z <- (b / se)
p <- 2 * pt(-abs(z), df=Inf)  ## two-sided test p-value

#------------------------------------------------------------------
#To calculate the optimal cutoff, 95%CI, auc, sensitivity, specificity, p-value
roc_result <- coords(roc, "best")
roc_result$auc <- auc
roc_result$'95L'<- LL
roc_result$'95H'<- HH
roc_result$'pValue'<- p

library(xlsx)
library("writexl")
write.table(roc_result,"results.txt",sep="\t")

library(export)
rt=read.table("riskOutput.txt",header=T,sep="\t",check.names=F,row.names=1)
tiff("ROC.tiff",width = 1500,height = 1200,res=300)
basicplot=ggplot(rt,aes(d=group,m=riskScore,col="red"))+geom_roc(n.cuts = 0)
#pdf('plotROC.pdf',width = 10,height = 8)
basicplot+style_roc()
dev.off()
basicplot=ggplot(rt,aes(d=group,m=riskScore,col="red"))+geom_roc(n.cuts = 0)
basicplot+style_roc()
graph2ppt(file="effect plot.pptx", width=7, height=5)


#styleplot          #??ROC
#To show the optimal cutoff on ROC curve
#pdf('Show Cutoff on ROC.pdf',width = 10,height = 8)
#rocobj <- roc(rt$group, rt$riskScore)
#plot(rocobj,
#     legacy.axes = TRUE,
#     main="Best cutoff",
#     thresholds="best", # the best cutoff based on youden index
#     print.thres="best") # show the best cutoff on ROC curve
#dev.off()







###Video source: http://study.163.com/provider/1026136977/index.htm?share=2&shareId=1026136977
######Video source: http://www.biowolf.cn/shop/
######??????ѧ??: http://www.biowolf.cn/
######???????�?2749657388@qq.com
######????΢??: 18520221056