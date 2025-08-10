
library(pROC) # ??????pROC???
library(export)

rt <- read.table("input.txt",header = T,sep = "\t",row.names = 1,check.names = F)

plot.roc(rt$group, rt$hsa_circRNA_400091,
         
         main="Confidence interval of a threshold", percent=TRUE,
         
         ci=TRUE, of="thresholds", # compute AUC (of threshold)
         
         thresholds="best", # select the (best) threshold
         
         print.thres="best") # also highlight this threshold on the plot
graph2ppt(file="hsa_circRNA_400091.pptx", width=10, height=10)

