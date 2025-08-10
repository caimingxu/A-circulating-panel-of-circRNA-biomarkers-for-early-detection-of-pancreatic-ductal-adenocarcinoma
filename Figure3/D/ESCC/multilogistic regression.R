## install.packages("DAAG")
library(DAAG)
library(survminer)
rt=read.table("input.txt",header=T,sep="\t",row.names = 1)
model <- glm(group ~ .,family=binomial(link="logit"), data=rt)
multilogistic=step(model,direction = "both")    #????AICֵ??Coxģ?ͽ????ٴι??ˣ??????????ι??ˣ?ֻ????Lasso?ع?ɸѡ??????Ҫ??????Cox?ع?????ɸѡ???????Բ???????һ?У?
logisticSummary=summary(multilogistic)
outTab=data.frame()
outTab=cbind(coef=logisticSummary$coefficients, 
             pvalue=logisticSummary$coefficients[,"Pr(>|z|)"]) 

CI_0.95<-confint(model)
CI_0.95<-as.data.frame(cbind(id=row.names(CI_0.95),CI_0.95))
outTab<-as.data.frame(cbind(id=row.names(outTab),outTab))

outTab<-dplyr::inner_join(outTab,CI_0.95,by ="id")
write.table(outTab,file="multilogistic.xls",sep = "\t",quote = F,col.names = T,row.names = F)
