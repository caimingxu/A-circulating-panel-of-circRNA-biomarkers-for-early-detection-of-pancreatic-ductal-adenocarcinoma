library(riskRegression)
library(export)

rt=read.table("input.txt",header=T,sep="\t",row.names = 1)
rt<- as.data.frame(rt)

model1 <- as.formula(group ~ CA199)
model2 <- as.formula(group ~ circRNA)
model3 <- as.formula(group ~ riskScore)

########## Calibration curve #############
fit1 <- glm(model1,data = rt,family = binomial())
fit2 <- glm(model2,data = rt,family = binomial())
fit3 <- glm(model3,data = rt,family = binomial())

xb <- Score(list("fit2"=fit2),formula=group~1,
            null.model = FALSE,
            conf.int = TRUE,
            plots = c("calibration","ROC"),
            metrics = c("auc","brier"),
            B=1000,M=50,
            data=rt)

xb <- Score(list("fit1"=fit1,
                 "fit2"=fit2,
                 "fit3"=fit3),formula=group~1,
            null.model = FALSE,
            conf.int = TRUE,
            plots = c("calibration","ROC"),
            metrics = c("auc","brier"),
            B=1000,M=50,
            data=rt)

plotCalibration(xb)

graph2ppt(file="calibration.pptx", width=10, height=8)
