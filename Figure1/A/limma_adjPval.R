######Video source: https://shop119322454.taobao.com
#source("http://bioconductor.org/biocLite.R")
#biocLite("limma")
#biocLite("impute")

logFoldChange=1
adjustP=0.05

library(limma)
library("impute")
rt=read.table("sampleExp.txt",sep="\t",header=T)
rt=as.matrix(rt)
rownames(rt)=rt[,1]
exp=rt[,2:ncol(rt)]
dimnames=list(rownames(exp),colnames(exp))
exp=matrix(as.numeric(as.matrix(exp)),nrow=nrow(exp),dimnames=dimnames)

#impute missing expression data
mat=impute.knn(exp)
rt=mat$data

rt=avereps(rt)     #??????应????探??取??值
#normalize
#pdf(file="rawBox.pdf")
#boxplot(rt,col = "blue",xaxt = "n",outline = F)
#dev.off()
rt=normalizeBetweenArrays(as.matrix(rt))
#pdf(file="normalBox.pdf")
#boxplot(rt,col = "red",xaxt = "n",outline = F)
#dev.off()

rt=log2(rt+1)             #取log2值

#differential
#class <- c("con","con","treat","con","treat","treat")
class <- c(rep("con",20),rep("treat",20))    数字需要修改
design <- model.matrix(~0+factor(class))
colnames(design) <- c("con","treat")
fit <- lmFit(rt,design)
cont.matrix<-makeContrasts(treat-con,levels=design)
fit2 <- contrasts.fit(fit, cont.matrix)
fit2 <- eBayes(fit2)

allDiff=topTable(fit2,adjust='fdr',number=200000)
write.table(allDiff,file="limmaTab.xls",sep="\t",quote=F)

#火山图
gene = allDiff
library(ggplot2)

gene[which(gene$P.Value < 0.01 & gene$logFC <= -1),'sig'] <- 'Down'
gene[which(gene$P.Value < 0.01 & gene$logFC >= 1),'sig'] <- 'Up'
gene[which(gene$P.Value >= 0.01 | abs(gene$logFC) < 1),'sig'] <- 'None'
p <- ggplot(gene, aes(x = logFC, y = -log10(P.Value), color = sig)) +
  geom_point(alpha = 0.6, size = 1) + scale_x_continuous(breaks = seq(-7,7,1))+scale_y_continuous(breaks = seq(0,16,2)) +  
  scale_colour_manual(values  = c('red2', 'blue2', 'gray'), limits = c('Up', 'Down', 'None')) +
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), plot.title = element_text(hjust = 0.5)) +
  theme(legend.key = element_rect(fill = 'transparent'), legend.background = element_rect(fill = 'transparent'), legend.position = c(0.9, 0.93)) +
  geom_vline(xintercept = c(-1, 1), color = 'gray', size = 0.3) +
  geom_hline(yintercept = -log(0.01, 10), color = 'gray', size = 0.3) +
  labs(x = '\nLog2 Fold Change', y = '-Log10(p_value)\n', color = '', title = 'Cancer vs Normal\n')
ggsave('gene.png', p, width = 5, height = 6)

write.table(file="diff.xls",sep="\t",quote=F)
#write table
diffSig <- allDiff[with(allDiff, (abs(logFC)>logFoldChange & adj.P.Val < adjustP )), ]
write.table(diffSig,file="diff.xls",sep="\t",quote=F)
diffUp <- allDiff[with(allDiff, (logFC>logFoldChange & adj.P.Val < adjustP )), ]
write.table(diffUp,file="up.xls",sep="\t",quote=F)
diffDown <- allDiff[with(allDiff, (logFC<(-logFoldChange) & adj.P.Val < adjustP )), ]
write.table(diffDown,file="down.xls",sep="\t",quote=F)

#write expression level of diff gene
hmExp=rt[rownames(diffSig),]
diffExp=rbind(id=colnames(hmExp),hmExp)
write.table(diffExp,file="diffExp.txt",sep="\t",quote=F,col.names=F)

#volcano
pdf(file="vol.pdf")
xMax=max(-log10(allDiff$adj.P.Val))
yMax=max(abs(allDiff$logFC))
plot(-log10(allDiff$adj.P.Val), allDiff$logFC, xlab="-log10(adj.P.Val)",ylab="logFC",
     main="Volcano", xlim=c(0,xMax),ylim=c(-yMax,yMax),yaxs="i",pch=20, cex=0.4)
diffSub=subset(allDiff, adj.P.Val<adjustP & logFC>logFoldChange)
points(-log10(diffSub$adj.P.Val), diffSub$logFC, pch=20, col="red",cex=0.4)
diffSub=subset(allDiff, adj.P.Val<adjustP & logFC<(-logFoldChange))
points(-log10(diffSub$adj.P.Val), diffSub$logFC, pch=20, col="green",cex=0.4)
abline(h=0,lty=2,lwd=3)
dev.off()
