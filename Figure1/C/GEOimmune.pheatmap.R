###Video source: http://study.163.com/provider/1026136977/index.htm?share=2&shareId=1026136977
######Video source: http://www.biowolf.cn/shop/
######生信自学网: http://www.biowolf.cn/
######QQ：2749657388
######交流Q群：219795300
######微信: 18520221056

#install.packages("pheatmap")

rt=read.table("emerge.txt",sep="\t",header=T,row.names=1,check.names=F)
rt=t(rt)

library(pheatmap)
Type=c(rep("con",20),rep("treat",20))    #修改对照和处理组样品数目
names(Type)=colnames(rt)
Type=as.data.frame(Type)

pdf("heatmap.pdf",height=5,width=10)
pheatmap(rt, 
         annotation=Type, 
         color = colorRampPalette(c("green", "black", "red"))(50),
         cluster_cols =F,
         fontsize = 8,
         fontsize_row=7,
         fontsize_col=5)
dev.off()

###Video source: http://study.163.com/provider/1026136977/index.htm?share=2&shareId=1026136977
######Video source: http://www.biowolf.cn/shop/
######生信自学网: http://www.biowolf.cn/
######QQ：2749657388
######交流Q群：219795300
######微信: 18520221056