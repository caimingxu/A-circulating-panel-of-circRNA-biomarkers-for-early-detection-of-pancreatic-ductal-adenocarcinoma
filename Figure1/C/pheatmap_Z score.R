library(data.table)
library(dplyr)
library(ConsensusClusterPlus)
library(readxl)
library(ComplexHeatmap)
library(pheatmap)

data <- fread(file="heatmap.txt") %>% as.data.frame()
data.m <- data %>% tibble::column_to_rownames(var = "ID")
annotation=read.table("group.txt",sep="\t",header=T,row.names=1)

#transform into z-score by row (genes)
data.m <- t(scale(t(data.m)))
data.m = sweep(data.m,1, apply(data.m,1,median,na.rm=T))

tiff(file="heatmap1.tiff",width = 18,height = 12,units ="cm",compression="lzw",bg="white",res=300)
pheatmap(data.m, annotation=annotation,fontsize_row=7,fontsize_col=5)
dev.off()

tiff(file="heatmap2.tiff",width = 18,height = 12,units ="cm",compression="lzw",bg="white",res=300)
pheatmap(data.m, annotation=annotation, color = colorRampPalette(c("green", "black", "red"))(50),fontsize_row=8,fontsize_col=5)
dev.off()

tiff(file="heatmap3.tiff",width = 26,height = 12,units ="cm",compression="lzw",bg="white",res=300)
pheatmap(data.m, annotation=annotation, display_numbers = TRUE,fontsize_number = 5,fontsize_row=8,fontsize_col=5)
dev.off()

tiff(file="heatmap4.tiff",width = 18,height = 8,units ="cm",compression="lzw",bg="white",res=300)
pheatmap(data.m, annotation=annotation, cluster_cols = FALSE,fontsize_row=8,fontsize_col=5)
dev.off()

#the agglomeration method to be used. This should be (an unambiguous abbreviation of) one of "ward.D",
# "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
tiff(file="heatmap5.tiff",width = 18,height = 12,units ="cm",compression="lzw",bg="white",res=300)
pheatmap(data.m, annotation=annotation, clustering_method = "median",fontsize_row=8,fontsize_col=5)
dev.off()

tiff(file="heatmap6.tiff",width = 18,height = 12,units ="cm",compression="lzw",bg="white",res=300)
pheatmap(data.m, annotation=annotation, clustering_method = "mcquitty",fontsize_row=8,fontsize_col=5)
dev.off()
