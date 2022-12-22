library(tidyr)
library(plyr)
library(dplyr)
library(countreg)
library(vcdExtra)

# Generated from plot45.py and visualized below
df.results <- read.csv("output/df_results_plot45.csv")

show.mosaics <- function (target,title.name) {
  row <- df.results[df.results$target==target,]
  df.hyper.C <- data.frame(rbind(c(0,0,row$count0),c(0,1,row$count1),c(1,0,row$count2),c(1,1,row$count3)))
  colnames(df.hyper.C) <- c("source_green","target_ai","Freq")
  df.hyper.C$Freq <- as.numeric(as.character(df.hyper.C$Freq))
  tab.hyper.C <- xtabs(Freq ~ target_ai+source_green, df.hyper.C)
  rownames(tab.hyper.C) <- c("non-AI","AI")
  colnames(tab.hyper.C) <- c("non-climate","climate")
  df.hyper.residuals <- data.frame(rbind(c(0,0,row$residual0),c(0,1,row$residual1),c(1,0,row$residual2),c(1,1,row$residual3)))
  colnames(df.hyper.residuals) <- c("source_green","target_ai","residual")
  df.hyper.residuals$residual <- as.numeric(as.character(df.hyper.residuals$residual))
  tab.hyper.residuals <- xtabs(residual ~ target_ai+source_green, df.hyper.residuals)
  p.value <- round(row$p_value,3)
  cat("p.value",p.value)
  
  fname <- paste('output/mosaic_',target,'_hypergeom.pdf',sep='')
  pdf(fname)
  mosaic(tab.hyper.C,shade=TRUE,gp_varnames=gpar(fontsize=12),gp_labels=gpar(fontsize=11),gp_args=c(p.value=p.value),residuals=tab.hyper.residuals,residuals_type="z-score",legend=TRUE,labeling_args = list(set_varnames=c(target_ai=paste(title.name,"patents, accumulated citations\n"),source_green="Citations from subsequent patents\n")))
  dev.off()
}

show.mosaics('All','All')
show.mosaics('Y02','Climate')
show.mosaics('Y02A','Adaptation')
show.mosaics('Y02B','Buildings')
show.mosaics('Y02D','Energy-efficient ICT')
show.mosaics('Y02E','Energy')
show.mosaics('Y02P','Production')
show.mosaics('Y02T','Transport')
