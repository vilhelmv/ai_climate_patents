library(ggplot2)
library(dplyr)
library(dotwhisker)

data.dir <- "output/" # results created from using plot3_fitmodels.r

# The group names for the plot
dict_ycode_subclasses <- new.env(hash = TRUE, parent = emptyenv(), size = NA)
dict_ycode_subclasses[["Y02A"]] <- "Adaptation"
dict_ycode_subclasses[["Y02B"]] <- "Buildings"
dict_ycode_subclasses[["Y02C"]] <- "Carbon Capture and Storage"
dict_ycode_subclasses[["Y02D"]] <- "Energy-efficient ICT"
dict_ycode_subclasses[["Y02E"]] <- "Energy technologies"
dict_ycode_subclasses[["Y02P"]] <- "Production"
dict_ycode_subclasses[["Y02T"]] <- "Transport"
dict_ycode_subclasses[["Y02W"]] <- "Waste handling"
dict_ycode_subclasses[["B60-B64"]] <- 'Transport (general)'
dict_ycode_subclasses[["E04/F21"]] <- 'Buildings (general)'
dict_ycode_subclasses[["Y04S"]] <- 'Smart grids (general)'
dict_ycode_subclasses[["H01-H05"]] <- 'Electricity (general)'

# The Y02 groups (Created by using plot3_fitmodels.r and extracting the ai coefficients for mu (mean) from the output)
y02mucoefficients <- read.csv(paste(data.dir,'df_results_plot3_y02codes.csv',sep=''),stringsAsFactors = FALSE)
plotsummary.y02mucoefficients <- list()
for (i in 1:nrow(y02mucoefficients)) {
  modelsummary <- y02mucoefficients[i,]
  group.name <- modelsummary$code
  ai_mean <- modelsummary$Estimate
  ai_stderr <- modelsummary$Std.Error
  ai_lower <- ai_mean-ai_stderr*2
  ai_higher <- ai_mean+ai_stderr*2
  stars <- modelsummary$stars
  pval <- modelsummary$pr.gt
  cat(group.name,ai_lower,ai_mean,ai_higher,pval,stars,"\n")
  plotsummary.y02mucoefficients[[group.name]] <- c(group.name,ai_lower,ai_mean,ai_higher,pval,stars)
}
plotsummary.y02mucoefficients.df <- as.data.frame(t(as.data.frame(plotsummary.y02mucoefficients)))
colnames(plotsummary.y02mucoefficients.df) <- c("group.name","ai_lower","ai_mean","ai_upper","pval","stars")
plotsummary.y02mucoefficients.df$ai_lower <- as.numeric(as.character(plotsummary.y02mucoefficients.df$ai_lower))
plotsummary.y02mucoefficients.df$ai_mean <- as.numeric(as.character(plotsummary.y02mucoefficients.df$ai_mean))
plotsummary.y02mucoefficients.df$ai_upper <- as.numeric(as.character(plotsummary.y02mucoefficients.df$ai_upper))
plotsummary.y02mucoefficients.df$group.name <- factor(plotsummary.y02mucoefficients.df$group.name,levels=plotsummary.y02mucoefficients.df$group.name[order(plotsummary.y02mucoefficients.df$ai_mean,decreasing=TRUE)])
plotsummary.y02mucoefficients.df$term <- lapply(plotsummary.y02mucoefficients.df$group.name, function(x) {dict_ycode_subclasses[[as.character(x)]]})

# The control groups (Created by using plot3_fitmodels.r and extracting the ai coefficients for mu (mean) from the output)
controlmucoefficients <- read.csv(paste(data.dir,'df_results_plot3_controls.csv',sep=''),stringsAsFactors = FALSE)
plotsummary.controlmucoefficients <- list()
for (i in 1:nrow(controlmucoefficients)) {
  modelsummary <- controlmucoefficients[i,]
  group.name <- modelsummary$code
  ai_mean <- modelsummary$Estimate
  ai_stderr <- modelsummary$Std.Error
  ai_lower <- ai_mean-ai_stderr*2
  ai_higher <- ai_mean+ai_stderr*2
  stars <- modelsummary$stars
  pval <- modelsummary$pr.gt
  cat(group.name,ai_lower,ai_mean,ai_higher,pval,stars,"\n")
  plotsummary.controlmucoefficients[[group.name]] <- c(group.name,ai_lower,ai_mean,ai_higher,pval,stars)
}
plotsummary.controls.df <- as.data.frame(t(as.data.frame(plotsummary.controlmucoefficients)))
colnames(plotsummary.controls.df) <- c("group.name","ai_lower","ai_mean","ai_upper","pval","stars")
plotsummary.controls.df$ai_lower <- as.numeric(as.character(plotsummary.controls.df$ai_lower))
plotsummary.controls.df$ai_mean <- as.numeric(as.character(plotsummary.controls.df$ai_mean))
plotsummary.controls.df$ai_upper <- as.numeric(as.character(plotsummary.controls.df$ai_upper))
plotsummary.controls.df$group.name <- factor(plotsummary.controls.df$group.name,levels=plotsummary.controls.df$group.name[order(plotsummary.controls.df$ai_mean,decreasing=TRUE)])
plotsummary.controls.df$term <- lapply(plotsummary.controls.df$group.name, function(x) {dict_ycode_subclasses[[as.character(x)]]})

# Combine results for Y02 and other groups for the plot
df.joined <- rbind(plotsummary.controls.df,plotsummary.y02mucoefficients.df)
df.joined$estimate <- df.joined$ai_mean
df.joined$std.error <- (df.joined$ai_upper-df.joined$ai_mean)/2

b <- levels(plotsummary.y02mucoefficients.df$group.name)
c <- levels(plotsummary.controls.df$group.name)
d <- c(c,b)
e <- rev(lapply(d,function(x) dict_ycode_subclasses[[x]]))
results_df <- df.joined
print(results_df)

results_df$term <- factor(results_df$term,levels=e)
results_df <- with(results_df,results_df[order(term),])
results_df$term <- as.character(results_df$term)
results_df$stars <- as.character(results_df$stars)
# results_df$pval.disp <- as.character(results_df$pval)
results_df$pval.disp <- rep("<0.001",nrow(results_df))
# coordinates for the stars
results_df$stary <- rev(1:nrow(results_df)) 
results_df$starx <- rep(1,nrow(results_df))
results_df$groupx <- rep(1.7,nrow(results_df))
results_df$pvalx <- rep(1.35,nrow(results_df))
# column with class/group codes
results_df$group.name <- as.character(results_df$group.name)

# Plot with chosen brackets
two_brackets <- list(c("Climate patents","Adaptation","Buildings"),
                     c("Related technologies","Electricity (general)","Buildings (general)"))
{results_df %>% 
    dwplot(order_vars=e) + 
    theme_bw() + 
    theme(legend.position="none",axis.title=element_text(size=12), panel.grid.minor = element_blank(),axis.text.y = element_text(size=12),axis.text.x = element_text(size=12)) +
    scale_y_discrete(labels=as.list(dict_ycode_subclasses)) +
    ggtitle("Estimated difference of AI on patent forward citation counts") + 
    geom_segment(x = 0, y=-0.15, xend=0, yend=nrow(results_df)+1,colour = "grey60", linetype = 2,size=0.2) +
    geom_segment(x = 0, y=-0.5, xend=0, yend=-0.15,colour = "white", linetype = 1,size=0.2) +
    geom_segment(x = 1, y=-0.5, xend=1, yend=-0.15,colour = "white", linetype = 1,size=0.2) +
    geom_segment(x = 0.5, y=-0.5, xend=0.5, yend=-0.15,colour = "white", linetype = 1,size=0.2) +
    geom_text(aes(label=stars,x=starx,y=stary),alpha=0.5,size=3) +
    geom_text(aes(label=group.name,x=groupx,y=stary,alpha=0.3),size=3) +
    geom_text(aes(label=pval.disp,x=pvalx,y=stary,alpha=0.3),size=3) +
    coord_cartesian(xlim=c(-0.1,1.8),ylim=c(0,nrow(results_df)+0.55)) +
    scale_x_continuous(breaks=c(0,0.5,1), label=c("0","0.5","1")) +
    labs(x="\nModel coefficients and 95% confidence intervals") +
    annotate("text", x=1.35, y=nrow(results_df)+0.6, label="p",alpha=0.5,size=3) +
    annotate("text", x=1.7, y=nrow(results_df)+0.6, label="code",alpha=0.5,size=3) +
    annotate("text", x=1, y=nrow(results_df)+0.6, label="significance",alpha=0.5,size=3) +
    annotate("text", x=0, y=-0.35, label="+0%",alpha=0.5,size=3) +
    annotate("text", x=0.5, y=-0.35, label="+64%",alpha=0.5,size=3) +
    annotate("text", x=1, y=-0.35, label="+170%",alpha=0.5,size=3)
} %>% add_brackets(two_brackets,fontSize=1.0,face="plain")

# save as, e.g., output/coefplot_revised_similartechnologies.pdf
