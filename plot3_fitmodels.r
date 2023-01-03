# The code below will fit several regression models (specified below) and output model summaries to a file

library(vcd)
library(MASS)
library(countreg)
library(car)
library(gamlss)
library(ggplot2)
library(ggpubr)
library(moments)
library(dplyr)
library(tidyr)
library(stringr)
options(max.print=2000)

n.cpu.cores <- 12
data.dir <- "data/regression/"

dict_ycode_subclasses <- new.env(hash = TRUE, parent = emptyenv(), size = NA)
dict_ycode_subclasses[["Y02A"]] <- "Adaptation"
dict_ycode_subclasses[["Y02B"]] <- "Buildings"
dict_ycode_subclasses[["Y02C"]] <- "Carbon Capture and Storage"
dict_ycode_subclasses[["Y02D"]] <- "Energy-efficient ICT"
dict_ycode_subclasses[["Y02E"]] <- "Energy technologies"
dict_ycode_subclasses[["Y02P"]] <- "Production"
dict_ycode_subclasses[["Y02T"]] <- "Transport"
dict_ycode_subclasses[["Y02W"]] <- "Waste handling"

regress_class <- function (classcode, ncyc, thresholdnz, subset_sample_size) {
  fname <- paste(data.dir,classcode,'.csv',sep='')
  df_raw <- read.csv(fname)
  df_raw$organizational <- as.integer(df_raw$n_inventors_org>0)
  cat(classcode, ": before removing NAs",dim(df_raw),"\n")
  df <- df_raw %>% drop_na()
  cat(classcode, ": after removing NAs",dim(df),"\n")
  df <- within(df, {
    ipc_class <- factor(ipc_class)
    ipc_subclass <- factor(ipc_subclass)
    grantyear <- factor(grantyear)
    green_ycodes <- factor(green_ycodes,levels=0:1,labels = c("non-green","green"))
    ai <- factor(ai,levels=0:1,labels=c("non-ai","ai"))
    Y02A <- factor(Y02A, levels=0:1,labels=c("0","1"))
    Y02B <- factor(Y02B, levels=0:1,labels=c("0","1"))
    Y02C <- factor(Y02C, levels=0:1,labels=c("0","1"))
    Y02D <- factor(Y02D, levels=0:1,labels=c("0","1"))
    Y02E <- factor(Y02E, levels=0:1,labels=c("0","1"))
    Y02P <- factor(Y02P, levels=0:1,labels=c("0","1"))
    Y02T <- factor(Y02T, levels=0:1,labels=c("0","1"))
    Y02W <- factor(Y02W, levels=0:1,labels=c("0","1"))
    organizational <- factor(organizational, levels=0:1,labels=c("0","1"))
    tct_type <- factor(tct_type)
    tct_type <- factor(tct_type, levels=c("<5","5-10","10-15",">15"))
    n_forward_citations_4y_p1 <- n_forward_citations_4y+1
    claims_log_sqrt <- sqrt(claims_log)
    claims_log_square <- claims_log**2
    backward_citations_log1p_sqrt <- sqrt(backward_citations_log1p)
    n_backward_npl_citations_log1p_sqrt <- sqrt(n_backward_npl_citations_log1p)
    ipc_group <- factor(ipc_group)  
  })
  # select the relevant period
  udf <- df[df$grantyear==2017|df$grantyear==2016|df$grantyear==2015|df$grantyear==2014|df$grantyear==2013|df$grantyear==2012|df$grantyear==2011|df$grantyear==2010,]
  udf <- udf %>% 
    rename(
      claims_log = claims_log,
      individual_inventors_log = n_inventors_ind_log,
      patent_citations_log = backward_citations_log1p,
      research_citations_log = n_backward_npl_research_citations_log1p,
      other_citations_log = n_backward_npl_nonresearch_citations_log1p,
      ai_citations_log = n_backward_npl_ai_citations_log1p,
      cs_citations_log = n_backward_npl_cs_citations_log1p
    )

  # Controls for the different subclasses
  features.technologies <- character()
  features.grouping <- character()
  features.ycodes.subclasses <- character()
  feature.names <- colnames(df_raw)
  for (feature.name in feature.names) {
    if (str_count(feature.name,"[.]") == 0) {
      if (startsWith(feature.name,"subclass_")) { 
        features.ycodes.subclasses <- c(features.ycodes.subclasses, feature.name)
        udf[[feature.name]] <- factor(udf[[feature.name]],levels=0:1,labels=c("0","1"))
      }
    }
    if (str_count(feature.name,"[.]") == 1) {
      features.ycodes.subclasses <- c(features.ycodes.subclasses, feature.name)
      udf[[feature.name]] <- factor(udf[[feature.name]],levels=0:1,labels=c("0","1"))
    }
    if (str_count(feature.name,"[.]") == 2) {
      features.ycodes.subclasses <- c(features.ycodes.subclasses, feature.name)
      udf[[feature.name]] <- factor(udf[[feature.name]],levels=0:1,labels=c("0","1"))
    }
  }
  features.ycodes.subclasses <- sort(features.ycodes.subclasses)
  udf.ycodes.subclasses <- list()
  for (feature.ycodes.subclasses in features.ycodes.subclasses) {
    ycode_subclass_indices <- as.integer(as.character(udf[[feature.ycodes.subclasses]]))
    # include covariates with positive patent counts
    ycode_subclass <- subset(udf,ycode_subclass_indices==1)
    udf.ycodes.subclasses[[feature.ycodes.subclasses]] <- ycode_subclass
  }

  # if #patents are more than set computational limit, subsample
  subset_sample_size_bounded <- min(dim(udf)[1], subset_sample_size)
  subclass <- sample_n(udf, subset_sample_size_bounded)

  # possibly filter out very small subclasses/control variables with very few remaining patents (not done by default)
  # add controls to the list of covariates
  tmp <- subclass[features.ycodes.subclasses]
  tmp <- mutate_if(tmp, is.factor, ~ as.numeric(levels(.x))[.x])
  tmps <- colSums(tmp)
  tmpsnz <- tmps[tmps>=thresholdnz]
  tmpsnznames <- names(tmpsnz)
  tmpsnznamesycode <- tmpsnznames
  tmpexpr <- paste(sort(tmpsnznamesycode),collapse='+')

  # make sure a sufficient share of AI patents are included (against unbalanced data if/when subsampling)
  size_limit_ai <- subset_sample_size/4
  subclass_ai_patents <- subclass[subclass$ai=="ai",]
  if (dim(subclass_ai_patents)[1]>size_limit_ai) {
    subclass_ai_patents_target <- sample_n(subclass_ai_patents,size_limit_ai)
  } else {
    subclass_ai_patents_target <- subclass_ai_patents
  }
  
  size_limit <- subset_sample_size_bounded - dim(subclass_ai_patents_target)[1]
  subclass_nonai_patents <- subclass[subclass$ai=="non-ai",]
  if (dim(subclass_nonai_patents)[1]>size_limit) {
    subclass_nonai_patents_target <- sample_n(subclass_nonai_patents,size_limit)
  } else {
    subclass_nonai_patents_target <- subclass_nonai_patents
  }
  subclass_target <- rbind(subclass_ai_patents_target,subclass_nonai_patents_target)
  cat(classcode,dim(subclass_target),"\n")
  
  # set the data to the subsampled one
  subclass <- subclass_target
    
  # formula for the conditional mean (mu)
  modelformula.str <- paste("n_forward_citations_3y ~", tmpexpr, "+tct_type*grantyear + grantyear + ai + organizational + claims_log + individual_inventors_log + patent_citations_log + research_citations_log + other_citations_log")
  modelformula <- as.formula(modelformula.str)
  print(modelformula)

  # formula for location/scale/shape parameters
  modelformula.LSS.str <- paste("~", tmpexpr, "+tct_type*grantyear + grantyear + organizational+claims_log+individual_inventors_log+patent_citations_log+research_citations_log+other_citations_log")
  modelformula.LSS <- as.formula(modelformula.LSS.str)
  m.sichel.simple.ycodes <- gamlss(data=subclass,
                                   family=SICHEL,
                                   formula=modelformula,
                                   sigma.formula=modelformula.LSS,
                                   nu.formula=modelformula.LSS,
                                   n.cyc=ncyc,trace=TRUE,c.crit=0.05)
  cat("DONE fit",classcode,"\n")
  summary.m.sichel.simple.ycodes <- summary(m.sichel.simple.ycodes)
  cat("DONE summary",classcode,"\n")
  l <- list()
  l[['name']] <- classcode
  l[['model']] <- m.sichel.simple.ycodes
  l[['modelsummary']] <- summary.m.sichel.simple.ycodes
  return(l)
}

recursivelyTry <- function(classcode) {
  cat("Fitting model for",classcode,"\n") # Repeat the regression until it fits, in case there would be numerical error
  res <- 
    tryCatch({regress_class(classcode, 10, 0, 75000)}, # 10 or 20 are reasonable choices for number of iteration cycles
             error = function(e) {recursivelyTry(classcode)}
    )
}

# run the models in parallel
target.classes.mainpaper <- c('Y02A','Y02B','Y02D','Y02E','Y02P','Y02T','transport','buildings','smartgrids','electricity') 
target.classes <- target.classes.mainpaper

fname_output <- "output/plot3_fitmodels_output.txt"
sink(fname_output)
library(doParallel)
registerDoParallel(cores=n.cpu.cores)
fitted.models.ycodes.class <- 
  foreach (i=1:length(target.classes)) %dopar% {
    classcode <- target.classes[[i]]
    print(classcode)
    recursivelyTry(classcode)
  }
sink()

# see the file fname_output for estimated ai coefficients for mu models
# the coefficients are linear on the log mean scale; exponentiated these are multiciplicative contributions
# for visualising the results with plot3_visualize.r variables are extracted as:
# 
# code: fitted class/subclass
# Estimate: ai estimate, mu
# Std.Error: Std Error, mu
# t.value: t value, mu
# pr.gt: Pr(>|t|), mu
# stars: from the summary output
