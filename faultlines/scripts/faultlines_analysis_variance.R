# script analyzes variable operationalizations
#
# .libPaths(c(.libPaths(), '/home/rahnm/R/lib'))

# library(multcomp)
library(futile.logger)
library(data.table)
library(EnvStats)
library(ggplot2)
library(gridExtra)

# parameters:
param.analysis.sample = F
param.analysis.sample.size = 100

param.plot.res = 300
param.plot.exp =  "/Users/Max/Desktop/MA/R/NetworkAnalyzer/faultlines/plots/"
param.plot.width = 12
param.plot.height = 12
param.plot.units = "cm"

param.ops.import = "/Users/Max/Desktop/MA/R/NetworkAnalyzer/faultlines/analysis/faultlines/variation"

# --- define functions

#' import operationalization csv files and coerce them if more than one is being imported
#'
#' @param f: optional project name to import only a single operationalizations file
#' @return list of coerced operationalizations data frame
coerce.ratios <- function(f = NULL) {
  if (is.null(f)) {
    file.list = list.files(path = param.ops.import,
                           pattern = "op*",
                           full.names = TRUE)
    
    if(param.analysis.sample) {
      s <- sample(1:length(file.list), param.analysis.sample.size, replace = F)
      L <- (1:length(file.list)) %in% s
      
      file.list <- file.list[L]
    }
    
  } else {
    file.list = paste(param.ops.import, "/op_df_", f, ".csv", sep = "")
  }
  
  l.a_focus.simple = list()
  l.a_focus.rel = list()
  l.a_focus.rel.sd = list()
  l.experience = list()
  l.a_level = list()
  
  i = 1
  for (file in file.list) {
    ops <- read.csv(file)
    
    l.a_focus.simple[[i]] = get.a_focus.r_simple(ops)
    l.a_focus.rel[[i]] = get.a_focus.r_rel(ops)
    l.a_focus.rel.sd[[i]] = get.a_focus.r_sd(ops)
    l.experience[[i]] = get.experience(ops) 
    l.a_level[[i]] = get.a_level(ops)
    
    i = i + 1
  }
  
  ops.all = list()
  
  ops.all$a_focus.r_simple = rbindlist(l.a_focus.simple)
  ops.all$a_focus.r_rel = rbindlist(l.a_focus.rel)
  ops.all$a_focus.r_sd = rbindlist(l.a_focus.rel.sd)
  ops.all$a_level = rbindlist(l.a_level)
  ops.all$experience = rbindlist(l.experience)
  return(ops.all)
}

#' separates simple ratios from ops data frame
#' @param ops: operationalizations data frame
#' @return data.frame containing simple ops ratios
get.a_focus.r_simple <- function(ops) {
  ops.a_focus.r_simple <- as.data.frame(
    cbind(
      r_code_issue = ops$ratio_code_issue,
      r_code_review_contribution = ops$ratio_code_review_contribution,
      r_issue_reports_discussion = ops$ratio_issue_reports_discussion,
      r_technical_discussion = ops$ratio_technical_discussion
    )
  )
  return (ops.a_focus.r_simple)
}

#' separates relative ratios from ops data frame
#' @param ops: operationalizations data frame
#' @return data.frame containing relative ops
get.a_focus.r_rel <- function(ops) {
  ops.a_focus.r_rel <- as.data.frame(
    cbind(
      r_rel_code_issue = ops$ratio_rel_code_issue,
      r_rel_code_review_contribution = ops$ratio_rel_code_review_contribution,
      r_rel_issue_reports_discussion = ops$ratio_rel_issue_reports_discussion,
      r_rel_technical_discussion = ops$ratio_rel_technical_discussion
    )
  )
  
  return (ops.a_focus.r_rel)
}

#'
#'
#'
get.a_focus.r_sd <- function(ops) {
  ops.a_focus.r_sd <- as.data.frame(
    cbind(
      r_rel_code_issue = ops$ratio_rel_code_issue_sd,
      r_rel_code_review_contribution = ops$ratio_rel_code_review_contribution_sd,
      r_rel_issue_reports_discussion = ops$ratio_rel_issue_reports_discussion_sd,
      r_rel_technical_discussion = ops$ratio_rel_technical_discussion_sd
    )
  )
  
  return (ops.a_focus.r_sd)
}

#'
#'
#'
get.a_level <- function(ops){
  ops.a_level <- as.data.frame(cbind(persistency_simple = ops$persistency_simple,
                                     persistency_deviation = ops$persistency_deviation,
                                     persistency_sd = ops$persistency_sd,
                                     extent_simple = ops$contribution_extent,
                                     extent_share = ops$share_contribution_extent,
                                     r_rel_extent = ops$ratio_rel_contribution_extent,
                                     r_rel_extent_sd = ops$ratio_rel_contribution_extent_sd))
  
  return(ops.a_level)
}

#'
#'
#'
get.experience <- function(ops){
  ops.experience <- as.data.frame(ops$proj_experience) 
}

#' calculates column variances for the provided data frame
#' removes NAs before calculating variances
#' @param df:  data frame containing numeric vectors
#' @return data.frame containing variances for each column
calc.col_var <- function(df) {
  res = list()
  for (i in colnames(df)) {
    res[[i]] = var(df[[i]], na.rm = TRUE)
  }
  return(res)
}


#' converts a data frame with multiple variables in various columns to a data frame with two columns
#' holding the variable value and a factor scalar indicating the variable
#' This conversion is required to display categories in a geom_boxplot or geom_violin next to each other
#' @param ops.df  a data.frame containing multiple variables
#' @return the converted data frame with columns "val" (variable value) and "cat" (indicating the variable name)
ops.vectorize <- function(ops.df){
  ops.vec = data.frame()
  i = 1
  for(col in names(ops.df)) {
    temp <- data.frame(val = ops.df[[col]], cat = rep(i, nrow(ops.df)))
    ops.vec = rbind(temp, ops.vec)
    i = i+1
  }
  
  ops.vec$cat <- as.factor(ops.vec$cat)
  
  ops.vec <- ops.vec[!is.na(ops.vec$val),]
  
  return(ops.vec)
}

#' saves the supplied ggplot under the specified name
#' @param plot  ggplot2 object
#' @param name  filename to save the plot to
save.plot <- function(plot, name){
  png(
    filename = paste(param.plot.exp, name, sep = ""),
    res = param.plot.res,
    width = param.plot.width,
    height = param.plot.height,
    units = param.plot.units
  )
  
  print(plot)
  
  dev.off()
}

#' creates a boxplot for the simple activity focus ratios 
#' @param ops.ratio.simple
ops.boxplot.a_focus.r_simple <- function(ops.ratios.simple) {
  
  vec <- ops.vectorize(ops.ratios.simple)
  
  p <- ggplot(vec, aes( x = cat, y = val)) + 
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab("ratios") +
    ylab("ratio value") +
    ggtitle("Distribution of simple activity focus ratios") +
    theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1)) +
    scale_x_discrete(labels=c(
      "1" = "code v issue \n activity", 
      "2" = "code reviewing v\n contributing",
      "3" = "issue reporting v\n discussing",
      "4" = "tech. contributing v\n discussing")) 
  
  save.plot(p, "boxplot_ops_ratios_simple.png")
}

#' creates a boxplot for the relative activity focus ratios
#' if parameter param.plot.beanplot is set to TRUE, boxplot is generated as beanplot
#' @param ops.ratios.rel
ops.boxplot.a_focus.r_rel <- function(ops.a_focus.r_rel) {

  vec <- ops.vectorize(ops.a_focus.r_rel)
  
  no.obs.1 <- nrow(vec[vec$cat == 1,])
  no.obs.2 <- nrow(vec[vec$cat == 2,])
  no.obs.3 <- nrow(vec[vec$cat == 3,])
  no.obs.4 <- nrow(vec[vec$cat == 4,])
  
  p <- ggplot(vec, aes( x = cat, y = val )) + 
    #geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE)+
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab("ratios") +
    ylab("ratio value") +
    labs(title = "Distribution of relative activity focus ratios",
         subtitle = paste("No. observations:", no.obs.1, no.obs.2, no.obs.3, no.obs.4, sep = "\t")) +
    theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1)) +
    scale_x_discrete(labels=c(
      "1" = "code v issue \n activity", 
      "2" = "code reviewing v\n contributing",
      "3" = "issue reporting v\n discussing",
      "4" = "tech. contributing v\n discussing")) 
  
  save.plot(p, "boxplot_ops_ratios_rel.png")
}
ops.boxplot.a_focus.r_sd <- function(ops.a_focus.r_sd){
  no.obs = nrow(ops.a_focus.r_sd)
  
  vec <- ops.vectorize(ops.a_focus.r_sd)
  
  no.obs.1 <- nrow(vec[vec$cat == 1,])
  no.obs.2 <- nrow(vec[vec$cat == 2,])
  no.obs.3 <- nrow(vec[vec$cat == 3,])
  no.obs.4 <- nrow(vec[vec$cat == 4,])
  
  p <- ggplot(vec, aes( x = cat, y = val )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE) +
    # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab("ratios") +
    ylab("ratio value") +
    labs(title = "Distribution of standardized relative activity focus ratios", 
         subtitle = paste("No. observations:", no.obs.1, no.obs.2, no.obs.3, no.obs.4, sep = "\t")) +
    theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1)) +
    scale_x_discrete(labels=c(
      "1" = "code v issue \n activity", 
      "2" = "code reviewing v\n contributing",
      "3" = "issue reporting v\n discussing",
      "4" = "tech. contributing v\n discussing")) 
  
  save.plot(p, "boxplot_ops_ratios_rel_sd.png")
}

#' creates a boxplot for the activity level variables each
#'
#'
ops.boxplot.a_level.extent <- function(ops.a_level){
  
  png(
    filename = paste(param.plot.exp, "boxplot_activity_extent.png", sep = ""),
    res = param.plot.res,
    width = param.plot.width,
    height = param.plot.height,
    units = param.plot.units
  )
  
  # plot simple extent
  p1 <- ggplot(ops.a_level, aes( x = "activity extent", y = ops.a_level$extent_simple )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE)+
    #geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab(" ") +
    ylab("total number of contributions per participator") +
    labs(subtitle = "simple") 
  
  # plot share extent
  p2 <-  ggplot(ops.a_level, aes( x = "activity extent", y = ops.a_level$extent_share )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE)+
    #geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab("") +
    ylab("individuals' share in total project contributions") +
    labs(subtitle = "share") 
  
  # plot relative extent
  p3 <-  ggplot(ops.a_level, aes( x = "activity extent", y = ops.a_level$r_rel_extent )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE)+
    #geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab("") +
    ylab("deviation from project mean in percentage points") +
    labs(subtitle = "relative share") 
    
  # plot standardized relative extent
  p4 <- ggplot(ops.a_level, aes( x = "activity extent", y = ops.a_level$r_rel_extent_sd )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE)+
    #geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab(" ") +
    ylab("deviation from project mean in standard deviations") +
    labs(subtitle = "standardized relative share") 
  
  # grid.arrange(p1, p2, p3, p4, ncol = 4, top = "Distribution of activity extent")
  grid.arrange(p1, p4, ncol = 2, top = "Distribution of activity extent")
  
  dev.off()
}

#' creats a boxplot for activity persistency
#'
#'
ops.boxplot.a_level.persistency <- function(ops.a_level){
  
  png(
    filename = paste(param.plot.exp, "boxplot_activity_persistency.png", sep = ""),
    res = param.plot.res,
    width = param.plot.width,
    height = param.plot.height,
    units = param.plot.units
  )
  
  p1 <- ggplot(ops.a_level, aes( x = "activity persistency", y = ops.a_level$persistency_simple )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE)+
    #geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab("individuals' share of active periods since first contribution") +
    ylab("") +
    labs(subtitle = "simple") 
  
  p2 <- ggplot(ops.a_level, aes( x = "activity persistency", y = ops.a_level$persistency_deviation )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE)+
    #geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab(" ") +
    ylab("deviation from mean in percentage points") +
    labs(subtitle = "deviation from mean") 
  
  p3 <- ggplot(ops.a_level, aes( x = "activity persistency", y = ops.a_level$persistency_sd )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE)+
    #geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab(" ") +
    ylab("deviation from mean in sd's") +
    labs(subtitle = "standardized deviation from mean") 
  
  
  # grid.arrange(p1, p2, p3, ncol = 3, top = "Distribution of activity persistency")
  grid.arrange(p1, p3, ncol = 2, top = "Distribution of activity persistency")
  
  dev.off()

}

#' creates a pairplot for the relative activity ratios
ops.pairplot.rel <- function(ops.ratios.rel) {
  png(
    filename = paste(param.plot.exp, "pairplot_ops_rel.png", sep = ""),
    res = param.plot.res,
    width = param.plot.width,
    height = param.plot.height,
    units = param.plot.units
  )
  pairs(ops.ratios.rel)
  dev.off()
  
}

#' creates a pairplot for the simple activity ratios
ops.pairplot.simple <- function(ops.ratios.simple) {
  png(
    filename = paste(param.plot.exp, "pairplot_ops_simple.png", sep = ""),
    res = param.plot.res,
    width = param.plot.width,
    height = param.plot.height,
    units = param.plot.units
  )
  
  pairs(ops.ratios.simple)
  
  dev.off()
}

# chi-square tests
# target: test whether the variance in each variable is significantly different from 0

#' tests H0 that variance is equal to 0
#' function varTest comes from the package EnvStats
#' chisq.test from package base has no One-Sample test against specified value implemented
#'
#' @param var  numeric vector
#'
chisq.onesided <- function(var) {
  var <- var[!is.na(var)]
  t <-
    varTest(
      var,
      alternative = "greater",
      conf.level = 0.95,
      sigma.squared = 0.001
    )
  return(t)
}

#' performs a onesided chisquare test for all variables which are being passed in
#' a matrix
#' @param df  data.frame containing numeric vectors
#' @return list of htest objects
ops.test.var <- function (df) {
  res <- list()
  for (i in colnames(df)) {
    res[[i]] = chisq.onesided(df[[i]])
  }
  return(res)
}

#' print results of the chisquare test for all variables in a readable manner
#' @param res list of htest objects
ops.test.var.print <- function(res){
  cat("one sample Chisquare Test",
      "\nH0: Variance is equal to ", res[[1]]$null.value,
      "\nH1: Variance is greater than ", res[[1]]$null.value, 
      "\nconfidence level: 0.95",
      "\n\n")
  
  for (n in names(res)) {
    
      cat(
        n,
        "\n",
        "variance: ",
        res[[n]]$estimate,
        "\n",
        "p-value: ",
        res[[n]]$p.value,
        "\n",
        "lower bound confidence interval ",
        res[[n]]$conf.int[1],
        "\n",
        "\n"
      )
  }
}


### --- do the work
ops.all = coerce.ratios()

# plot the variables
ops.boxplot.a_focus.r_simple(ops.all$a_focus.r_simple)
ops.boxplot.a_focus.r_rel(ops.all$a_focus.r_rel)
ops.boxplot.a_focus.r_sd(ops.all$a_focus.r_sd)

ops.boxplot.a_level.extent(ops.all$a_level)
ops.boxplot.a_level.persistency(ops.all$a_level)

# ops.pairplot.simple(ops.all$a_focus.r_simple)
# ops.pairplot.rel(ops.all$a_focus.r_sd)

# test variances
ops.test.var.print(ops.test.var(ops.all$a_focus.r_simple))
ops.test.var.print(ops.test.var(ops.all$a_focus.r_rel))
ops.test.var.print(ops.test.var(ops.all$a_focus.r_sd))
