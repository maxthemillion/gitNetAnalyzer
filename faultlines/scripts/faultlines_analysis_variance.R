# script analyzes variable operationalizations
#
# .libPaths(c(.libPaths(), '/home/rahnm/R/lib'))

# library(multcomp)
library(futile.logger)
library(data.table)
library(EnvStats)
library(ggplot2)

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
  
  l.ratios.simple = list()
  l.ratios.rel = list()
  i = 1
  for (file in file.list) {
    ops <- read.csv(file)
    
    ops.ratios.simple = get.ratios.simple(ops)
    ops.ratios.rel = get.ratios.rel(ops)
    
    l.ratios.simple[[i]] = ops.ratios.simple
    l.ratios.rel[[i]] = ops.ratios.rel
    i = i + 1
  }
  
  ops.all = list()
  ops.all[[1]] = rbindlist(l.ratios.simple)
  ops.all[[2]] = rbindlist(l.ratios.rel)
  
  return(ops.all)
}

#' separates simple ratios from ops data frame
#' @param ops: operationalizations data frame
#' @return data.frame containing simple ops ratios
get.ratios.simple <- function(ops) {
  ops.ratios.simple <- as.data.frame(
    cbind(
      r_code_issue = ops$ratio_code_issue,
      r_code_review_contribution = ops$ratio_code_review_contribution,
      r_issue_reports_discussion = ops$ratio_issue_reports_discussion,
      r_technical_discussion = ops$ratio_technical_discussion
    )
  )
  return (ops.ratios.simple)
}

#' separates relative ratios from ops data frame
#' @param ops: operationalizations data frame
#' @return data.frame containing relative ops
get.ratios.rel <- function(ops) {
  ops.ratios.rel <- as.data.frame(
    cbind(
      r_rel_code_issue = ops$ratio_rel_code_issue,
      r_rel_code_review_contribution = ops$ratio_rel_code_review_contribution,
      r_rel_issue_reports_discussion = ops$ratio_rel_issue_reports_discussion,
      r_rel_technical_discussion = ops$ratio_rel_technical_discussion
    )
  )
  
  return (ops.ratios.rel)
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

#' creates a boxplot for the simple ratios
#' @param ops.ratio.simple
ops.boxplot.simple <- function(ops.ratios.simple) {
  
  vec <- ops.vectorize(ops.ratios.simple)
  
  p <- ggplot(vec, aes( x = cat, y= val)) + 
    #geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE) +
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

#' creates a boxplot for the relative ratios
#' if parameter param.plot.beanplot is set to TRUE, boxplot is generated as beanplot
#' @param ops.ratios.rel
ops.boxplot.rel <- function(ops.ratios.rel) {
  no.observations = nrow(ops.ratios.rel)
  
  vec <- ops.vectorize(ops.ratios.rel)
  
  p <- ggplot(vec, aes( x = cat, y = val )) + 
    #geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE)+
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab("ratios") +
    ylab("ratio value") +
    ggtitle("Distribution of relative activity focus ratios") +
    theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1)) +
    scale_x_discrete(labels=c(
      "1" = "code v issue \n activity", 
      "2" = "code reviewing v\n contributing",
      "3" = "issue reporting v\n discussing",
      "4" = "tech. contributing v\n discussing")) +
    ylim(0,5) +
    geom_hline(yintercept=1, linetype="dashed", color = "red")
  
  save.plot(p, "boxplot_ops_ratios_rel.png")
}

#' creates a pairplot for the relative ratios
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

#' creates a pairplot for the simple ratios
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


### --- do the work

ops.all = coerce.ratios()

ops.boxplot.simple(ops.all[[1]])
ops.boxplot.rel(ops.all[[2]])

ops.pairplot.simple(ops.all[[1]])
ops.pairplot.rel(ops.all[[2]])

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

res.ops.ratios.simple.var = ops.test.var(ops.all[[1]])
res.ops.ratios.rel.var = ops.test.var(ops.all[[2]])

ops.test.var.print(res.ops.ratios.simple.var)
ops.test.var.print(res.ops.ratios.rel.var)

