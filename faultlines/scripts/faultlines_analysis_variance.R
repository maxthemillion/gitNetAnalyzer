# script analyzes variable operationalizations
#
# .libPaths(c(.libPaths(), '/home/rahnm/R/lib'))

library(multcomp)
library(futile.logger)
library(beanplot)
library(data.table)
library(EnvStats)

# parameters:

param.plot.res = 300
param.plot.exp =  "/Users/Max/Desktop/MA/R/NetworkAnalyzer/faultlines/plots/"
param.plot.width = 12
param.plot.height = 12
param.plot.units = "cm"
param.plot.beanplot = FALSE

param.ops.import = "/Users/Max/Desktop/MA/R/NetworkAnalyzer/faultlines/analysis/faultlines/variation"

# --- define functions

#' import operationalization csv files and coerce them if more than one is being imported
#'
#' @param f: optional file path to import only a single operationalizations file
#' @return list of coerced operationalizations data frame
coerce.ratios <- function(f = NULL) {
  if (is.null(f)) {
    file.list = list.files(path = param.ops.import,
                           pattern = "op*",
                           full.names = TRUE)
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

#' creates a boxplot for the simple ratios
#' @param ops.ratio.simple
ops.boxplot.simple <- function(ops.ratios.simple) {
  png(
    filename = paste(param.plot.exp, "boxplot_ops_ratios_simple.png", sep = ""),
    res = param.plot.res,
    width = param.plot.width,
    height = param.plot.height,
    units = param.plot.units
  )
  
  if (!param.plot.beanplot) {
    boxplot(
      ops.ratios.simple,
      xlab = "ratios",
      ylab = "ratio value",
      main = "Distribution of activity focus ratios",
      names = c(
        "code v issue",
        "code rev. v cont.",
        "issue rep. v disc.",
        "tech. v disc."
      ),
      las = 2
    )
  } else{
    beanplot(
      ops.ratios.simple,
      xlab = "ratios",
      ylab = "ratio value",
      main = "Distribution of activity focus ratios",
      names = c(
        "code v issue",
        "code rev. v cont.",
        "issue rep. v disc.",
        "tech. v disc."
      ),
      las = 2
    )
  }
  dev.off()
}

#' creates a boxplot for the relative ratios
#' if parameter param.plot.beanplot is set to TRUE, boxplot is generated as beanplot
#' @param ops.ratios.rel
ops.boxplot.rel <- function(ops.ratios.rel) {
  no.observations = nrow(ops.ratios.rel)
  
  png(
    filename = paste(param.plot.exp, "boxplot_ops_ratios_rel.png", sep = ""),
    res = param.plot.res,
    width = param.plot.width,
    height = param.plot.height,
    units = param.plot.units
  )
  
  boxplot(
    ops.ratios.rel,
    xlab = "ratios",
    ylab = "ratio value",
    main = "Distribution of relative activity focus ratios",
    sub = paste("No. of observations:", no.observations, sep = " "),
    names = c(
      "code v issue",
      "code rev. v cont.",
      "issue rep. v disc.",
      "tech. v disc."
    ),
    las = 2
  )
  abline(h = 1, col = "red", lty = 2)
  
  dev.off()
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

ops.ratios.simple.var = calc.col_var(ops.all[[1]])
ops.ratios.rel.var = calc.col_var(ops.all[[2]])

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
#' @param var:  vector of numeric values
#' 
chisq.onesided <- function(var){
  var <- var[!is.na(var)]
  t <- varTest(var, alternative = "greater", conf.level = 0.95, sigma.squared = 0.01)
  return(t)
}

#' performs a onesided chisquare test for all variables which are being passed in 
#' a matrix
#' @param df:  data.frame containing numeric vectors
#' @return list of htest objects  
ops.test.var <- function (df){
  res <- list()
  for (i in colnames(df)){
    res[[i]] = chisq.onesided(df[[i]])
  }
  return(res)
}

res.ops.ratios.simple.var = ops.test.var(ops.all[[1]])
res.ops.ratios.rel.var = ops.test.var(ops.all[[2]])



