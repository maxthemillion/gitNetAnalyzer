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

old <- theme_set(theme_gray())
theme_update(axis.title = element_text(size = rel(0.65)))
theme_update(axis.text = element_text(size = rel(0.5)))

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
  l.a_focus.sd = list()
  l.experience = list()
  l.a_level = list()
  l.reputation = list()
  
  i = 1
  for (file in file.list) {
    ops <- read.csv(file)
    
    l.a_focus.simple[[i]] = get.a_focus.r_simple(ops)
    l.a_focus.sd[[i]] = get.a_focus.r_sd(ops)
    l.experience[[i]] = get.experience(ops) 
    l.a_level[[i]] = get.a_level(ops)
    l.reputation[[i]] = get.reputation(ops)
    
    i = i + 1
  }
  
  ops.all = list()
  
  ops.all$a_focus.simple = rbindlist(l.a_focus.simple)
  ops.all$a_focus.sd = rbindlist(l.a_focus.sd)
  ops.all$a_level = rbindlist(l.a_level)
  ops.all$experience = rbindlist(l.experience)
  ops.all$reputation = rbindlist(l.reputation)
  
  return(ops.all)
}

#' separates simple ratios from ops data frame
#' @param ops: operationalizations data frame
#' @return data.frame containing simple ops ratios
get.a_focus.simple <- function(ops) {
  ops.a_focus.simple <- as.data.frame(
    cbind(
      code_issue = ops$ratio_code_issue,
      code_review_contribution = ops$ratio_code_review_contribution,
      issue_reports_discussion = ops$ratio_issue_reports_discussion,
      technical_discussion = ops$ratio_technical_discussion
    )
  )
  return (ops.a_focus.simple)
}

#' separates standardized relative activity focus ratios from op df
#'
#'
get.a_focus.sd <- function(ops) {
  ops.a_focus.sd <- as.data.frame(
    cbind(
      code_issue = ops$ratio_code_issue_sd,
      code_review_contribution = ops$ratio_code_review_contribution_sd,
      issue_reports_discussion = ops$ratio_issue_reports_discussion_sd,
      technical_discussion = ops$ratio_technical_discussion_sd
    )
  )
  
  return (ops.a_focus.r_sd)
}

#' separates activity level ratios from op df
#'
#'
get.a_level <- function(ops){
  ops.a_level <- as.data.frame(cbind(persistency = ops$persistency,
                                     persistency_sd = ops$persistency_sd,
                                     extent= ops$contribution_extent,
                                     extent_sd = ops$contribution_extent_sd))
  
  return(ops.a_level)
}

#' separates experience measures from op df
#'
#'
get.experience <- function(ops){
  ops.experience <- as.data.frame(cbind(proj_experience = ops$proj_experience,
                                    proj_experience_sd = ops$proj_experience_sd))
}


#' separates reputation measures from op df
#'
#'
get.reputation <- function(ops){
  ops.reputation <- data.frame(degree_centrality = ops$degree_centrality,
                               betweenness_centrality = ops$betweenness_centrality,
                               closeness_centrality = ops$closeness_centrality,
                               degree_prestige = ops$degree_prestige,
                               proximity_prestige = ops$proximity_prestige)
  return(ops.reputation)
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
ops.boxplot.a_focus.simple <- function(ops.ratios.simple) {
  
  vec <- ops.vectorize(ops.ratios.simple)
  
  no.obs.1 <- nrow(vec[vec$cat == 1,])
  no.obs.2 <- nrow(vec[vec$cat == 2,])
  no.obs.3 <- nrow(vec[vec$cat == 3,])
  no.obs.4 <- nrow(vec[vec$cat == 4,])
  
  
  p <- ggplot(vec, aes( x = cat, y = val)) + 
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab("ratios") +
    ylab("ratio value") +
    labs(title = "Distribution of simple activity focus ratios",
         subtitle = paste("No. observations:", no.obs.1, no.obs.2, no.obs.3, no.obs.4, sep = " ")) +
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
ops.boxplot.a_focus.sd <- function(ops.a_focus.sd){
  
  vec <- ops.vectorize(ops.a_focus.sd)
  
  no.obs.1 <- nrow(vec[vec$cat == 1,])
  no.obs.2 <- nrow(vec[vec$cat == 2,])
  no.obs.3 <- nrow(vec[vec$cat == 3,])
  no.obs.4 <- nrow(vec[vec$cat == 4,])
  
  p <- ggplot(vec, aes( x = cat, y = val )) + 
    #geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab("ratios") +
    ylab("ratio value") +
    labs(title = "Distribution of standardized activity focus ratios", 
         subtitle = paste("No. observations:", no.obs.1, no.obs.2, no.obs.3, no.obs.4, sep = " ")) +
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
  p1 <- ggplot(ops.a_level, aes( x = "1", y = extent )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE)+
    #geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab(" ") +
    ylab("share of individual contributions in total project contributions") +
    scale_x_discrete(labels=c("1" = "activity extent\nsimple ratio"))
  
    
  # plot standardized relative extent
  p2 <- ggplot(ops.a_level, aes( x = "1", y = extent_sd )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE)+
    #geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab(" ") +
    ylab("deviation from project mean in sd") +
    scale_x_discrete(labels=c("1" = "activity extent\nstandardized deviation from mean"))
  
  # grid.arrange(p1, p2, p3, p4, ncol = 4, top = "Distribution of activity extent")
  grid.arrange(p1, p2, ncol = 2, top = "Distribution of activity extent")
  
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
  
  p1 <- ggplot(ops.a_level, aes( x = "1", y = ops.a_level$persistency)) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE)+
    #geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab("") +
    ylab("share of active periods in periods since first contribution") +
    scale_x_discrete(labels=c( "1" = "activity persistency\nsimple ratio"))
      

  p3 <- ggplot(ops.a_level, aes( x = "1", y = ops.a_level$persistency_sd )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE)+
    #geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab(" ") +
    ylab("deviation from project mean in sd's") +
    scale_x_discrete(labels=c( "1" = "activity persistency\nstandardized deviation from mean"))
  
  # grid.arrange(p1, p2, p3, ncol = 3, top = "Distribution of activity persistency")
  grid.arrange(p1, p3, ncol = 2, 
               top = "Distribution of activity persistency")
  
  dev.off()
}

#'
#'
#'
ops.boxplot.reputation <- function(ops.reputation){

  png(
    filename = paste(param.plot.exp, "boxplot_reputation.png", sep = ""),
    res = param.plot.res,
    width = param.plot.width,
    height = param.plot.height,
    units = param.plot.units
  )
  
  p1 <- ggplot(ops.reputation, aes( x = "1" , y = ops.reputation$degree_centrality )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE) +
    # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab(" ") +
    ylab(" ") +
    scale_x_discrete(labels=c("1" = "degree centrality\nnorm. by g-1"))
  
  p2 <- ggplot(ops.reputation, aes( x = "1" , y = ops.reputation$betweenness_centrality )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE) +
    # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab(" ") +
    ylab(" ") +
    scale_x_discrete(labels=c("1" = "betweenness centrality\nnorm. by g-1"))

  
  p3 <- ggplot(ops.reputation, aes( x = "1" , y = ops.reputation$closeness_centrality )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE) +
    # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab(" ") +
    ylab(" ") +
    scale_x_discrete(labels=c("1" = "closeness centrality\nnorm. by g-1"))
  
  
  p4 <- ggplot(ops.reputation, aes( x = "1" , y = ops.reputation$proximity_prestige )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE)+
    # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab(" ") +
    ylab(" ") +
    scale_x_discrete(labels=c("1" = "proximity prestige\n "))
  
  
  grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2, top = "Distribution of reputation measures")
  
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
ops.boxplot.a_focus.simple(ops.all$a_focus.simple)
ops.boxplot.a_focus.sd(ops.all$a_focus.sd)

ops.boxplot.a_level.extent(ops.all$a_level)
ops.boxplot.a_level.persistency(ops.all$a_level)

ops.boxplot.reputation(ops.all$reputation)

# test variances
ops.test.var.print(ops.test.var(ops.all$a_focus.simple))
ops.test.var.print(ops.test.var(ops.all$a_focus.sd))
