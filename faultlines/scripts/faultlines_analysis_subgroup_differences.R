library(futile.logger)
library(data.table)
library(plyr)
library(ggplot2)

param.analysis.all = FALSE
param.analysis.groups.size = 15
param.analysis.groups.includeResiduals = TRUE

param.plot.res = 300
param.plot.exp =  "/Users/Max/Desktop/MA/R/NetworkAnalyzer/faultlines/plots/"
param.plot.width = 12
param.plot.height = 12
param.plot.units = "cm"

param.ops.import = "/Users/Max/Desktop/MA/R/NetworkAnalyzer/faultlines/analysis/faultlines/variation"

#' import operationalization csv files and coerce them if more than one is being imported
#'
#' @param f:  project name
#' @return operationalizations data frame including group information
ops.import <- function(f) {
  file = paste(param.ops.import, "/op_df_", f, ".csv", sep = "")
  
  ops <- read.csv(file)

  return(ops)
}


#' selects groups according to project size
#'
#' @param n     scalar of type numeric. Determines how many groups should be selected
#' 
ops.groups.select <- function(ops, n = NULL, s = NULL){
  select_by_size = TRUE
  if (is.null(n) && is.null(s)){
    s = 20
    
  } else if (!is.null(n) && is.null(s)) {
    select_by_size = FALSE
  } 
  
  group.count = ddply(ops,"group", summarize, count=length(unique(gha_id)))
  
  group.count = group.count[order(-group.count$count),] 
  
  if (select_by_size){
    L = group.count$count >= s
    group.info = group.count$group[L]
  } else {
    group.info = head(group.count$group, n = n)    
  }
  
  if(param.analysis.groups.includeResiduals){
    L = ! ops$group %in% group.info
    ops$group[L] = -999
    group.info[length(group.info) + 1] = -999 
  }
  
  ops.L = ops$group %in% group.info
  ops.new = ops[ops.L,]
  
  return(ops.new)
}

#' plot group sizes
ops.groups.plot <- function (ops){
  
  
  group.count = ddply(ops,"group", summarize, count=length(unique(gha_id)))
  
  png(
    filename = paste(param.plot.exp, "boxplot_group_size.png", sep = ""),
    res = param.plot.res,
    width = param.plot.width,
    height = param.plot.height,
    units = param.plot.units
  )
  
  boxplot(
    group.count$count,
    xlab = "",
    ylab = "group size",
    main = "Distribution of group sizes"
    # names = c(),
  ) 
  
  dev.off()
}

ops.ratios.simple.boxplot <- function (ops){
  ops$group <- as.factor(ops$group)
  
  old <- theme_set(theme_gray())
  theme_update(axis.title = element_text(size = rel(0.65)))
  theme_update(axis.text = element_text(size = rel(0.5)))
  
  png(
    filename = paste(param.plot.exp, "boxplot_ops_ratios.png", sep = ""),
    res = param.plot.res,
    width = param.plot.width,
    height = param.plot.height,
    units = param.plot.units
  )
  
  p1 <- ggplot(ops, aes( x = group, y = ratio_code_issue )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE) +
    ylab("code vs issue activity") +
    ylim(0,1)
    
  
  p2 <- ggplot(ops, aes( x = group, y = ratio_code_review_contribution )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE) +
    ylab("code reviewing vs contributing") +
    ylim(0,1)
  
  p3 <- ggplot(ops, aes( x = group, y = ratio_issue_reports_discussion )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE) +
    ylab("issue reporting vs discussing") +
    ylim(0,1)
  
  
  p4 <- ggplot(ops, aes( x = group, y = ratio_technical_discussion )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE) +
    ylab("code contributions vs commenting") +
    ylim(0,1)
  
  
  grid.arrange(p1, p2, p3, p4, top = "Distribution of simple activity ratios")
  
  dev.off()
}

ops.ratios.rel.boxplot <-function (ops){
  ops$group <- as.factor(ops$group)
  
  old <- theme_set(theme_gray())
  theme_update(axis.title = element_text(size = rel(0.65)))
  theme_update(axis.text = element_text(size = rel(0.5)))
  
  png(
    filename = paste(param.plot.exp, "boxplot_ops_ratios_rel.png", sep = ""),
    res = param.plot.res,
    width = param.plot.width,
    height = param.plot.height,
    units = param.plot.units
  )
  
  p1 <- ggplot(ops, aes( x = group, y = ratio_rel_code_issue )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE) +
    ylab("code vs issue activity") +
    ylim(0, 1.5) +
    geom_hline(yintercept=1, linetype="dashed", color = "red")
  
  
  p2 <- ggplot(ops, aes( x = group, y = ratio_rel_code_review_contribution )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE) +
    ylab("code reviewing vs contributing") +
    ylim(0,2.5) +
    geom_hline(yintercept=1, linetype="dashed", color = "red")
  
  
  p3 <- ggplot(ops, aes( x = group, y = ratio_rel_issue_reports_discussion )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE) +
    ylab("issue reporting vs discussing") +
    ylim(0,1.5)+
    geom_hline(yintercept=1, linetype="dashed", color = "red")
  
  
  p4 <- ggplot(ops, aes( x = group, y = ratio_rel_technical_discussion )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE) +
    ylab("commenting vs contributing") +
    ylim(0,2.5) +    
    geom_hline(yintercept=1, linetype="dashed", color = "red")
  
  grid.arrange(p1, p2, p3, p4, top = "Distribution of relative activity ratios")
  
  dev.off()
}

#' performs an anova for the specified variable and tests anova assumptions
#'
#'   @param op      operationalization dataframe
#'   @param var     column name of the variable to analyze
#' 
perform_anova <- function(op, var) {
  # -- ANOVA --
  # http://www.sthda.com/english/wiki/one-way-anova-test-in-r#what-is-one-way-anova-test
  # estimate anova on various operationalizations
  res.aov <- aov(op[, var] ~ op$group)
  summary(res.aov)
  
  # since we do not know, which groups differ, calculate Tukey Honest Significant Differences
  TukeyHSD(res.aov)
  
  # further comparisons can be made with package 'multcomp'
  # https://cran.r-project.org/web/packages/multcomp/multcomp.pdf
  
  # check assumptions of ANOVA
  # 1. homogeieity of variance
  # plot should show no relationship between residuals and fitted values
  
  png(
    filename = paste(param.plot.exp, "plot_aov_homogeneity.png", sep = ""),
    res = param.plot.res,
    width = param.plot.width,
    height = param.plot.height,
    units = param.plot.units
  )
  plot(res.aov, 1)
  dev.off()
  
  # -- Levene test --
  # test must not to be signifiant in order to prove that variances in subgroups are homogenous
  # (p-value > 0.05)
  leveneTest(op[, var] ~ op$group)
  
  # 2. normality assumption
  png(
    filename = paste(param.plot.exp, "plot_aov_normality.png", sep = ""),
    res = param.plot.res,
    width = param.plot.width,
    height = param.plot.height,
    units = param.plot.units
  )
  plot(res.aov, 2)
  dev.off()
  
  # shapiro wilk test
  aov_residuals <- residuals(object = res.aov)
  shapiro.test(x = aov_residuals)
}

# performs a kruskal test for the specified variable
# parameters:
#     op      operationalization data frame
#     var     column name of the variable to analyze
# returns: -
perform_kruskal <- function (op, var) {
  # -- Kruskal Test --
  # to be used, when assumptions of one-way anova are not met
  # http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
  # estimate kruskal test on various operationalizations
  res.kruskal.c_i_ratio <- kruskal.test(op[, var] ~ op$group)
  #...
  
  # since we do not know, which groups differ, calculate pairwise wilcox test
  pairwise.wilcox.test(op[, var], op$group, p.adjust.method = "BH")
  #...
  
}


main <- function(){
  if(param.analysis.all) {
    
    
    
  } else {
    # import project
    ops <- ops.import('OneDrive')
    
    # plot group sizes
    ops.groups.plot(ops)
    
    # select groups
    #
    # ops <- ops.groups.select(ops, n = 2)
    ops <- ops.groups.select(ops, s = param.analysis.groups.size)
    
    ops.ratios.simple.boxplot(ops)
    ops.ratios.rel.boxplot(ops)
    }
  
  
}


main()
