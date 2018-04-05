library(futile.logger)
library(data.table)
library(plyr)
library(ggplot2)
library(gridExtra)

param.analysis.all = F                          # specifies, whether all or only a single project should be imported and analysed
param.analysis.single = 'waffleio'              # name of a project which should be imported, when param.analysis.all = F
param.analysis.groups.minsize = 0               # min group size which qualifies for further analysis
param.analysis.groups.includeResiduals = TRUE   # if T, all remaining groups which are smaller than the specified minsize will be aggregated and analyzed as well

param.plot.res = 300
param.plot.exp =  "/Users/Max/Desktop/MA/R/NetworkAnalyzer/faultlines/plots/"
param.plot.width = 12
param.plot.height = 12
param.plot.units = "cm"

param.ops.import = "/Users/Max/Desktop/MA/R/NetworkAnalyzer/faultlines/analysis/faultlines/variation"

#' import operationalization csv file
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
    
    #TODO
    
    
  } else {
    # import project
    ops <- ops.import(param.analysis.single)
    
    # select groups
    #
    # ops <- ops.groups.select(ops, n = 2)
    ops.sel <- ops.groups.select(ops, s = param.analysis.groups.minsize)
    
    ops.ratios.simple.boxplot(ops.sel)
    ops.ratios.rel.boxplot(ops.sel)
    }
}

main()
