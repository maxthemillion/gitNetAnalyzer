# performs an anova for the specified variable and tests anova assumptions
# parameters:
#   op      operationalization dataframe
#   var     column name of the variable to analyze
# returns: -
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