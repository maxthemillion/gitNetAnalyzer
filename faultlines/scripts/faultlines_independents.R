# script analyzes variable operationalizations

#### libraries ####
# library(multcomp)
library(data.table)
library(EnvStats)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(plyr)
#library(GGally)

#### parameters ####

# define which dataset to use. can be "sp180_c10", "sp180_c20", "sp180_c50"or "sp90_c20" 
param.dataset = "sp180_c20" 

# which parts of the code should be executed?
param.plot.ops = T                 # should operationalisations be plotted?
param.independents.generate = F    # should independents be generated?
param.plot.ind = F                 # should independents be plotted?

# set plotting parameters
param.plot.res = 300      # plot resolution in ppi
param.plot.width = 18     
param.plot.height = 12
param.plot.units = "cm"

# custom ggplot layout changes
old <- theme_set(theme_gray())
theme_update(axis.title = element_text(size = rel(0.65)))
theme_update(axis.text = element_text(size = rel(0.5)))

# file paths
# to set up the script in a different environment, set the root parameter
param.path.root = "/Users/Max/Desktop/MA/R/NetworkAnalyzer/faultlines/"
param.plot.out =  paste(param.path.root, "analysis/", param.dataset, "/variation/", sep = "")
param.ops.in = paste(param.path.root, "data/variation/", param.dataset, "/ops_all.csv", sep = "")
param.independents.out = paste(param.path.root, "data/models/", param.dataset, "/", sep = "")

# thresholds for defining group membership
param.threshold.code_issue_sd = 0
param.threshold.code_review_contribution_sd = 0
param.threshold.issue_reports_discussion_sd = 0
param.threshold.technical_discussion_sd = 0 
param.threshold.persistency_sd = 0
param.threshold.extent_sd = 0
param.threshold.proximity_prestige_sd = 0
param.threshold.experience_sd = 0


#### import ####
#'import ops file and convert to numeric/factor
coerce_new <- function(){
  ops <- read.csv(param.ops.in)
  
  return(ops)
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


#### plots ####
#' saves the supplied ggplot under the specified name
#' @param plot  ggplot2 object
#' @param name  filename to save the plot to
save.plot <- function(plot, name){
  png(
    filename = paste(param.plot.out, name, sep = ""),
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
ops.boxplot.a_focus.simple <- function(ops) {
  
  data = ops[, c("ratio_code_issue",
                 "ratio_code_review_contribution",
                 "ratio_issue_reports_discussion",
                 "ratio_technical_discussion")]
  
  vec <- melt(data)
  
  no.obs.1 <- nrow(vec[(vec$variable == "ratio_code_issue" & !is.na(vec$value)),])
  no.obs.2 <- nrow(vec[(vec$variable == "ratio_code_review_contribution" & !is.na(vec$value)),])
  no.obs.3 <- nrow(vec[(vec$variable == "ratio_issue_reports_discussion" & !is.na(vec$value)),])
  no.obs.4 <- nrow(vec[(vec$variable == "ratio_technical_discussion" & !is.na(vec$value)),])
  
  
  p <- ggplot(vec, aes( x = variable, y = value)) + 
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab("ratios") +
    ylab("deviation from project mean in standard deviations") +
    labs(title = "Distribution of simple activity focus ratios",
         subtitle = paste("No. observations:", no.obs.1, no.obs.2, no.obs.3, no.obs.4, sep = " ")) +
    theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1)) +
    scale_x_discrete(labels=c(
      "ratio_code_issue" = "code v issue \n activity", 
      "ratio_code_review_contribution" = "code reviewing v\n contributing",
      "ratio_issue_reports_discussion" = "issue reporting v\n discussing",
      "ratio_technical_discussion" = "tech. contributing v\n discussing")) 
  
  save.plot(p, "boxplot_ops_ratios_simple.png")
}

#' creates a boxplot for the relative activity focus ratios
#' if parameter param.plot.beanplot is set to TRUE, boxplot is generated as beanplot
#' @param ops.ratios.rel
ops.boxplot.a_focus.sd <- function(ops){
  
  data = ops[, c("ratio_code_issue_sd",
                 "ratio_code_review_contribution_sd",
                 "ratio_issue_reports_discussion_sd",
                 "ratio_technical_discussion_sd")]
  
  vec <- melt(data)
  
  no.obs.1 <- nrow(vec[(vec$variable == "ratio_code_issue_sd" & !is.na(vec$value)),])
  no.obs.2 <- nrow(vec[(vec$variable == "ratio_code_review_contribution_sd" & !is.na(vec$value)),])
  no.obs.3 <- nrow(vec[(vec$variable == "ratio_issue_reports_discussion_sd" & !is.na(vec$value)),])
  no.obs.4 <- nrow(vec[(vec$variable == "ratio_technical_discussion_sd" & !is.na(vec$value)),])
  
  p <- ggplot(vec, aes( x = variable, y = value )) + 
    #geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab("ratios") +
    ylab("ratio value") +
    labs(title = "Distribution of standardized activity focus ratios", 
         subtitle = paste("No. observations:", no.obs.1, no.obs.2, no.obs.3, no.obs.4, sep = " ")) +
    theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1)) +
    scale_x_discrete(labels=c(
      "ratio_code_issue_sd" = "code v issue \n activity", 
      "ratio_code_review_contribution_sd" = "code reviewing v\n contributing",
      "ratio_issue_reports_discussion_sd" = "issue reporting v\n discussing",
      "ratio_technical_discussion_sd" = "tech. contributing v\n discussing")) 
  
  save.plot(p, "boxplot_ops_ratios_rel_sd.png")
}

#' creates a boxplot for all standardized faultline items
#' if parameter param.plot.beanplot is set to TRUE, boxplot is generated as beanplot
#' @param ops.ratios.rel
ops.boxplot.sd <- function(ops){
  
  vec <- melt(ops[, c("ratio_code_issue_sd",
                 "ratio_code_review_contribution_sd",
                 "ratio_issue_reports_discussion_sd",
                 "ratio_technical_discussion_sd",
                 "contribution_extent_sd",
                 "persistency_sd",
                 "proximity_prestige_sd",
                 "proj_experience_sd"
  )])
  
  p <- ggplot(vec, aes( x = variable, y = value )) + 
    #geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab("ratios") +
    ylab("ratio value") +
    labs(title = "Distribution of standardized faultline variables") +
    theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1)) +
    scale_x_discrete(labels=c(
      "ratio_code_issue_sd" = "code v issue \n activity", 
      "ratio_code_review_contribution_sd" = "code reviewing v\n contributing",
      "ratio_issue_reports_discussion_sd" = "issue reporting v\n discussing",
      "ratio_technical_discussion_sd" = "tech. contributing v\n discussing",
      "contribution_extent_sd" = "contribution extent",
      "persistency_sd" = "contribution persistency",
      "proximity_prestige_sd" = "reputation",
      "proj_experience_sd" = "project experience"
    ))
  
  save.plot(p, "boxplot_ops_sd.png")
}

#' creates a boxplot for the activity level variables each
#'
#'
ops.boxplot.simple <- function(ops){
  
  png(
    filename = paste(param.plot.out, "boxplot_standard.png", sep = ""),
    res = param.plot.res,
    width = param.plot.width,
    height = param.plot.height,
    units = param.plot.units
  )
  
  vec <- melt(ops[, c("ratio_code_issue",
                      "ratio_code_review_contribution",
                      "ratio_issue_reports_discussion",
                      "ratio_technical_discussion")])
  
  p1 <- ggplot(vec, aes( x = variable, y = value)) + 
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab(" ") +
    ylab(" ") +
    theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1)) +
    scale_x_discrete(labels=c(
      "ratio_code_issue" = "code v issue \n activity", 
      "ratio_code_review_contribution" = "code reviewing\nv contributing",
      "ratio_issue_reports_discussion" = "issue reporting\nv discussing",
      "ratio_technical_discussion" = "tech. contributing\nv discussing"
      )) 
  
  vec <- melt(ops[, c("contribution_extent",
                      "persistency",
                      "proj_experience")])
  
  p2 <- ggplot(vec, aes( x = variable, y = value)) + 
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab(" ") +
    ylab(" ") +
    theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1)) +
    scale_x_discrete(labels=c(
      "contribution_extent" = "contrib. extent",
      "persistency" = "contrib. persistency",
      "proj_experience" = "project experience"
    )) 
  
  p3 <- ggplot(ops, aes( x = "1" , y = proximity_prestige )) + 
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab(" ") +
    ylab(" ") +
    theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1)) +
    scale_x_discrete(labels=c("1" = "proximity prestige "))
  
  grid.arrange(p1, p2, p3, top = "Distribution of faultline operationalizations", widths = c(5, 4, 2), heights = c(1))
  
  dev.off()
}

#' creates a boxplot for the activity level variables each
#'
#'
ops.boxplot.a_level.extent <- function(ops){
  
  png(
    filename = paste(param.plot.out, "boxplot_activity_extent.png", sep = ""),
    res = param.plot.res,
    width = param.plot.width,
    height = param.plot.height,
    units = param.plot.units
  )
  
  # plot simple extent
  p1 <- ggplot(ops, aes( x = "1", y = contribution_extent )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE)+
    #geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab(" ") +
    ylab("share of individual contributions in total project contributions") +
    scale_x_discrete(labels=c("1" = "activity extent\nsimple ratio"))
  
  
  # plot standardized relative extent
  p2 <- ggplot(ops, aes( x = "1", y = contribution_extent_sd )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE)+
    #geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab(" ") +
    ylab("deviation from project mean in sd") +
    scale_x_discrete(labels=c("1" = "activity extent\nstandardized deviation from mean"))
  
  grid.arrange(p1, p2, ncol = 2, top = "Distribution of activity extent")
  
  dev.off()
}

#' creats a boxplot for activity persistency
#'
#'
ops.boxplot.a_level.persistency <- function(ops){
  
  png(
    filename = paste(param.plot.out, "boxplot_activity_persistency.png", sep = ""),
    res = param.plot.res,
    width = param.plot.width,
    height = param.plot.height,
    units = param.plot.units
  )
  
  p1 <- ggplot(ops, aes( x = "1", y = persistency)) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE)+
    #geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab("") +
    ylab("share of active periods in periods since first contribution") +
    scale_x_discrete(labels=c( "1" = "activity persistency\nsimple ratio"))
  
  
  p3 <- ggplot(ops, aes( x = "1", y = persistency_sd )) + 
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
ops.boxplot.reputation <- function(ops){
  
  png(
    filename = paste(param.plot.out, "boxplot_reputation.png", sep = ""),
    res = param.plot.res,
    width = param.plot.width,
    height = param.plot.height,
    units = param.plot.units
  )
  
  p1 <- ggplot(ops, aes( x = "1" , y = degree_centrality )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE) +
    # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab(" ") +
    ylab(" ") +
    scale_x_discrete(labels=c("1" = "degree centrality\nnorm. by g-1"))
  
  p2 <- ggplot(ops, aes( x = "1" , y = betweenness_centrality )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE) +
    # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab(" ") +
    ylab(" ") +
    scale_x_discrete(labels=c("1" = "betweenness centrality\nnorm. by g-1"))
  
  
  p3 <- ggplot(ops, aes( x = "1" , y = closeness_centrality )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE) +
    # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab(" ") +
    ylab(" ") +
    scale_x_discrete(labels=c("1" = "closeness centrality\nnorm. by g-1"))
  
  
  p4 <- ggplot(ops, aes( x = "1" , y = proximity_prestige )) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE)+
    # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab(" ") +
    ylab(" ") +
    scale_x_discrete(labels=c("1" = "proximity prestige\n "))
  
  
  grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2, top = "Distribution of reputation measures")
  
  dev.off()
  
}

#' creates a pairplot for the simple activity ratios
ops.pairplot.simple <- function(ops) {
  data = ops[, c("ratio_code_issue",
                 "ratio_code_review_contribution",
                 "ratio_issue_reports_discussion",
                 "ratio_technical_discussion")]
  
  png(
    filename = paste(param.plot.out, "pairplot_ops_simple.png", sep = ""),
    res = param.plot.res,
    width = param.plot.width,
    height = param.plot.height,
    units = param.plot.units
  )
  
  pairs(data)
  
  dev.off()
}

#'
#'
#'
ops.scatterplot.a_level.sd <- function(ops){
  
  p <- ggplot(ops, aes(x = persistency_sd, y = extent_sd)) +
    geom_point(na.rm = TRUE, alpha = 1/20, size = 1) +
    labs(title="Activity level dimensions",
         subtitle = "deviation from mean in sd") +
    ylab("contribution extent") +
    xlab("contribution persistency") +
    geom_hline(aes(yintercept=0), colour="grey", linetype="dashed") +
    geom_vline(aes(xintercept=0), colour="grey", linetype="dashed") +
    ylim(-5, 30)+
    xlim(-3, 6)
  
  save.plot(p, 'scatterplot_a_level.png')
}

#'
#'
#'
projects.boxplot.a_level.sd <- function(ops){
  
  sample.size = 50
  pool = unique(ops$project)
  
  data.sample = sample(pool, sample.size)
  
  plot.data = ops[ops$project %in% data.sample,]
  
  # ops.a_level.sd$project <- as.factor(ops.a_level.sd$project)
  
  p <- ggplot(plot.data, aes(x = project, y = persistency_sd)) +
    geom_hline(aes(yintercept= -1), colour="grey", linetype="dashed") +
    geom_hline(aes(yintercept= 0), colour="black") +
    geom_hline(aes(yintercept= 1), colour="grey", linetype="dashed") +
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=0.2, notch=FALSE) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black")) +
    labs(title = "Distribution of activity persistency per project",
         subtitle = paste("Sample of ", sample.size, " projects")) +
    ylab("deviation from mean in standard deviations") +
    xlab("projects ")
    # + stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=1,show_guide = FALSE)
  
  save.plot(p, "projects_persistency_sd.png")
}
projects.boxplot.a_focus.sd <- function(ops){
  sample.size = 50
  pool = unique(ops$project)
  
  data.sample = sample(pool, sample.size)
  
  plot.data = ops[ops$project %in% data.sample,]
  
  # ops.a_focus.sd$project <- as.factor(ops.a_focus.sd$project)
  
  p <- ggplot(plot.data, aes(x = project, y = ratio_code_issue_sd)) +
    geom_hline(aes(yintercept= -1), colour="grey", linetype="dashed") +
    geom_hline(aes(yintercept= 0), colour="black") +
    geom_hline(aes(yintercept= 1), colour="grey", linetype="dashed") +
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=0.2, notch=FALSE) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black")) +
    labs(title = "Distribution of code vs issue ratio per project",
         subtitle = paste("Sample of ", sample.size, " projects")) +
    ylab("deviation from mean in standard deviations") +
    xlab("projects ") +
    # stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=1,show_guide = FALSE)
  
  save.plot(p, "projects_code_issue_sd.png")
}
projects.boxplot.reputation.sd <- function(ops){
  sample.size = 50
  pool = unique(ops$project)
  
  data.sample = sample(pool, sample.size)
  
  plot.data = ops[ops$project %in% data.sample,]
  
  #ops.reputation$project <- as.factor(ops.reputation$project)
  
  p <- ggplot(plot.data, aes(x = project, y = proximity_prestige_sd)) +
    geom_hline(aes(yintercept= -1), colour="grey", linetype="dashed") +
    geom_hline(aes(yintercept= 0), colour="black") +
    geom_hline(aes(yintercept= 1), colour="grey", linetype="dashed") +
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=0.2, notch=FALSE) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black")) +
    labs(title = "Distribution of code vs issue ratio per project",
         subtitle = paste("Sample of ", sample.size, " projects")) +
    ylab("deviation from mean in standard deviations") +
    xlab("projects ") +
    stat_summary(fun.y=mean, colour="darkred", geom="point", 
                 shape=18, size=1,show_guide = FALSE)
  
  save.plot(p, "projects_reputation_sd.png")
}

#### Chi-square tests ####
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

# plot independents
#' plots pairplot for all independent variables
#'
#'
ind.pairs <- function(independents){
  
  png(
    filename = paste(param.plot.out, "pairs_independents.png", sep = ""),
    res = param.plot.res,
    width = param.plot.width,
    height = param.plot.height,
    units = param.plot.units
  )
  
  
  g <- ggpairs(independents, columns=c('issue_focus', 
                                       'code_comment_focus', 
                                       'issue_comment_focus',
                                       'techcontrib_focus'),
               title = "Correlation of activity focus ratios",
               columnLabels = c("code/issue", "c. review/contrib.", "i. reports/discuss.", "contrib./discuss."),
               lower = list(
                 continuous = wrap("smooth", alpha = 0.2, color = "blue") 
               ),
               upper = list(continuous = wrap("cor", size = 2))
  )
  
  
  g <- g + theme(
    axis.text = element_text(size = 4),
    axis.title = element_text(size = 4),
    legend.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "grey95")
  )
  
  print(g)
  
  dev.off()
  
}

#' plots boxplot for all independents
#'
#'
ind.boxplot <- function(independents){
  
  # vec <- ops.vectorize(independents)
  
  vec <- melt(independents)
  
  p <- ggplot(vec, aes(y = value, x = variable)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    ylab("share of subgroup members in project members") +
    xlab("subgroups") +
    labs(title = "Distribution of subgroup member shares",
         subtitle = paste("Number of projects: ", nrow(independents))) +
    theme(panel.grid.major.x = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black")) +
    scale_x_discrete(labels=c(
      "rel_persistent" = paste("rel. persistent\ncut: ", param.threshold.persistency_sd),
      "rel_extensive" = paste("rel. extensive\ncut: ", param.threshold.extent_sd),
      "issue_focus" = paste("rel. much issue activity\ncut: ", param.threshold.code_issue_sd),
      "code_comment_focus" = paste("rel. much code comments\ncut: ", param.threshold.code_review_contribution_sd),
      "issue_comment_focus" = paste("rel. much issue comments\ncut: ", param.threshold.issue_reports_discussion_sd),
      "techcontrib_focus" = paste("rel. much code discussion\ncut :", param.threshold.code_review_contribution_sd),
      "rel_high_reputation" = paste("rel. much project reputation\ncut: ", param.threshold.proximity_prestige_sd),
      "rel_experienced" = paste("rel. experienced\ncut: ", param.threshold.experience_sd)
    )) +
    theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))
  
  save.plot(p, "boxplot_subgroup_member_share.png")
}

#### main part ####

#' calculates the relative sizes of subgroups
#' @param var vector of variable to calculate shares of
#' @param project vector of projects
#' @param varname name of the resulting variable
#' @param threshold threshold value where var will be split
#' @param blau logical, if T also calculate Blau's index
#'
calculate_share <- function(var, project, varname, threshold, blau = F){
  
  temp = data.frame(var = var, project = project)
  temp$L = temp$var > threshold
  res = (aggregate(temp$L, by=list(Category=temp$project), FUN=sum, na.rm = T))
  res$x = res$x/plyr::count(temp, "project")$freq
  res <- plyr::rename(res, c("x" = varname, "Category" = "project"))
  
  if(blau){
    
    varname_new = paste(varname, "blau", sep = "_")
    
    # divide by 0.5 to ensure index ranges from 0 to 1 (with two categories, 0.5 is max)
    res[, varname_new] = ((1 - (res[, varname]^2 + (1-res[, varname])^2))/0.5) 
    
  }
  
  return(res)
}


#' 
#'
#'
get.independents.new <- function(ops){
  independents = data.frame(project = unique(ops$project)) 
  
  # activity persistency
  ap = calculate_share(ops$persistency_sd, 
                       ops$project, 
                       "rel_persistent", 
                       param.threshold.persistency_sd,
                       blau = T)
  independents <- merge(x = independents, y = ap, by = "project", all.x = T)
  
  # activity extent
  ae = calculate_share(ops$contribution_extent_sd,
                       ops$project,
                       "rel_extensive",
                       param.threshold.extent_sd,
                       blau = T)
  independents <- merge(x = independents, y = ae, by = "project", all.x = T)
  
  # activity focus
  # 1
  af.1 = calculate_share(ops$ratio_code_issue_sd,
                         ops$project,
                         "issue_focus",
                         param.threshold.code_issue_sd,
                         blau = T)
  independents <- merge(x = independents, y = af.1, by = "project", all.x = T)
  
  #2
  af.2 = calculate_share(ops$ratio_code_review_contribution_sd,
                         ops$project,
                         "code_comment_focus",
                         param.threshold.code_review_contribution_sd,
                         blau = T)
  independents <- merge(x = independents, y = af.2, by = "project", all.x = T)
  
  #3
  af.3 = calculate_share(ops$ratio_issue_reports_discussion_sd,
                         ops$project,
                         "issue_comment_focus",
                         param.threshold.issue_reports_discussion_sd,
                         blau = T)
  independents <- merge(x = independents, y = af.3, by = "project", all.x = T)
  
  #4
  af.4 = calculate_share(ops$ratio_technical_discussion_sd,
                         ops$project,
                         "techcontrib_focus",
                         param.threshold.technical_discussion_sd,
                         blau = T)
  independents <- merge(x = independents, y = af.4, by = "project", all.x = T)
  
  # reputation
  rp = calculate_share(ops$proximity_prestige_sd,
                       ops$project,
                       "rel_high_reputation",
                       param.threshold.proximity_prestige_sd)
  independents <- merge(x = independents, y = rp, by = "project", all.x = T)
  
  # project experience
  pe <- calculate_share(ops$proj_experience_sd,
                        ops$project,
                        "rel_experienced",
                        param.threshold.experience_sd,
                        blau = T)
  independents <- merge(x = independents, y = pe, by = "project", all.x = T)
  
  # no subgroups
  independents$no_subgroups = (aggregate(ops$no_subgroups, by=list(Category=ops$project), FUN=mean, na.rm = T))[,-1]
  
  # modularity
  independents$modularity = (aggregate(ops$modularity, by=list(Category=ops$project), FUN=mean, na.rm = T))[,-1]
  
  
  return(independents)
}

# put all projects together
ops  = coerce_new()

# generate some plots
if(param.plot.ops) {
  if (T) {
    # ops.boxplot.a_focus.simple(ops)
    # ops.boxplot.a_focus.sd(ops)
    ops.boxplot.sd(ops)
    ops.boxplot.simple(ops)
    
    # ops.boxplot.a_level.extent(ops)
    # ops.boxplot.a_level.persistency(ops)
    
    # ops.boxplot.reputation(ops)
  }
  
  if (F) {
    projects.boxplot.a_level.sd(ops)
    projects.boxplot.a_focus.sd(ops)
    projects.boxplot.reputation.sd(ops)
  }
}

# test variances
# ops.test.var.print(ops.test.var(ops.all$a_focus.simple))
# ops.test.var.print(ops.test.var(ops.all$a_focus.sd))

if(param.independents.generate){
  # get all independents
  independents <- get.independents.new(ops)
  
  # save independents to csv
  file.path = paste(param.independents.out, "independents.csv", sep = "")
  write.csv(independents, file = file.path, row.names = F)
  
  
  if(param.plot.ind){
    ind.pairs(independents)
    ind.boxplot(independents)
  }
}
