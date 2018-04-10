# script analyzes variable operationalizations
#
# .libPaths(c(.libPaths(), '/home/rahnm/R/lib'))

# library(multcomp)
library(futile.logger)
library(data.table)
library(EnvStats)
library(ggplot2)
library(gridExtra)
library(plyr)
library(GGally)

# parameters:
param.analysis.sample = F
param.analysis.sample.size = 100

param.plot.ops = T
param.plot.ind = T
param.plot.res = 300
param.plot.exp =  "/Users/Max/Desktop/MA/R/NetworkAnalyzer/faultlines/plots/"
param.plot.width = 12
param.plot.height = 12
param.plot.units = "cm"

param.ops.import = "/Users/Max/Desktop/MA/R/NetworkAnalyzer/faultlines/analysis/faultlines/variation"

param.threshold.code_issue_sd = 0
param.threshold.code_review_contribution_sd = 0
param.threshold.issue_reports_discussion_sd = 0
param.threshold.technical_discussion_sd = 0 

param.threshold.persistency_sd = 0
param.threshold.extent_sd = 0
param.threshold.proximity_prestige = 0
param.threshold.experience_sd = 0


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
  l.project = list()
  
  i = 1
  for (file in file.list) {
    ops <- read.csv(file)
    
    l.a_focus.simple[[i]] = get.a_focus.simple(ops, i)
    l.a_focus.sd[[i]] = get.a_focus.sd(ops, i)
    l.experience[[i]] = get.experience(ops, i) 
    l.a_level[[i]] = get.a_level(ops, i)
    l.reputation[[i]] = get.reputation(ops, i)

    i = i + 1
  }
  
  ops.all = list()
  
  ops.all$a_focus.simple = rbindlist(l.a_focus.simple)
  ops.all$a_focus.sd = rbindlist(l.a_focus.sd)
  ops.all$a_level = rbindlist(l.a_level)
  ops.all$experience = rbindlist(l.experience)
  ops.all$reputation = rbindlist(l.reputation)
  
  ops.all$a_level$project = as.factor(ops.all$a_level$project)
  ops.all$a_focus.sd$project = as.factor(ops.all$a_focus.sd$project)
  ops.all$experience$project = as.factor(ops.all$experience$project)
  ops.all$reputation$project = as.factor(ops.all$reputation$project)
  
  return(ops.all)
}

#'import ops file and convert to numeric/factor
coerce_new <- function(){
  file = "/Users/Max/Desktop/MA/R/NetworkAnalyzer/faultlines/analysis/faultlines/variation/ops_all.csv"
  ops <- read.csv(file)
  
  ops$project <- as.factor(ops$project)
  
  ops.all = list()
  
  ops.all$a_focus.simple = get.a_focus.simple(ops)
  ops.all$a_focus.sd = get.a_focus.sd(ops)
  ops.all$a_level = get.a_level(ops)
  ops.all$experience = get.experience(ops)
  ops.all$reputation = get.reputation(ops)
  
  return(ops.all)
}



#' separates simple ratios from ops data frame
#' @param ops: operationalizations data frame
#' @return data.frame containing simple ops ratios
get.a_focus.simple <- function(ops, i = NULL) {
  ops.a_focus.simple <- data.frame(
      code_issue = ops$ratio_code_issue,
      code_review_contribution = ops$ratio_code_review_contribution,
      issue_reports_discussion = ops$ratio_issue_reports_discussion,
      technical_discussion = ops$ratio_technical_discussion,
      project = ops$project
    )
  return (ops.a_focus.simple)
}

#' separates standardized relative activity focus ratios from op df
#'
#'
get.a_focus.sd <- function(ops, i = NULL) {
  ops.a_focus.sd <- data.frame(
      code_issue = ops$ratio_code_issue_sd,
      code_review_contribution = ops$ratio_code_review_contribution_sd,
      issue_reports_discussion = ops$ratio_issue_reports_discussion_sd,
      technical_discussion = ops$ratio_technical_discussion_sd,
      project = ops$project
  )
  
  return (ops.a_focus.sd)
}

#' separates activity level ratios from op df
#'
#'
get.a_level <- function(ops, i = NULL){
  ops.a_level <- data.frame(persistency = ops$persistency,
                                     persistency_sd = ops$persistency_sd,
                                     extent= ops$contribution_extent,
                                     extent_sd = ops$contribution_extent_sd,
                                     project = ops$project
  )
  
  return(ops.a_level)
}

#' separates experience measures from op df
#'
#'
get.experience <- function(ops, i = NULL){
  ops.experience <- data.frame(proj_experience = ops$proj_experience,
                                    proj_experience_sd = ops$proj_experience_sd,
                                    project = ops$project)
}


#' separates reputation measures from op df
#'
#'
get.reputation <- function(ops, i = NULL ){
  ops.reputation <- data.frame(degree_centrality = ops$degree_centrality,
                               betweenness_centrality = ops$betweenness_centrality,
                               closeness_centrality = ops$closeness_centrality,
                               degree_prestige = ops$degree_prestige,
                               proximity_prestige = ops$proximity_prestige,
                               project = ops$project
  )
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
  
  vec <- ops.vectorize(ops.ratios.simple[,1:4])
  
  no.obs.1 <- nrow(vec[vec$cat == 1,])
  no.obs.2 <- nrow(vec[vec$cat == 2,])
  no.obs.3 <- nrow(vec[vec$cat == 3,])
  no.obs.4 <- nrow(vec[vec$cat == 4,])
  
  
  p <- ggplot(vec, aes( x = cat, y = val)) + 
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    xlab("ratios") +
    ylab("deviation from project mean in standard deviations") +
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
  
  vec <- ops.vectorize(ops.a_focus.sd[,1:4])
  
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

#'
#'
#'
ops.scatterplot.a_level.sd <- function(ops.a_level.sd){
  p <- ggplot(ops.a_level.sd, aes(x = persistency_sd, y = extent_sd)) +
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
projects.boxplot.a_level.sd <- function(ops.a_level.sd){
  sample.size = 50
  pool = unique(ops.a_level.sd$project)
  
  data.sample = sample(pool, sample.size)
  
  plot.data = ops.a_level.sd[ops.a_level.sd$project %in% data.sample,]
  
  ops.a_level.sd$project <- as.factor(ops.a_level.sd$project)
  
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
    xlab("projects ") +
    stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=1,show_guide = FALSE)

    save.plot(p, "projects_persistency_sd.png")
}
projects.boxplot.a_focus.sd <- function(ops.a_focus.sd){
  sample.size = 50
  pool = unique(ops.a_focus.sd$project)
  
  data.sample = sample(pool, sample.size)
  
  plot.data = ops.a_focus.sd[ops.a_focus.sd$project %in% data.sample,]
  
  ops.a_focus.sd$project <- as.factor(ops.a_focus.sd$project)
  
  p <- ggplot(plot.data, aes(x = project, y = code_issue)) +
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
  
  save.plot(p, "projects_code_issue_sd.png")
}
projects.boxplot.reputation.sd <- function(ops.reputation){
    sample.size = 50
    pool = unique(ops.reputation$project)
    
    data.sample = sample(pool, sample.size)
    
    plot.data = ops.reputation[ops.reputation$project %in% data.sample,]
    
    ops.reputation$project <- as.factor(ops.reputation$project)
    
    p <- ggplot(plot.data, aes(x = project, y = proximity_prestige)) +
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
    filename = paste(param.plot.exp, "pairs_independents.png", sep = ""),
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
      "rel_high_reputation" = paste("rel. much project reputation\ncut: ", param.threshold.proximity_prestige),
      "rel_experienced" = paste("rel. experienced\ncut: ", param.threshold.experience_sd)
    )) +
    theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))
  
  save.plot(p, "boxplot_subgroup_member_share.png")
}

### --- do the work

# put all projects together
ops.all = coerce_new()

# generate some plots
if(param.plot.ops) {
  if (T) {
    ops.boxplot.a_focus.simple(ops.all$a_focus.simple)
    ops.boxplot.a_focus.sd(ops.all$a_focus.sd)
    
    ops.boxplot.a_level.extent(ops.all$a_level)
    ops.boxplot.a_level.persistency(ops.all$a_level)
    
    ops.boxplot.reputation(ops.all$reputation)
  }
  
  if (T) {
    projects.boxplot.a_level.sd(ops.all$a_level)
    projects.boxplot.a_focus.sd(ops.all$a_focus.sd)
    projects.boxplot.reputation.sd(ops.all$reputation)
  }
}

# test variances
# ops.test.var.print(ops.test.var(ops.all$a_focus.simple))
# ops.test.var.print(ops.test.var(ops.all$a_focus.sd))

# generate variable matrix
get.independents <- function(ops.all){
  # activity persistency
  temp = data.frame(persistency_sd = ops.all$a_level$persistency_sd,
                    project = ops.all$a_level$project)
  temp$L = temp$persistency_sd > param.threshold.persistency_sd
  agg = aggregate(temp$L, by=list(Category=temp$project), FUN=sum, na.rm = T)
  ind  = data.frame(project = agg$Category,
                    rel_persistent = agg$x/count(temp, "project")$freq)
  
  # activity extent
  temp = data.frame(extent_sd = ops.all$a_level$extent_sd,
                    project = ops.all$a_level$project)
  temp$L = temp$extent_sd > param.threshold.extent_sd
  ind$rel_extensive  = (aggregate(temp$L, by=list(Category=temp$project), FUN=sum, na.rm = T)/count(temp, "project"))[,-1]
  
  # activity focus
  # 1. 
  temp = data.frame(code_issue_sd = ops.all$a_focus.sd$code_issue,
                    project = ops.all$a_focus.sd$project)
  temp$L = temp$code_issue_sd > param.threshold.code_issue_sd
  ind$issue_focus  = (aggregate(temp$L, by=list(Category=temp$project), FUN=sum, na.rm = T)/count(temp, "project"))[,-1]
  
  
  # 2. 
  temp = data.frame(code_review_contribution_sd = ops.all$a_focus.sd$code_review_contribution,
                    project = ops.all$a_focus.sd$project)
  temp$L = temp$code_review_contribution_sd > param.threshold.code_review_contribution_sd
  ind$code_comment_focus  = (aggregate(temp$L, by=list(Category=temp$project), FUN=sum, na.rm = T)/count(temp, "project"))[,-1]
  
  
  # 3. 
  temp = data.frame(issue_reports_discussion_sd = ops.all$a_focus.sd$issue_reports_discussion,
                    project = ops.all$a_focus.sd$project)
  temp$L = temp$issue_reports_discussion_sd > param.threshold.issue_reports_discussion_sd
  ind$issue_comment_focus  = (aggregate(temp$L, by=list(Category=temp$project), FUN=sum, na.rm = T)/count(temp, "project"))[,-1]
  
  # 4. 
  temp = data.frame(technical_discussion_sd = ops.all$a_focus.sd$technical_discussion,
                    project = ops.all$a_focus.sd$project)
  temp$L = temp$technical_discussion_sd > param.threshold.technical_discussion_sd
  ind$techcontrib_focus  = (aggregate(temp$L, by=list(Category=temp$project), FUN=sum, na.rm = T)/count(temp, "project"))[,-1]
  
  # reputation
  # 1. proximity prestige 
  temp = data.frame(proximity_prestige = ops.all$reputation$proximity_prestige,
                    project = ops.all$reputation$project)
  temp$L = temp$proximity_prestige > param.threshold.proximity_prestige
  ind$rel_high_reputation  = (aggregate(temp$L, by=list(Category=temp$project), FUN=sum, na.rm = T)/count(temp, "project"))[,-1]
  # 2. ...
  
  # experience
  temp = data.frame(experience_sd = ops.all$experience$proj_experience_sd,
                    project = ops.all$experience$project)
  temp$L = temp$experience_sd > param.threshold.experience_sd
  ind$rel_experienced = (aggregate(temp$L, by=list(Category=temp$project), FUN=sum, na.rm = T)/count(temp, "project"))[,-1]
  
  # no subgroups
  
  
  return(ind)
}


# get all independents
independents <- get.independents(ops.all)

# save independents to csv
file.path = "/Users/Max/Desktop/MA/R/NetworkAnalyzer/faultlines/analysis/faultlines/model/independents.csv"
write.csv(independents, file = file.path, row.names = F)


if(param.plot.ind){
  ind.pairs(independents)
  ind.boxplot(independents)
}

