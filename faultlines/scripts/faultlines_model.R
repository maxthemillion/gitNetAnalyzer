#### libraries ####
library(ggplot2)
library(reshape2)
library(car)
library(MASS)
library(stargazer)
library(plyr)
library(pscl)

#### parameters ####
param.dataset = "sp180_c20" # can be "sp180_c10", "sp180_c20" or "sp90_c20" 

param.plot.facets = F
param.plot.correlation = T
param.plot.hist = F
param.models.estimate = F
param.glm.control = glm.control(epsilon = 1e-8, maxit = 100, trace = FALSE)

param.m.add_const_to_releases = F # adding a constant would violate distribution assumption, leave F
param.m.transform.blau = F
param.m.transform.controls = F

param.path.root = "/Users/Max/Desktop/MA/R/NetworkAnalyzer/faultlines/"
param.m.in = paste(param.path.root, "data/models/", param.dataset, "/", sep = "")
param.table.out =  paste(param.path.root, "/analysis/", param.dataset, "/models/tables/", sep = "")
param.plot.out = paste(param.path.root, "analysis/", param.dataset, "/models/plots/", sep = "")

param.plot.res = 300
param.plot.width = 12
param.plot.height = 12
param.plot.units = "cm"

old <- theme_set(theme_gray())
theme_update(axis.title = element_text(size = rel(0.65)))
theme_update(axis.text = element_text(size = rel(0.5)))

#### import ####

#' imports all variables (dependents and independents)
#' @return data frame containing all model variables
#'
import.variables <- function(){
  
  # read independent variables
  independents <- read.csv(paste(param.m.in, "independents.csv", sep = ""))
  
  # read dependent variables
  dependents <- read.csv(paste(param.m.in, "success.csv", sep = ""))
  
  ci <- read.csv(paste(param.path.root, "data/models/travis.csv", sep =""))[, c("project", "has_travis")]
  
  # merge
  res = merge(x = independents, y = dependents, by = "project")
  res = merge(x = res, y=ci, by = "project")
  
  res$has_travis = as.factor(res$has_travis)
  
  if(param.m.transform.blau){
    oss = transform.blau(oss)
  }
  
  if(param.m.transform.controls){
    oss = transform.controls(oss)
  }
  
  # calculate categorial variable for zero inflated NBR and PR
  oss$releases_control_cat = as.factor(oss$releases_control > 0)
  
  return(res)
}

#' separates dependent variables for visualization purposes
#' @param oss data frame containing all model variables
#' @return data frame containing dependent variables
#'
get_dependents <- function(oss){
  dependents = oss[, c("project", 
                       "ratio_core",
                       "no_core",
                       "no_non_core",
                       "releases")]
  return(dependents)
}

#' separates independent variables for visualization purposes
#' @param oss data frame containing all model variables
#' @return data frame containing predictor variables
#'
get_independents <- function(oss){
  independents = oss[, c("project", 
                         "rel_persistent",
                         "rel_persistent_blau",
                         "rel_extensive",
                         "rel_extensive_blau",
                         "issue_focus",
                         "issue_focus_blau",
                         "code_comment_focus",
                         "code_comment_focus_blau",
                         "issue_comment_focus",
                         "issue_comment_focus_blau",
                         "techcontrib_focus",
                         "techcontrib_focus_blau",
                         "rel_high_reputation",
                         "rel_experienced",
                         "rel_experienced_blau",
                         "no_subgroups",
                         "modularity",
                         "proj_age",
                         "proj_size"
  )]
  return(independents)
}


#### plots ####
#'
#'
#'
hist.dep <- function(df){
  
  
  dfmelt <- melt(df[,c("releases", "no_non_core")])
  
  png(
    filename = paste(param.plot.out, "dist_dependents.png", sep = ""),
    res = param.plot.res,
    width = param.plot.width,
    height = param.plot.height,
    units = param.plot.units
  )
  
  p1 <- ggplot(dfmelt, aes(x = value)) +
    geom_histogram() +
    facet_wrap(~variable, scales = "free") +
    labs(subtitle = "no transformation")
  
  # save.plot(p, "histogram_dependents.png")
  
  dfmelt <-melt(log(df[,c("releases", "no_non_core")] + 1))
  
  p2 <- ggplot(dfmelt, aes(x = value)) +
    geom_histogram() +
    facet_wrap(~variable, scales = "free") +
    labs(subtitle = "log-transformed")
  
  grid.arrange(p1, p2, top = "Distribution of dependent variables", nrow = 2)
  
  dev.off()
  # save.plot(p, "histogram_dependents_log.png")
}

#'
#'
#'
hist.indep <- function(df){
  df1 <- df[, c("rel_persistent",
                "rel_extensive",
                "issue_focus",
                "code_comment_focus",
                "issue_comment_focus",
                "techcontrib_focus",
                "rel_high_reputation",
                "rel_experienced")]
  
  df2 <- df[, c("rel_persistent_blau",
                "rel_extensive_blau",
                "issue_focus_blau",
                "code_comment_focus_blau",
                "issue_comment_focus_blau",
                "techcontrib_focus_blau",
                "rel_high_reputation",
                "rel_experienced_blau")]
  
  dfmelt1 <- melt(df1)
  dfmelt2 <- melt(sqrt(df2))
  
  p <- ggplot(dfmelt1, aes(x = value)) +
    geom_histogram() +
    facet_wrap(~variable, scales = "free") +
    labs(title = "Distribution of independent variables",
         subtitle = "no transformation")
  
  save.plot(p, "histogram_independents.png")
  
  p <- ggplot(dfmelt2, aes(x = value)) +
    geom_histogram() +
    facet_wrap(~variable, scales = "free") +
    labs(title = "Distribution of independent variables",
         subtitle = "blau-transformed")
  
  save.plot(p, "histogram_independents_blau.png")
  
}

#'
#'
#'
generate.facet.releases <- function(df){
  
  df <- df[,c("releases", 
              "rel_persistent", 
              "rel_extensive", 
              "issue_focus",
              "code_comment_focus",
              "issue_comment_focus",
              "techcontrib_focus",
              "rel_high_reputation",
              "rel_experienced",
              "no_subgroups")]
  
  dfmelt <- melt(df, id = "releases")
  
  p <- ggplot(dfmelt, aes(x = value, y = dfmelt$releases)) +
    facet_wrap(~variable, ncol = 3) +
    geom_point(alpha = 0.1, size = 1) +
    ylab("Number of releases") +
    ylim(0, 100) +
    theme(strip.text = element_text(size=5)) +
    theme(panel.grid.major = element_line(size = 0.2, colour = "white"), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank()) +
    labs(title = paste("Development activity against independent variables", sep = "")) + 
    stat_smooth(size=0.3)
  
  save.plot(p, paste("facet_plot_releases", ".png", sep = ""))
  
  # excluding all projects which did not release anything
  dfmelt <- dfmelt[!dfmelt$releases == 0,]
  
  p2 <- ggplot(dfmelt, aes(x = value, y = dfmelt$releases)) +
    facet_wrap(~variable, ncol = 4) +
    geom_point(alpha = 0.1, size = 1) +
    ylab("Number of releases") +
    ylim(0, 100) +
    theme(strip.text = element_text(size=5)) +
    theme(panel.grid.major = element_line(size = 0.2, colour = "white"), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank()) +
    labs(title = paste("Development activity against independent variables", sep = ""),
         subtitle = "excluding all projects without releases") + 
    stat_smooth(size=0.3)
  
  save.plot(p2, paste("facet_plot_excl_norelease", ".png", sep = ""))
}

#'
#'
#'
generate.facet.core_ratio <- function(df){
  
  df <- df[,c("ratio_core",
              "rel_persistent", 
              "rel_extensive", 
              "issue_focus",
              "code_comment_focus",
              "issue_comment_focus",
              "techcontrib_focus",
              "rel_high_reputation",
              "rel_experienced",
              "no_subgroups")]
  
  dfmelt <- melt(df, id = "ratio_core")
  
  p <- ggplot(dfmelt, aes(x = value, y = dfmelt$ratio_core)) +
    facet_wrap(~variable, ncol = 4) +
    geom_point(alpha = 0.1, size = 1) +
    ylab("Share of core developers in active developers") +
    theme(strip.text = element_text(size=5)) +
    theme(panel.grid.major = element_line(size = 0.2, colour = "white"), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank()) +
    labs(title = paste("Core developer share", sep = "")) + 
    stat_smooth(size=0.3)
  
  save.plot(p, paste("facet_plot_ratio_core", ".png", sep = ""))
}

#'
#'
#'
generate.facet.no_core <- function(df){
  
  df <- df[,c("no_core",
              "rel_persistent", 
              "rel_extensive", 
              "issue_focus",
              "code_comment_focus",
              "issue_comment_focus",
              "techcontrib_focus",
              "rel_high_reputation",
              "rel_experienced")]
  
  dfmelt <- melt(df, id = "no_core")
  
  p <- ggplot(dfmelt, aes(x = value, y = dfmelt$no_core)) +
    facet_wrap(~variable, ncol = 4) +
    geom_point(alpha = 0.1, size = 1) +
    ylab("No. of core developers") +
    theme(strip.text = element_text(size=5)) +
    theme(panel.grid.major = element_line(size = 0.2, colour = "white"), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank()) +
    labs(title = paste("No of core developers", sep = "")) + 
    stat_smooth(size=0.3)
  
  save.plot(p, paste("facet_plot_no_core", ".png", sep = ""))
}

#'
#'
#'
generate.facet.no_non_core <- function(df){
  
  df <- df[,c("no_non_core",
              "rel_persistent", 
              "rel_extensive", 
              "issue_focus",
              "code_comment_focus",
              "issue_comment_focus",
              "techcontrib_focus",
              "rel_high_reputation",
              "rel_experienced")]
  
  dfmelt <- melt(df, id = "no_non_core")
  
  p <- ggplot(dfmelt, aes(x = value, y = dfmelt$no_non_core)) +
    facet_wrap(~variable, ncol = 4) +
    geom_point(alpha = 0.1, size = 1) +
    ylab("No. of non-core developers") +
    theme(strip.text = element_text(size=5)) +
    theme(panel.grid.major = element_line(size = 0.2, colour = "white"), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank()) +
    labs(title = paste("No of non-core developers", sep = "")) + 
    stat_smooth(size=0.3)
  
  save.plot(p, paste("facet_plot_no_non_core", ".png", sep = ""))
}

#'
#'
#'
plot.facets <- function(oss){
  generate.facet.releases(oss)
  generate.facet.core_ratio(oss)
  generate.facet.no_core(oss)
  generate.facet.no_non_core(oss)
}

#'
#'
#'
c.heatmap <- function(df, title, subtitle = NULL, file.path, reorder = T){
  
  c_m = cor(df, use = "pairwise.complete.obs")
  # stargazer(c, type = "text", out = paste(param.plot.exp, "models/correlation_1_as_is.txt", sep  =""))
  
  # plot a heatmap 
  if(reorder){c_m <- reorder_cormat(c_m)}
  c_m <- get_lower_tri(c_m)
  c_m <- melt(c_m, na.rm = T)
  c_m$value <- round(c_m$value, digits = 2)
  
  p <- ggplot(c_m, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    geom_text(aes(Var1, Var2, label = value), color = "black", size = 2) +
    labs(title = title, subtitle = subtitle) +
    ylab("") +
    xlab("") +
    scale_fill_gradient2(low = "dodgerblue4", 
                         high = "gray35", 
                         mid = "white",
                         midpoint = 0,
                         guide = F,
                         limit = c(-1,1), 
                         space = "Lab", 
                         name="") +
    theme(axis.text.x = element_text(angle = 45, 
                                     vjust = 1, 
                                     hjust = 1),
          legend.justification = c(1, 0),
          legend.position = c(1, 0.7),
          legend.direction = "horizontal") +
    coord_fixed() 
  save.plot(p, file.path)
}

#'
#'
#'
get_lower_tri <- function(c){
  c[upper.tri(c)] <- NA
  return(c)
}

#'
#'
#'
reorder_cormat <- function(c){
  # Use correlation between variables as distance
  dd <- as.dist((1-c)/2)
  hc <- hclust(dd)
  c <-c[hc$order, hc$order]
  
  return(c)
}

#### transformations ####

#' transform right skewed blau measures
#'
#'
transform.blau <- function(oss){
  
  names <- c(
    "rel_persistent_blau",
    "rel_extensive_blau",
    "code_comment_focus_blau",
    "issue_comment_focus_blau",
    "techcontrib_focus_blau",
    "rel_experienced_blau",
    "issue_focus_blau")
  
  for (i in 1:length(names)){
    oss[, names[[i]] ] <- log(((1 - oss[, names[[i]] ]) * 100) + 1)
  }
  
  dfmelt <- melt(oss[, names])
  
  p <- ggplot(dfmelt, aes(x = value)) +
    geom_histogram() +
    facet_wrap(~variable, scales = "free") +
    labs(title = "Distribution of transformed blau",
         subtitle = "log(((1 - x) * 100) + 1)")
  
  save.plot(p, "histogram_blau_transf.png")
  
  return(oss)
}

#' transform skewed control variables
#'
#'
transform.controls<- function(oss){
  
  dfmelt <- melt(oss[, c("releases_control",
                         "proj_size",
                         "proj_age")])
  
  p <- ggplot(dfmelt, aes(x = value)) +
    geom_histogram() +
    facet_wrap(~variable, scales = "free") +
    labs(title = "Distribution of control variables")
  save.plot(p, "histogram_controls.png")
  
  # oss$releases_control <- as.factor(oss$releases_control > 0)
  
  oss$proj_size <- log(oss$proj_size + 1)
  
  # oss$proj_age <- log(oss$proj_age + 1)
  
  dfmelt <- melt(oss[, c("releases_control",
                         "proj_size",
                         "proj_age")])
  
  p <- ggplot(dfmelt, aes(x = value)) +
    geom_histogram() +
    facet_wrap(~variable, scales = "free") +
    labs(title = "Distribution of control variables",
         subtitle = "controls log-transformed")
  save.plot(p, "histogram_controls_log.png")
  
  return(oss)
}


#### output ####
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

#'
#'
#'
m.to.table <- function(m_list, title, m_name){
  
  p_d = c("pearson disp.")
  for(i in 1:length((m_list))){
    p_d[[i + 1]] = pearson.dispersion(m_list[[i]])
  }

  if(param.m.transform.blau){
    m_name = paste(m_name, "btf", sep = "_")
    title = paste(title, " - log(blau)")
  }
  
  if(param.m.transform.controls){
    m_name = paste(param.dataset, m_name, "ctf", sep = "_")
    title = paste(title, " - log(controls)")
  }
  
  stargazer(m_list, 
            type = "text", 
            title = title,
            add.lines = list(p_d),
            out = paste(param.table.out, m_name, ".txt", sep = ""))
}

#'
#'
#'
pearson.dispersion <- function(m){
  
  return(round(sum(residuals(m, type = "pearson")^2)/m$df.residual, digits = 3))
  
}

#'
#'
#'
report.dispersion <- function(d){
  calc <- function(x) {
    
    return(data.frame(variable = names(x),
                      var = round(var(x[[1]]), digits = 1),
                      mean = round(mean(x[[1]]), digits = 1)
    ))
  }
  
  report <- adply(d, 2, function(x) calc(x), .id = NULL)
  
  stargazer(report, 
            type = "text", 
            title = "Dependent variables summary statistics",
            out = paste(param.table.out, "dependents_summary_stats.txt", sep = ""),
            summary = F)
}

#### release models standard ####
estimate.releases.nbr.standard <- function(oss){
  
  #' Negative Binomial GLM
  #' controls: 
  #' - none
  #'
  m.releases.nbr.s.1<- glm.nb(
    releases ~
      rel_persistent + 
      rel_extensive + 
      issue_focus +
      code_comment_focus +
      issue_comment_focus +
      techcontrib_focus +
      rel_high_reputation +
      rel_experienced,
    data = oss,
    control = param.glm.control)
  
  #' Negative Binomial GLM
  #' controls: 
  #' - releases_control ***
  #'
  m.releases.nbr.s.2 <- glm.nb(
    releases ~
      rel_persistent + 
      rel_extensive + 
      issue_focus +
      code_comment_focus +
      issue_comment_focus +
      techcontrib_focus +
      rel_high_reputation +
      rel_experienced +
      releases_control
    ,
    data = oss,
    control = param.glm.control)
  
  #' Negative Binomial GLM
  #' controls: 
  #' - project age ***
  #'
  m.releases.nbr.s.3 <- glm.nb(
    releases ~
      rel_persistent + 
      rel_extensive + 
      issue_focus +
      code_comment_focus +
      issue_comment_focus +
      techcontrib_focus +
      rel_high_reputation +
      rel_experienced +
      proj_age
    ,
    data = oss,
    control = param.glm.control)
  
  #' Negative Binomial GLM
  #' controls: 
  #' - project size ***
  #'
  m.releases.nbr.s.4 <- glm.nb(
    releases ~
      rel_persistent + 
      rel_extensive + 
      issue_focus +
      code_comment_focus +
      issue_comment_focus +
      techcontrib_focus +
      rel_high_reputation +
      rel_experienced +
      proj_size
    ,
    data = oss,
    control = param.glm.control)
  
  #' Negative Binomial GLM
  #' controls:  ***
  #' - proj size
  #' - releases control
  #'
  m.releases.nbr.s.5 <- glm.nb(
    releases ~
      rel_persistent + 
      rel_extensive + 
      issue_focus +
      code_comment_focus +
      issue_comment_focus +
      techcontrib_focus +
      rel_high_reputation +
      rel_experienced +
      releases_control +
      proj_size
    ,
    data = oss,
    control = param.glm.control)
  
  #' Negative Binomial GLM
  #' controls:
  #' - proj size
  #' - releases control
  #'
  m.releases.nbr.s.6 <- glm.nb(
    releases ~
      rel_persistent + 
      rel_extensive + 
      issue_focus +
      code_comment_focus +
      techcontrib_focus +
      rel_high_reputation +
      rel_experienced +
      releases_control +
      proj_size
    ,
    data = oss,
    control = param.glm.control)
  
  out <- list()
  out [[1]] <- m.releases.nbr.s.1
  out [[2]] <- m.releases.nbr.s.2
  out [[3]] <- m.releases.nbr.s.3
  out [[4]] <- m.releases.nbr.s.4
  out [[5]] <- m.releases.nbr.s.5
  out [[6]] <- m.releases.nbr.s.6
  
  m.to.table(out, 
             title = "Release models (standard indicators, glm family: negbin)",
             m_name = "nbr_releases_standard") 
}

estimate.releases.zinbr.standard <- function(oss){
  
  #' ZIPR
  #' no controls
  #'
  m.releases.zipr.standard.1 <- zeroinfl(releases ~
                                           rel_persistent + 
                                           rel_extensive + 
                                           issue_focus +
                                           code_comment_focus +
                                           techcontrib_focus +
                                           rel_high_reputation +
                                           rel_experienced | releases_control_cat
                                         ,
                                         data = oss)
  
  #' ZINBR 1
  #' no controls
  #'
  m.releases.zinbr.standard.1 <- zeroinfl(releases ~
                                            rel_persistent + 
                                            rel_extensive + 
                                            issue_focus +
                                            code_comment_focus +
                                            techcontrib_focus +
                                            rel_high_reputation +
                                            rel_experienced | releases_control_cat
                                          ,
                                          data = oss,
                                          EM = T,
                                          dist = "negbin",
                                          control = zeroinfl.control(method ="BFGS", maxit = 10000, trace = F))
  
  #' VUONG test comparing m.zipr1 and m.zinbr1
  #'
  vuong(m.releases.zipr.standard.1, m.releases.zinbr.standard.1)
  
  
  #' ZINBR 2
  #' controlling for previous releases
  #'
  m.releases.zinbr.standard.2 <- zeroinfl(releases ~
                                            rel_persistent + 
                                            rel_extensive + 
                                            issue_focus +
                                            code_comment_focus +
                                            techcontrib_focus +
                                            rel_high_reputation +
                                            rel_experienced +
                                            releases_control | releases_control_cat
                                          ,
                                          data = oss,
                                          dist = "negbin",
                                          # EM = T,
                                          control = zeroinfl.control(method ="BFGS", maxit = 10000, trace = F))
  
  
  #' ZINBR 3
  #' controlling for project size
  #'
  m.releases.zinbr.standard.3 <- zeroinfl(releases ~
                                            rel_persistent + 
                                            rel_extensive + 
                                            issue_focus +
                                            code_comment_focus +
                                            techcontrib_focus +
                                            rel_high_reputation +
                                            rel_experienced +
                                            proj_size | releases_control_cat
                                          ,
                                          data = oss,
                                          dist = "negbin")
  
  #' ZINBR 4
  #' controlling for project size and previous releases
  #'
  m.releases.zinbr.standard.4 <- zeroinfl(releases ~
                                            rel_persistent + 
                                            rel_extensive + 
                                            issue_focus +
                                            code_comment_focus +
                                            techcontrib_focus +
                                            rel_high_reputation +
                                            rel_experienced +
                                            releases_control +
                                            proj_size | releases_control_cat
                                          ,
                                          data = oss,
                                          dist = "negbin")
  
  out <- list()
  out [[1]] <- m.releases.zinbr.standard.1
  out [[2]] <- m.releases.zinbr.standard.2
  out [[3]] <- m.releases.zinbr.standard.3
  out [[4]] <- m.releases.zinbr.standard.4
  
  m.to.table(out, 
             title = "Zero inflated release models (standard indicators, glm family: zi-negbin)", 
             m_name = "zinbr_releases_standard"
  ) 
}

estimate.ci_releases.nbr.standard <- function(oss){
  
  #' Negative Binomial GLM
  #' controls: 
  #' - ci (yes/no)
  #'
  m.releases.ci.nbr.standard.1 <- glm.nb(
    releases ~
      rel_persistent + 
      rel_extensive + 
      issue_focus +
      code_comment_focus +
      issue_comment_focus +
      techcontrib_focus +
      rel_high_reputation +
      rel_experienced +
      has_travis,
    data = oss,
    control = param.glm.control)
  
  #' Negative Binomial GLM
  #' controls: 
  #' - releases_control ***
  #' - ci (yes/no)
  #'
  m.releases.ci.nbr.standard.2 <- glm.nb(
    releases ~
      rel_persistent + 
      rel_extensive + 
      issue_focus +
      code_comment_focus +
      issue_comment_focus +
      techcontrib_focus +
      rel_high_reputation +
      rel_experienced +
      releases_control +
      has_travis
    ,
    data = oss,
    control = param.glm.control)
  
  #' Negative Binomial GLM
  #' controls: 
  #' - project size ***
  #' - ci (yes/no)
  #'
  m.releases.ci.nbr.standard.3 <- glm.nb(
    releases ~
      rel_persistent + 
      rel_extensive + 
      issue_focus +
      code_comment_focus +
      issue_comment_focus +
      techcontrib_focus +
      rel_high_reputation +
      rel_experienced +
      proj_size +
      has_travis
    ,
    data = oss,
    control = param.glm.control)
  
  
  #' Negative Binomial GLM
  #' controls: ***
  #' - releases control
  #' - project size 
  #' - ci (yes/no)
  #'
  m.releases.ci.nbr.standard.4 <- glm.nb(
    releases ~
      rel_persistent + 
      rel_extensive + 
      issue_focus +
      code_comment_focus +
      issue_comment_focus +
      techcontrib_focus +
      rel_high_reputation +
      rel_experienced +
      releases_control +
      proj_size +
      has_travis
    ,
    data = oss,
    control = param.glm.control)
  
  out <- list()
  out [[1]] <- m.releases.ci.nbr.standard.1
  out [[2]] <- m.releases.ci.nbr.standard.2
  out [[3]] <- m.releases.ci.nbr.standard.3
  out [[4]] <- m.releases.ci.nbr.standard.4
  
  m.to.table(out, 
             title = "Release models - controlling for CI (Standard indicators, glm family: negbin)", 
             m_name = "nbr_ci_releases_standard"
  ) 
}

#### release models blau ####
estimate.releases.nbr.blau <- function(oss){
  
  #' Negative Binomial GLM
  #' controls: 
  #' - none
  #'
  m.releases.nbr.blau.1 <- glm.nb(
    releases ~
      rel_persistent_blau + 
      rel_extensive_blau + 
      issue_focus_blau +
      code_comment_focus_blau +
      issue_comment_focus_blau +
      techcontrib_focus_blau +
      rel_high_reputation +
      rel_experienced_blau,
    data = oss,
    control = param.glm.control)
  
  
  #' Negative Binomial GLM
  #' controls: 
  #' - releases_control ***
  #'
  m.releases.nbr.blau.2 <- glm.nb(
    releases ~
      rel_persistent_blau + 
      rel_extensive_blau + 
      issue_focus_blau +
      code_comment_focus_blau +
      issue_comment_focus_blau +
      techcontrib_focus_blau +
      rel_high_reputation +
      rel_experienced_blau +
      releases_control
    ,
    data = oss,
    control = param.glm.control)
  
  #' Negative Binomial GLM
  #' controls: 
  #' - project age ***
  #'
  m.releases.nbr.blau.3 <- glm.nb(
    releases ~
      rel_persistent_blau + 
      rel_extensive_blau + 
      issue_focus_blau +
      code_comment_focus_blau +
      issue_comment_focus_blau +
      techcontrib_focus_blau +
      rel_high_reputation +
      rel_experienced_blau +
      proj_age
    ,
    data = oss,
    control = param.glm.control)
  
    #' Negative Binomial GLM
  #' controls: 
  #' - project size ***
  #'
  m.releases.nbr.blau.4 <- glm.nb(
    releases ~
      rel_persistent_blau + 
      rel_extensive_blau + 
      issue_focus_blau +
      code_comment_focus_blau +
      issue_comment_focus_blau +
      techcontrib_focus_blau +
      rel_high_reputation +
      rel_experienced_blau +
      proj_size
    ,
    data = oss,
    control = param.glm.control)
  
  
  #' Negative Binomial GLM
  #' controls: ***
  #' - releases control
  #' - project size 
  #'
  m.releases.nbr.blau.5 <- glm.nb(
    releases ~
      rel_persistent_blau + 
      rel_extensive_blau + 
      issue_focus_blau +
      code_comment_focus_blau +
      issue_comment_focus_blau +
      techcontrib_focus_blau +
      rel_high_reputation +
      rel_experienced_blau +
      releases_control +
      proj_size
    ,
    data = oss,
    control = param.glm.control)
  
  #' Negative Binomial GLM
  #' controls:
  #' - releases control
  #' - project size 
  #'
  m.releases.nbr.blau.6 <- glm.nb(
    releases ~
      rel_persistent_blau + 
      rel_extensive_blau + 
      issue_focus_blau +
      techcontrib_focus_blau +
      code_comment_focus_blau +
      rel_high_reputation +
      rel_experienced_blau +
      releases_control +
      proj_size
    ,
    data = oss,
    control = param.glm.control)
  
  #'
  #' added interaction terms
  #'
  m.releases.nbr.blau.7 <- glm.nb(
    releases ~
      rel_persistent_blau * 
      rel_extensive_blau + 
      issue_focus_blau *
      techcontrib_focus_blau +
      code_comment_focus_blau +
      rel_high_reputation *
      rel_experienced_blau +
      releases_control +
      proj_size
    ,
    data = oss,
    control = param.glm.control)
  
  
  out <- list()
  out [[1]] <- m.releases.nbr.blau.1
  out [[2]] <- m.releases.nbr.blau.2
  out [[3]] <- m.releases.nbr.blau.4
  out [[4]] <- m.releases.nbr.blau.5
  out [[5]] <- m.releases.nbr.blau.6
  
  m.to.table(out, 
             title = "Release models - (Blau indicators, glm family: negbin)", 
             m_name = "nbr_releases_blau"
  ) 
}

estimate.releases.zinbr.blau <- function(oss){
  
  #' ZIPR
  #' no controls
  #'
  m.releases.zipr.blau.1 <- zeroinfl(releases ~
                                       rel_persistent_blau + 
                                       rel_extensive_blau + 
                                       issue_focus_blau +
                                       code_comment_focus_blau +
                                       techcontrib_focus_blau +
                                       rel_high_reputation +
                                       rel_experienced_blau | releases_control_cat
                                     ,
                                     data = oss)
  
  #' ZINBR 1
  #' no controls
  #'
  m.releases.zinbr.blau.1 <- zeroinfl(releases ~
                                        rel_persistent_blau + 
                                        rel_extensive_blau + 
                                        issue_focus_blau +
                                        code_comment_focus_blau +
                                        techcontrib_focus_blau +
                                        rel_high_reputation +
                                        rel_experienced_blau | releases_control_cat
                                      ,
                                      data = oss,
                                      EM = T,
                                      dist = "negbin",
                                      control = zeroinfl.control(method ="BFGS", maxit = 10000, trace = F))
  
  #' VUONG test comparing m.zipr1 and m.zinbr1
  #'
  vuong(m.releases.zipr.blau.1, m.releases.zinbr.blau.1)
  
  
  #' ZINBR 2
  #' controlling for previous releases
  #'
  m.releases.zinbr.blau.2 <- zeroinfl(releases ~
                                        rel_persistent_blau + 
                                        rel_extensive_blau + 
                                        issue_focus_blau +
                                        code_comment_focus_blau +
                                        techcontrib_focus_blau +
                                        rel_high_reputation +
                                        rel_experienced_blau +
                                        releases_control | releases_control_cat
                                      ,
                                      data = oss,
                                      dist = "negbin",
                                      # EM = T,
                                      control = zeroinfl.control(method ="BFGS", maxit = 10000, trace = F))
  
  
  #' ZINBR 3
  #' controlling for project size
  #'
  m.releases.zinbr.blau.3 <- zeroinfl(releases ~
                                        rel_persistent_blau + 
                                        rel_extensive_blau + 
                                        issue_focus_blau +
                                        code_comment_focus_blau +
                                        techcontrib_focus_blau +
                                        rel_high_reputation +
                                        rel_experienced_blau +
                                        proj_size | releases_control_cat
                                      ,
                                      data = oss,
                                      dist = "negbin")
  
  #' ZINBR 4
  #' controlling for project size and previous releases
  #'
  m.releases.zinbr.blau.4 <- zeroinfl(releases ~
                                        rel_persistent_blau + 
                                        rel_extensive_blau + 
                                        issue_focus_blau +
                                        code_comment_focus_blau +
                                        techcontrib_focus_blau +
                                        rel_high_reputation +
                                        rel_experienced_blau +
                                        releases_control +
                                        proj_size | releases_control_cat
                                      ,
                                      data = oss,
                                      dist = "negbin")
  
  out <- list()
  out [[1]] <- m.releases.zinbr.blau.1
  out [[2]] <- m.releases.zinbr.blau.2
  out [[3]] <- m.releases.zinbr.blau.3
  out [[4]] <- m.releases.zinbr.blau.4
  
  m.to.table(out, 
             title = "Zero inflated release models (Blau indicators - glm family: ZINBR)", 
             m_name = "zinbr_releases_blau"
  ) 
}

estimate.ci_releases.nbr.blau <- function(oss){
  
  #' Negative Binomial GLM
  #' 
  #' dependent variable: 
  #' - releases
  #' 
  #' predictors: 
  #' - all faultine ratios (variety diversity indicators were blau transformed!)
  #' 
  #' controls: 
  #' - none
  #' - ci (yes/no)
  #'
  m.releases.ci.nbr.blau.1 <- glm.nb(
    releases ~
      rel_persistent_blau + 
      rel_extensive_blau + 
      issue_focus_blau +
      code_comment_focus_blau +
      issue_comment_focus_blau +
      techcontrib_focus_blau +
      rel_high_reputation +
      rel_experienced_blau +
      has_travis,
    data = oss,
    control = param.glm.control)
  
  #' Negative Binomial GLM
  #' 
  #' dependent variable: 
  #' - releases
  #' 
  #' predictors: 
  #' - all faultine ratios (variety diversity indicators were blau transformed!)
  #' 
  #' controls: 
  #' - releases_control ***
  #' - ci (yes/no)
  #'
  m.releases.ci.nbr.blau.2 <- glm.nb(
    releases ~
      rel_persistent_blau + 
      rel_extensive_blau + 
      issue_focus_blau +
      code_comment_focus_blau +
      issue_comment_focus_blau +
      techcontrib_focus_blau +
      rel_high_reputation +
      rel_experienced_blau +
      releases_control +
      has_travis
    ,
    data = oss,
    control = param.glm.control)
  
  #' Negative Binomial GLM
  #' 
  #' 
  #' dependent variable: 
  #' - releases
  #' 
  #' predictors: 
  #' - all faultine ratios (variety diversity indicators were blau transformed!)
  #' 
  #' controls: 
  #' - project size ***
  #' - ci (yes/no)
  #'
  m.releases.ci.nbr.blau.3 <- glm.nb((releases) ~
                  rel_persistent_blau + 
                  rel_extensive_blau + 
                  issue_focus_blau +
                  code_comment_focus_blau +
                  issue_comment_focus_blau +
                  techcontrib_focus_blau +
                  rel_high_reputation +
                  rel_experienced_blau +
                  proj_size +
                  has_travis
                ,
                data = oss,
                control = param.glm.control)
    
    
    #' Negative Binomial GLM
    #' 
    #' 
    #' dependent variable: 
    #' - releases
    #' 
    #' predictors: 
    #' - all faultine ratios (variety diversity indicators were blau transformed!)
    #' 
    #' controls: ***
    #' - releases control
    #' - project size 
    #' - ci (yes/no)
    #'
    m.releases.ci.nbr.blau.4 <- glm.nb(
      releases ~
        rel_persistent_blau + 
        rel_extensive_blau + 
        issue_focus_blau +
        code_comment_focus_blau +
        issue_comment_focus_blau +
        techcontrib_focus_blau +
        rel_high_reputation +
        rel_experienced_blau +
        releases_control +
        proj_size +
        has_travis
      ,
      data = oss,
      control = param.glm.control)
    
    out <- list()
    out [[1]] <- m.releases.ci.nbr.blau.1
    out [[2]] <- m.releases.ci.nbr.blau.2
    out [[3]] <- m.releases.ci.nbr.blau.3
    out [[4]] <- m.releases.ci.nbr.blau.4
    
    m.to.table(out, 
               title = "Release models - controlling for CI", 
               m_name = "nbr_ci_releases_blau"
    ) 
  }
  

#### non-core models standard ####
estimate.noncore.nbr.standard <- function(oss){
  
  #' Negative Binomial GLM
  #' - no controls
  #'
  m.noncore.nbr.standard.1 <- glm.nb(
    no_non_core ~
      rel_persistent + 
      rel_extensive + 
      issue_focus +
      code_comment_focus +
      issue_comment_focus +
      techcontrib_focus +
      rel_high_reputation +
      rel_experienced
    ,
    data = oss,
    control = param.glm.control)
  
  #' Negative Binomial GLM
  #' controls: 
  #' - previous releases ***
  #'
  m.noncore.nbr.standard.2 <- glm.nb(
    no_non_core ~
      rel_persistent + 
      rel_extensive + 
      issue_focus +
      code_comment_focus +
      issue_comment_focus +
      techcontrib_focus +
      rel_high_reputation +
      rel_experienced +
      release_control
    ,
    data = oss,
    control = param.glm.control)
  
  
  #' Negative Binomial GLM
  #' controls: 
  #' - project size ***
  #'
  m.noncore.nbr.standard.3 <- glm.nb(
    no_non_core ~
      rel_persistent + 
      rel_extensive + 
      issue_focus +
      code_comment_focus +
      techcontrib_focus +
      rel_high_reputation +
      rel_experienced +
      proj_size 
    ,
    data = oss,
    control = param.glm.control)
  
  #' Negative Binomial GLM
  #' controls: 
  #' - project size
  #' - previous releases
  m.noncore.nbr.standard.4 <- glm.nb(
    no_non_core ~
      rel_persistent + 
      rel_extensive + 
      issue_focus +
      code_comment_focus +
      techcontrib_focus +
      rel_high_reputation +
      rel_experienced +
      proj_size +
      release_control
    ,
    data = oss,
    control = param.glm.control)
  
  out <- list()
  out [[1]] <- m.noncore.nbr.standard.1
  out [[2]] <- m.noncore.nbr.standard.2
  out [[3]] <- m.noncore.nbr.standard.3
  out [[4]] <- m.noncore.nbr.standard.4
  
  m.to.table(out, 
             title = "Noncore models (negbin)", 
             m_name = "nbr_noncore_standard"
  ) 
  
}

estimate.ci_noncore.nbr.standard <- function(oss){
  
  #' Negative Binomial GLM
  #' - ci (yes/no)
  #' 
  m.noncore.ci.nbr.standard.1 <- glm.nb(
    no_non_core ~
      rel_persistent + 
      rel_extensive + 
      issue_focus +
      code_comment_focus +
      issue_comment_focus +
      techcontrib_focus +
      rel_high_reputation +
      rel_experienced +
      has_travis
    ,
    data = oss,
    control = param.glm.control)
  
  #' Negative Binomial GLM
  #' controls: 
  #' - release control ***
  #' - ci (yes/no)
  m.noncore.ci.nbr.standard.2 <- glm.nb(
    no_non_core ~
      rel_persistent + 
      rel_extensive + 
      issue_focus +
      code_comment_focus +
      issue_comment_focus +
      techcontrib_focus +
      rel_high_reputation +
      rel_experienced +
      release_control +
      has_travis
    ,
    data = oss,
    control = param.glm.control)
  
  #' Negative Binomial GLM
  #' controls: 
  #' - project size ***
  #' - 
  m.noncore.ci.nbr.standard.3 <- glm.nb(
    no_non_core ~
      rel_persistent + 
      rel_extensive + 
      issue_focus +
      code_comment_focus +
      issue_comment_focus +
      techcontrib_focus +
      rel_high_reputation +
      rel_experienced +
      proj_size +
      has_travis
    ,
    data = oss,
    control = param.glm.control)
  
  #' Negative Binomial GLM
  #' controls: ***
  #' - release control
  #' - project size 
  #' - has travis
  #'
  m.noncore.ci.nbr.standard.4 <- glm.nb(
    no_non_core ~
      rel_persistent + 
      rel_extensive + 
      issue_focus +
      code_comment_focus +
      issue_comment_focus +
      techcontrib_focus +
      rel_high_reputation +
      rel_experienced +
      release_control + 
      proj_size +
      has_travis
    ,
    data = oss,
    control = param.glm.control)
  
  out <- list()
  out [[1]] <- m.noncore.ci.nbr.standard.1
  out [[2]] <- m.noncore.ci.nbr.standard.2
  out [[3]] <- m.noncore.ci.nbr.standard.3
  out [[4]] <- m.noncore.ci.nbr.standard.4
  
  m.to.table(out, 
             title = "Noncore models - CI (negbin)", 
             m_name = "nbr_noncore_ci_standard"
  ) 
}

#### non-core models blau ####
estimate.noncore.nbr.blau <- function(oss){
  
  #' Negative Binomial GLM
  #' controls: 
  #' - none
  #'
  m.noncore.nbr.blau.1 <- glm.nb(
    no_non_core ~
      rel_persistent_blau + 
      rel_extensive_blau + 
      issue_focus_blau +
      code_comment_focus_blau +
      issue_comment_focus_blau +
      techcontrib_focus_blau +
      rel_high_reputation +
      rel_experienced_blau 
    ,
    data = oss,
    control = param.glm.control)
  
  #' Negative Binomial GLM
  #' controls: 
  #' - release control
  #'
  m.noncore.nbr.blau.2 <- glm.nb(no_non_core ~
                  rel_persistent_blau + 
                  rel_extensive_blau + 
                  issue_focus_blau +
                  code_comment_focus_blau +
                  issue_comment_focus_blau +
                  techcontrib_focus_blau +
                  rel_high_reputation +
                  rel_experienced_blau +
                  release_control
                ,
                data = oss,
                control = param.glm.control)
  
  #' Model specification:
  #' Negative Binomial GLM
  #' 
  #' 
  #' dependent variable: 
  #' - number of non-core contributors 
  #' 
  #' predictors: 
  #' - all faultine ratios (variety diversity indicators were blau transformed!)
  #' 
  #' controls: 
  #' - project size ***
  #'
    m.noncore.nbr.blau.3 <- glm.nb(
      no_non_core ~
                  rel_persistent_blau + 
                  rel_extensive_blau + 
                  issue_focus_blau +
                  code_comment_focus_blau +
                  issue_comment_focus_blau +
                  techcontrib_focus_blau +
                  rel_high_reputation +
                  rel_experienced_blau +
                  proj_size
                ,
                data = oss,
                control = param.glm.control)

  #' Negative Binomial GLM
  #' controls: ***
  #' 
  #' - release control
  #' - project size 
  #'
    m.noncore.nbr.blau.4 <- glm.nb(
      no_non_core ~
                  rel_persistent_blau + 
                  rel_extensive_blau + 
                  issue_focus_blau +
                  code_comment_focus_blau +
                  issue_comment_focus_blau +
                  techcontrib_focus_blau +
                  rel_high_reputation +
                  rel_experienced_blau +
                  release_control +
                  proj_size
                ,
                data = oss,
                control = param.glm.control)
    
  
    out <- list()
    out [[1]] <- m.noncore.nbr.blau.1
    out [[2]] <- m.noncore.nbr.blau.2
    out [[3]] <- m.noncore.nbr.blau.3
    out [[4]] <- m.noncore.nbr.blau.4
    
    m.to.table(out, 
               title = "Noncore models (negbin)", 
               m_name = "nbr_noncore_blau"
    ) 
  
}

estimate.ci_noncore.nbr.blau <- function(oss){
  
  #' Negative Binomial GLM
  #' controls: 
  #' - ci (yes/no)
  m.noncore.ci.nbr.blau.1 <- glm.nb(
    no_non_core ~
      rel_persistent_blau + 
      rel_extensive_blau + 
      issue_focus_blau +
      code_comment_focus_blau +
      issue_comment_focus_blau +
      techcontrib_focus_blau +
      rel_high_reputation +
      rel_experienced_blau +
      has_travis
    ,
    data = oss,
    control = param.glm.control)
  
  #' Negative Binomial GLM
  #' controls: 
  #' - release control ***
  #' - ci (yes/no)
    m.noncore.ci.nbr.blau.2 <- glm.nb(
      no_non_core ~
                  rel_persistent_blau + 
                  rel_extensive_blau + 
                  issue_focus_blau +
                  code_comment_focus_blau +
                  issue_comment_focus_blau +
                  techcontrib_focus_blau +
                  rel_high_reputation +
                  rel_experienced_blau +
                  release_control +
                  has_travis
                ,
                data = oss,
                control = param.glm.control)

    #' Negative Binomial GLM
    #' controls: 
    #' - project size ***
    #' - ci (yes/no)
    m.noncore.ci.nbr.blau.3 <- glm.nb(
      no_non_core ~
        rel_persistent_blau + 
        rel_extensive_blau + 
        issue_focus_blau +
        code_comment_focus_blau +
        issue_comment_focus_blau +
        techcontrib_focus_blau +
        rel_high_reputation +
        rel_experienced_blau +
        proj_size +
        has_travis
      ,
      data = oss,
      control = param.glm.control)
    
  
    #' Negative Binomial GLM
    #' controls: 
    #' - release control
    #' - project size ***
    #' - ci (yes/no)
    m.noncore.ci.nbr.blau.4 <- glm.nb(
      no_non_core ~
        rel_persistent_blau + 
        rel_extensive_blau + 
        issue_focus_blau +
        code_comment_focus_blau +
        issue_comment_focus_blau +
        techcontrib_focus_blau +
        rel_high_reputation +
        rel_experienced_blau +
        proj_size +
        has_travis
      ,
      data = oss,
      control = param.glm.control)
    
    out <- list()
    out [[1]] <- m.noncore.ci.nbr.blau.1
    out [[2]] <- m.noncore.ci.nbr.blau.2
    out [[3]] <- m.noncore.ci.nbr.blau.3
    out [[4]] <- m.noncore.ci.nbr.blau.4
    
    m.to.table(out, 
               title = "Noncore models - CI (negbin)", 
               m_name = "nbr_noncore_ci_blau"
    )   
  
}

#### main ####
main <- function(){
  oss = import.variables()
  
  independents <- get_independents(oss)
  dependents <- get_dependents(oss)
  
  report.dispersion(dependents[, c("releases", "no_non_core")])
  
  # plot distributions
  if(param.plot.hist){
    hist.dep(dependents[, -1])
    hist.indep(independents[, -1])
  }
  
  if(param.plot.correlation){
    # create correlation matrices
    # c.heatmap(independents[, -1], 
    c.heatmap(subset(independents, select = -c(project, 
                                               issue_comment_focus, 
                                               issue_focus, 
                                               rel_experienced,
                                               rel_persistent,
                                               code_comment_focus,
                                               techcontrib_focus,
                                               modularity,
                                               no_subgroups,
                                               rel_extensive
    )),
    title = "Pearson correlation",
    subtitle = "independent variables",
    file.path = "cor_independents.png")
    
    c.heatmap(independents[, c("issue_focus_blau", 
                               "issue_comment_focus_blau", 
                               "code_comment_focus_blau", 
                               "techcontrib_focus_blau")], 
              title = "Pearson correlation",
              subtitle = "subgroup shares",
              file.path = "cor_subgroup_shares.png",
              reorder = F)
    
    c.heatmap(dependents[, -1],
              title = "Pearson correlation",
              subtitle = "dependent variables",
              file.path = "cor_dependents.png")
  }
  
  if(param.plot.facets){ 
    plot.facets(oss)
  }
  
  # estimate some models
  if(param.models.estimate){
    
    if(T){
      estimate.releases.nbr.standard(oss)
      estimate.releases.nbr.blau(oss)
      
      # estimate.releases.zinb.blau/standard(oss) -> read about how to compare it here: https://statisticalhorizons.com/zero-inflated-models
    }
    
    if(T){
      estimate.noncore.nbr.blau(oss)
    }
    
    if(T){
      estimate.ci_releases.nbr.blau(oss)
      estimate.ci_noncore.nbr.blau(oss)
    }
    
  }
}

main()