#### libraries ####
library(ggplot2)
library(reshape2)
library(car)
library(MASS)
library(stargazer)
library(plyr)
library(pscl)
library(modEvA)
library(gridExtra)

#### parameters ####
param.dataset = "sp180_c20" # can be "sp180_c10", "sp180_c20" or "sp90_c20" 
param.sd.median = T

param.plot.facets = F
param.plot.correlation = T
param.plot.hist = T
param.glm.control = glm.control(epsilon = 1e-8, maxit = 100, trace = FALSE)

param.m.transform.blau = T  # no significant effects when untransformed
param.m.transform.controls = F

param.table.format = "text"  # can be "latex" or "txt"
param.table.file = ".tex"      # can be .tex or .txt

param.path.root = "/Users/Max/Desktop/MA/R/NetworkAnalyzer/faultlines/"

if(param.sd.median){
  param.m.in = paste(param.path.root, "data/models_median/", param.dataset, "/", sep = "")
  param.table.out =  paste(param.path.root, "/analysis/", param.dataset, "/models_median/tables/", sep = "")
  param.plot.out = paste(param.path.root, "analysis/", param.dataset, "/models_median/plots/", sep = "")
} else {
  param.m.in = paste(param.path.root, "data/models/", param.dataset, "/", sep = "")
  param.table.out =  paste(param.path.root, "/analysis/", param.dataset, "/models/tables/", sep = "")
  param.plot.out = paste(param.path.root, "analysis/", param.dataset, "/models/plots/", sep = "")
}

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
  oss = merge(x = independents, y = dependents, by = "project")
  oss = merge(x = oss, y=ci, by = "project")
  
  oss$has_travis = as.factor(oss$has_travis)
  
  if(param.m.transform.blau){
    oss = transform.blau(oss)
  }
  
  if(param.m.transform.controls){
    oss = transform.controls(oss)
  }
  
  # calculate categorial variable for zero inflated NBR and PR
  oss$releases_control_cat = as.factor(oss$releases_control > 0)
  
  return(oss)
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

#'
#'
#'
formula.add.variable <- function(form, var){
  vars = all.vars(form)
  resp = vars[1]
  rhs = vars[2:length(vars)]
  
  len_init = length(rhs)
  
  for (i in 1:length(var)){
    rhs[len_init + i] <- var[i]
  }
  
  return(reformulate(rhs, response=resp))
}

#'
#'
#'
formula.drop.variable <- function(form, var){
  vars = all.vars(form)
  resp = vars[1]
  rhs = vars[2:length(vars)]
  
  pos = match(var, rhs)
  
  return(reformulate(rhs[-pos], resp))
}

#'
#'
#'
outliers.remove.releases <- function(oss){
  oss <- oss[-c(76, 190, 262, 386, 451, 467),  ]
  return(oss)  
}

#'
#'
#'
outliers.remove.noncore <- function(oss){
  oss <- oss[-c(37, 234), ]
  
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
m.to.table <- function(m_list, title, m_name, label, float.env = "table", ns = NULL){
  
  p_d = c("pearson disp.")
  pR2_McFadden = c("McFadden")
  pR2_Nagelkerke = c("Nagelkerke")
  for(i in 1:length((m_list))){
    
    if("glm" %in% class(m_list[[i]])){
      # calculate pseudo R2 for NBRs
      pR2 = modEvA::RsqGLM(m_list[[i]])
      pR2_McFadden[[i + 1]] = round(pR2$McFadden, digits = 4)
      pR2_Nagelkerke[[i + 1]] = round(pR2$Nagelkerke, digits = 4)
    } else if("zeroinfl" %in% class(m_list[[i]])){
      
      pR2_McFadden[[i + 1]] = round(
        (1 - summary(m_list[[i]])$loglik/update(m_list[[i]], . ~ 1)$loglik)
      , digits = 4)
      pR2_Nagelkerke[[i + 1]] = "-"
    }
    p_d[[i + 1]] = pearson.dispersion(m_list[[i]])
  }
  
  stargazer(m_list, 
            type = param.table.format, 
            title = title,
            add.lines = list(p_d, pR2_McFadden, pR2_Nagelkerke),
            out = paste(param.table.out, m_name, param.table.file, sep = ""),
            no.space = F,
            notes.align = "l",
            single.row = F,
            report = "vc*",
            label = label,
            float = T,
            float.env = float.env            
            )
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
            type = param.table.format, 
            title = "Dependent variables summary statistics",
            out = paste(param.table.out, "dependents_summary_stats.txt", sep = ""),
            summary = F)
}


#### release models standard ####
estimate.releases.nbr.standard <- function(){
  
  m.releases.nbr.controls <- glm.nb(releases ~ releases_control_cat +
                                      proj_age +
                                      proj_size, 
                                    data = oss.releases, 
                                    control = param.glm.control)
  
  form.s = releases ~
    rel_high_reputation +
    rel_experienced +
    issue_focus +
    techcontrib_focus +
    code_comment_focus +
    issue_comment_focus +
    rel_persistent + 
    rel_extensive
  
  #' Negative Binomial GLM
  #' controls: 
  #' - none
  #'
  form = form.s
  m.releases.nbr.s.1<- glm.nb(form, data = oss.releases, control = param.glm.control)
  
  #' Negative Binomial GLM
  #' controls: 
  #' - releases_control ***
  #'
  form = formula.add.variable(form.s, "releases_control_cat")
  m.releases.nbr.s.2 <- glm.nb(form, data = oss.releases,  control = param.glm.control)
  
  #' Negative Binomial GLM
  #' controls: 
  #' - project age ***
  #'
  form = formula.add.variable(form.s, c("releases_control_cat", "proj_age"))
  m.releases.nbr.s.3 <- glm.nb(form, data = oss.releases,  control = param.glm.control)
  
  #' Negative Binomial GLM
  #' controls:  ***
  #' - proj size
  #' - releases control
  form = formula.add.variable(form.s, c("releases_control_cat", "proj_age", "proj_size"))
  m.releases.nbr.s.5 <- glm.nb(form, data = oss.releases,  control = param.glm.control)
  
  #' Negative Binomial GLM
  #' removed issue_comment_focus
  #' 
  #' controls:
  #' - proj size
  #' - releases control
  #'
  form = formula.drop.variable(form.s, "issue_focus")
  form = formula.add.variable(form, c("releases_control_cat", "proj_age", "proj_size"))
  m.releases.nbr.s.6 <- glm.nb(form, data = oss.releases, control = param.glm.control)
  
  out <- list()
  out [[1]] <- m.releases.nbr.s.1
  out [[2]] <- m.releases.nbr.s.2
  out [[3]] <- m.releases.nbr.s.3
  out [[4]] <- m.releases.nbr.s.5
  out [[5]] <- m.releases.nbr.s.6
  out [[6]] <- m.releases.nbr.controls
  
  m.to.table(out, 
             title = "Release models (standard indicators, glm family: negbin)",
             m_name = "nbr_releases_simple",
             label = "tab:res_nbr_release_simple",
             float.env = "sidewaystable",
             ns = T)
  
  message("VIF Model 5:")
  print(vif(m.releases.nbr.s.5))
  
  # message("Likelihood ratio test:")
  # print(summary(pscl::odTest(m.releases.nbr.s.6)))
  
  return(m.releases.nbr.s.6)
}

estimate.releases.zinbr.standard <- function(nbr){

  m.releases.zinbr.controls <- zeroinfl(releases ~ releases_control_cat + proj_age + proj_size | releases_control_cat,
                                        data = oss.releases,
                                        EM = T,
                                        dist = "negbin")
  
  #' ZINBR 1
  #' no controls
  #'
  m.releases.zinbr.standard.1 <- zeroinfl(releases ~
                                            rel_high_reputation +
                                            rel_experienced +
                                            issue_focus +
                                            techcontrib_focus +
                                            code_comment_focus +
                                            issue_comment_focus +
                                            rel_persistent + 
                                            rel_extensive | releases_control_cat
                                          ,
                                          data = oss.releases,
                                          EM = T,
                                          dist = "negbin")
  
  
    m.releases.zinbr.standard.5 <- zeroinfl(releases ~
                                            rel_high_reputation +
                                            rel_experienced +
                                            issue_focus +
                                            techcontrib_focus +
                                            code_comment_focus +
                                            issue_comment_focus +
                                            rel_persistent + 
                                            rel_extensive +
                                            releases_control_cat +
                                            proj_age +
                                            proj_size | releases_control_cat
                                          ,
                                          data = oss.releases,
                                          dist = "negbin")
  
  #' ZINBR 5
  #' controlling for project size and previous releases
  #'
  m.releases.zinbr.standard.6 <- zeroinfl(releases ~
                                            rel_high_reputation +
                                            rel_experienced +
                                            techcontrib_focus +
                                            code_comment_focus +
                                            issue_comment_focus +
                                            rel_persistent + 
                                            rel_extensive +
                                            releases_control_cat +
                                            proj_age +
                                            proj_size | releases_control_cat
                                          ,
                                          data = oss.releases,
                                          dist = "negbin")
  
  out <- list()
  out [[1]] <- m.releases.zinbr.standard.1
  out [[2]] <- m.releases.zinbr.standard.5
  out [[3]] <- m.releases.zinbr.standard.6
  out [[4]] <- m.releases.zinbr.controls
  
  m.to.table(out, 
             title = "Zero inflated release models (standard indicators, glm family: zi-negbin)", 
             m_name = "zinbr_releases_simple",
             label = "tab:res_zinbr_release_simple"
  ) 
  
  message("vuong test")
  print(vuong(nbr, m.releases.zinbr.standard.6))
  
}

#### non-core models standard ####
estimate.noncore.nbr.standard <- function(){
  
  m.noncore.nbr.controls <- glm.nb(no_non_core ~ releases_control_cat +
                                     proj_age +
                                     proj_size, 
                                   data = oss.noncore, 
                                   control = param.glm.control)
  
  form.s = no_non_core ~
    rel_high_reputation +
    rel_experienced +
    issue_focus +
    techcontrib_focus +
    code_comment_focus +
    issue_comment_focus +
    rel_persistent + 
    rel_extensive
  
  #' Negative Binomial GLM
  #' controls: 
  #' - none
  #'
  form = form.s
  m.noncore.nbr.s.1<- glm.nb(form, data = oss.noncore, control = param.glm.control)
  
  #' Negative Binomial GLM
  #' controls: 
  #' - releases_control ***
  #'
  form = formula.add.variable(form.s, "releases_control_cat")
  m.noncore.nbr.s.2 <- glm.nb(form, data = oss.noncore,  control = param.glm.control)
  
  #' Negative Binomial GLM
  #' controls: 
  #' - project age ***
  #'
  form = formula.add.variable(form.s, c("releases_control_cat", "proj_age"))
  m.noncore.nbr.s.3 <- glm.nb(form, data = oss.noncore,  control = param.glm.control)
  
  #' Negative Binomial GLM
  #' controls: 
  #' - project size ***
  form = formula.add.variable(form.s, c("releases_control_cat", "proj_age", "proj_size"))
  m.noncore.nbr.s.4 <- glm.nb(form, data = oss.noncore, control = param.glm.control)
  
  #' Negative Binomial GLM
  #' removed issue_comment_focus
  #' 
  #' controls:
  #' - proj size
  #' - releases control
  #'
  form = formula.drop.variable(form.s, "issue_focus")
  form = formula.add.variable(form, c("proj_size", "proj_age", "releases_control_cat"))
  m.noncore.nbr.s.6 <- glm.nb(form, data = oss.noncore, control = param.glm.control)
  
  out <- list()
  out [[1]] <- m.noncore.nbr.s.1
  out [[2]] <- m.noncore.nbr.s.2
  out [[3]] <- m.noncore.nbr.s.3
  out [[4]] <- m.noncore.nbr.s.4
  out [[5]] <- m.noncore.nbr.s.6
  out [[6]] <- m.noncore.nbr.controls
  
  m.to.table(out, 
             title = "Noncore models (standard indicators, glm family: negbin)",
             m_name = "nbr_nocore_simple",
             label = "tab:res_noncore_simple",
             float.env = "sidewaystable") 
  
  message("VIF model 4")
  print(vif(m.noncore.nbr.s.4))
  }

#### ci models ####
estimate.ci.standard <- function(){
  form.s = releases ~
    rel_persistent + 
    rel_extensive + 
    code_comment_focus +
    issue_comment_focus +
    techcontrib_focus +
    rel_high_reputation +
    rel_experienced +
    has_travis
  
  
  #' Negative Binomial GLM
  #' controls: 
  #' - ci (yes/no)
  #'
  m.releases.ci.nbr.standard.1 <- glm.nb(form.s, data = oss.releases, control = param.glm.control)
  
  #' Negative Binomial GLM
  #' controls: ***
  #' - releases control
  #' - project age
  #' - project size 
  #' - ci (yes/no)
  #'
  form = formula.add.variable(form.s, c("releases_control_cat", "proj_age", "proj_size"))
  m.releases.ci.nbr.standard.2 <- glm.nb(form, data = oss.releases, control = param.glm.control)
  
  m.releases.ci.zinbr.standard.1 <- zeroinfl(releases ~
                                            rel_high_reputation +
                                            rel_experienced +
                                            techcontrib_focus +
                                            code_comment_focus +
                                            issue_comment_focus +
                                            rel_persistent + 
                                            rel_extensive +
                                            has_travis +
                                            releases_control_cat +
                                            proj_age +
                                            proj_size | releases_control_cat
                                          ,
                                          data = oss.releases,
                                          dist = "negbin")
  
  #' Negative Binomial GLM
  #' controls: ***
  #' - release control
  #' - project size 
  #' - has travis
  #' 
  form.s = no_non_core ~
  rel_persistent + 
    rel_extensive + 
    code_comment_focus +
    issue_comment_focus +
    techcontrib_focus +
    rel_high_reputation +
    rel_experienced +
    has_travis
  form = formula.add.variable(form.s, c("releases_control_cat", "proj_size", "proj_age"))
  m.noncore.ci.nbr.standard.4 <- glm.nb(form, data = oss.noncore, control = param.glm.control)
  
  out <- list()
  out [[1]] <- m.releases.ci.nbr.standard.1
  out [[2]] <- m.releases.ci.nbr.standard.2
  out [[3]] <- m.releases.ci.zinbr.standard.1
  out [[4]] <- m.noncore.ci.nbr.standard.4

  m.to.table(out, 
             title = "Controlling for CI (Standard indicators)", 
             m_name = "ci_standard",
             label = "tab:res_ci_models"
  ) 
}


#### Blau Index models ####
estimate.blau <- function(){
  
  m.releases.nbr.controls <- glm.nb(releases ~ releases_control_cat +
                                      proj_age +
                                      proj_size, 
                                    data = oss.releases, 
                                    control = param.glm.control)
  
  form.s = releases ~
    rel_high_reputation +
    rel_experienced_blau +
    issue_focus_blau +
    techcontrib_focus_blau +
    code_comment_focus_blau +
    rel_persistent_blau + 
    rel_extensive_blau 
  
  #' Negative Binomial GLM
  #' controls: 
  #' - none
  #'
  m.releases.nbr.blau.1 <- glm.nb(form.s, data = oss.releases, control = param.glm.control)
  
  
  #' Negative Binomial GLM
  #' controls: 
  #' - project size ***
  #'
  form = formula.add.variable(form.s, c("releases_control_cat", "proj_age", "proj_size"))
  m.releases.nbr.blau.4 <- glm.nb(form, data = oss.releases, control = param.glm.control)

  
  
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
                                      data = oss.releases,
                                      EM = T,
                                      dist = "negbin",
                                      control = zeroinfl.control(method ="BFGS", maxit = 10000, trace = F))
  
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
                                        releases_control_cat +
                                        proj_age +
                                        proj_size | releases_control_cat
                                      ,
                                      data = oss.releases,
                                      dist = "negbin")

  m.noncore.nbr.controls <- glm.nb(no_non_core ~ 
                                     releases_control_cat +
                                     proj_age +
                                     proj_size, 
                                   data = oss.noncore, 
                                   control = param.glm.control)
  
  form.s = no_non_core ~
    rel_high_reputation +
    rel_experienced_blau +
    issue_focus_blau +
    techcontrib_focus_blau +
    code_comment_focus_blau +
    rel_persistent_blau + 
    rel_extensive_blau 
  
  m.noncore.nbr.blau.1 <- glm.nb(form.s, data = oss.noncore, control = param.glm.control)
  
  form = formula.add.variable(form.s, c("releases_control_cat", "proj_age", "proj_size"))
  m.noncore.nbr.blau.2 <- glm.nb(form, data = oss.noncore, control = param.glm.control)
  
  out <- list()
  out [[1]] <- m.releases.nbr.blau.1
  out [[2]] <- m.releases.nbr.blau.4
  
  out [[3]] <- m.noncore.nbr.blau.1
  out [[4]] <- m.noncore.nbr.blau.2

  m.to.table(out, 
             title = "NBR using blau scores for variety diversity faultlines", 
             m_name = "nbr_blau",
             label = "tab:res_blau"
  ) 
}

#### main ####
main <- function(){
  # clear environment
  message("clearing global environment...")
  rm(list = ls(envir = .GlobalEnv))
  
  oss.all <<- import.variables()
  oss.releases <<- outliers.remove.releases(oss.all)
  oss.noncore <<- outliers.remove.noncore(oss.all)
  
  independents <- get_independents(oss.all)
  dependents <- get_dependents(oss.all)
  
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
                                               issue_comment_focus_blau, 
                                               issue_focus_blau, 
                                               rel_experienced_blau,
                                               rel_persistent_blau,
                                               code_comment_focus_blau,
                                               techcontrib_focus_blau,
                                               modularity,
                                               no_subgroups,
                                               rel_extensive_blau
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
  
  nbr <- estimate.releases.nbr.standard()
  estimate.releases.zinbr.standard(nbr)
  
  estimate.noncore.nbr.standard()
  estimate.ci.standard()
  
  estimate.blau()
  }


main()