#### libraries ####
library(ggplot2)
library(reshape2)
library(car)
library(MASS)
library(stargazer)

#### parameters ####
param.plot.facets = F
param.plot.correlation = F
param.plot.hist = F
param.models.estimate = T
param.glm.control = glm.control(epsilon = 1e-8, maxit = 100, trace = FALSE)

param.m.add_const_to_releases = F # adding a constant would violate distribution assumption, leave F
param.m.transform.blau = T
param.m.transform.controls = T

param.plot.res = 300
param.plot.exp =  "/Users/Max/Desktop/MA/R/NetworkAnalyzer/faultlines/plots/"
param.plot.width = 12
param.plot.height = 12
param.plot.units = "cm"


old <- theme_set(theme_gray())
theme_update(axis.title = element_text(size = rel(0.65)))
theme_update(axis.text = element_text(size = rel(0.5)))

#### import ####

#' imports all variables (dependents and independents)
#'
#'
import.variables <- function(){
  # read independent variables
  independents <- read.csv("/Users/Max/Desktop/MA/R/NetworkAnalyzer/faultlines/analysis/faultlines/model/independents.csv")
  
  # read dependent variables
  dependents <- read.csv("/Users/Max/Desktop/MA/R/NetworkAnalyzer/faultlines/analysis/faultlines/model/success.csv")
  
  # merge
  res = merge(x = independents, y = dependents, by = "project")
  
  return(res)
}

#'
#'
#'
get_dependents <- function(oss){
  dependents = oss[, c("project", 
                       "ratio_core",
                       "no_core",
                       "no_non_core",
                       "releases")]
  return(dependents)
}

#'
#'
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
                         # is NA for some reason  "proj_age",
                         "proj_size"
  )]
  return(independents)
}

#### plots ####
#'
#'
#'
hist.dep <- function(df){
  dfmelt <- melt(df)
  
  p <- ggplot(dfmelt, aes(x = value)) +
    geom_histogram() +
    facet_wrap(~variable, scales = "free") +
    labs(title = "Distribution of dependent variables")
  
  save.plot(p, "models/histogram_dependents.png")
  
  df <- log(df + 1)
  dfmelt <- melt(df)
  
  p <- ggplot(dfmelt, aes(x = value)) +
    geom_histogram() +
    facet_wrap(~variable, scales = "free") +
    labs(title = "Distribution of dependent variables", subtitle = "log-transformed")
  
  save.plot(p, "models/histogram_dependents_log.png")
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
  
  save.plot(p, "models/histogram_independents.png")
  
  p <- ggplot(dfmelt2, aes(x = value)) +
    geom_histogram() +
    facet_wrap(~variable, scales = "free") +
    labs(title = "Distribution of independent variables",
         subtitle = "blau-transformed")
  
  save.plot(p, "models/histogram_independents_blau.png")
  
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

### transformations ###

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
  
  save.plot(p, "models/histogram_blau_transf.png")
  
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
  save.plot(p, "models/histogram_controls.png")
  
  # oss$releases_control <- as.factor(oss$releases_control > 0)
  
  oss$proj_size <- (oss$proj_size + 1)
  # oss$proj_age <- log(oss$proj_age + 1)
  
  dfmelt <- melt(oss[, c("releases_control",
                         "proj_size",
                         "proj_age")])
  
  p <- ggplot(dfmelt, aes(x = value)) +
    geom_histogram() +
    facet_wrap(~variable, scales = "free") +
    labs(title = "Distribution of control variables",
         subtitle = "controls log-transformed")
  save.plot(p, "models/histogram_controls_log.png")
  
  return(oss)
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
    m_name = paste(m_name, "ctf", sep = "_")
    title = paste(title, " - log(controls)")
  }
  
  stargazer(m_list, 
            type = "text", 
            title = title,
            add.lines = list(p_d),
            out = paste(param.plot.exp, "models/", m_name, ".txt", sep = ""))
}

#'
#'
#'
pearson.dispersion <- function(m){
  
  return(round(sum(residuals(m, type = "pearson")^2)/m$df.residual, digits = 3))
  
}

#### release models ####
estimate.releases.glmnb.blau <- function(oss){

  if(param.m.add_const_to_releases){
    oss$releases <- oss$releases + 1
    oss$releases_control <- oss$releases_control + 1
  }
  
  #' Model specification:
  #' Negative Binomial GLM
  #' 
  #' dependent variable: 
  #' - releases + 1 ***
  #' 
  #' predictors: 
  #' - all faultine ratios (variety diversity indicators were blau transformed!)
  #' 
  #' controls: 
  #' - none
  #'
  m.releases.glmnb.blau.2 <- function(oss){
    m <- glm.nb((releases) ~
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
    
    # process_model(m, "releases_glmnb_3", oss)
    
    step <- stepAIC(m, direction="both")
    return(list(m, step))
  }
  
  #' Model specification:
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
  #'
  m.releases.glmnb.blau.3 <- function(oss){
    m <- glm.nb((releases) ~
                  rel_persistent_blau + 
                  rel_extensive_blau + 
                  issue_focus_blau +
                  code_comment_focus_blau +
                  issue_comment_focus_blau +
                  techcontrib_focus_blau +
                  rel_high_reputation +
                  rel_experienced_blau +
                  (releases_control)
                ,
                data = oss,
                control = param.glm.control)
    
    # process_model(m, "releases_glmnb_3", oss)
    
    step <- stepAIC(m, direction="both")
    return(list(m, step))
  }
  
  #' Model specification:
  #' Negative Binomial GLM
  #' 
  #' 
  #' dependent variable: 
  #' - releases + 1
  #' 
  #' predictors: 
  #' - all faultine ratios (variety diversity indicators were blau transformed!)
  #' 
  #' controls: 
  #' - project age ***
  #'
  m.releases.glmnb.blau.4 <- function(oss){
    m <- glm.nb((releases) ~
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
    
    step <- stepAIC(m, direction="both")
    return(list(m, step))
  }
  
  #' Model specification:
  #' Negative Binomial GLM
  #' 
  #' 
  #' dependent variable: 
  #' - releases + 1
  #' 
  #' predictors: 
  #' - all faultine ratios (variety diversity indicators were blau transformed!)
  #' 
  #' controls: 
  #' - project size ***
  #'
  m.releases.glmnb.blau.5 <- function(oss){
    m <- glm.nb((releases) ~
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
    
    # process_model(m, "releases_glmnb_3", oss)
    
    step <- stepAIC(m, direction="both")
    return(list(m, step))
  }
  
  #' Model specification:
  #' Negative Binomial GLM
  #' 
  #' 
  #' dependent variable: 
  #' - releases + 1 ***
  #' 
  #' predictors: 
  #' - all faultine ratios (variety diversity indicators were blau transformed!)
  #' 
  #' controls: ***
  #' - project age
  #' - releases control + 1
  #' - project size 
  #'
  m.releases.glmnb.blau.6 <- function(oss){
    m <- glm.nb((releases) ~
                  rel_persistent_blau + 
                  rel_extensive_blau + 
                  issue_focus_blau +
                  code_comment_focus_blau +
                  issue_comment_focus_blau +
                  techcontrib_focus_blau +
                  rel_high_reputation +
                  rel_experienced_blau +
                  (releases_control) +
                  proj_size
#                  + proj_age
                ,
                data = oss,
                control = param.glm.control)
    
    step <- stepAIC(m, direction="both")
    return(list(m, step))
  }
  
  out <- m.releases.glmnb.blau.2(oss)    # only predictors
  out <- append(out, list(m.releases.glmnb.blau.3(oss)[[1]]))    # 
#  out <- append(out, list(m.releases.glmnb.blau.4(oss)[[1]]))    # 
  out <- append(out, list(m.releases.glmnb.blau.5(oss)[[1]]))    # 
  out <- append(out, m.releases.glmnb.blau.6(oss))    # all shares and controls
  
    m.to.table(out, 
             title = "Release models", 
             m_name = "glmnb_releases_blau"
             ) 
  
}

estimate.releases.glmnb.standard <- function(oss){
  const = 0
  if(param.m.add_const_to_releases){
    const = 1
  }
  
  #' Model specification:
  #' Negative Binomial GLM
  #' 
  #' dependent variable: 
  #' - releases + 1 ***
  #' 
  #' predictors: 
  #' - all faultine ratios
  #' 
  #' controls: 
  #' - none
  #'
  m.releases.glmnb.standard.2 <- function(oss){
    m <- glm.nb((releases) ~
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
    
    # process_model(m, "releases_glmnb_3", oss)
    
    step <- stepAIC(m, direction="both")
    return(list(m, step))
  }
  
  #' Model specification:
  #' Negative Binomial GLM
  #' 
  #' dependent variable: 
  #' - releases + 1
  #' 
  #' predictors: 
  #' - all faultine ratios
  #' 
  #' controls: 
  #' - releases_control + 1 ***
  #'
  m.releases.glmnb.standard.3 <- function(oss){
    m <- glm.nb((releases) ~
                  rel_persistent + 
                  rel_extensive + 
                  issue_focus +
                  code_comment_focus +
                  issue_comment_focus +
                  techcontrib_focus +
                  rel_high_reputation +
                  rel_experienced +
                  (releases_control)
                ,
                data = oss,
                control = param.glm.control)
    
    # process_model(m, "releases_glmnb_3", oss)
    
    step <- stepAIC(m, direction="both")
    return(list(m, step))
  }
  
  #' Model specification:
  #' Negative Binomial GLM
  #' 
  #' 
  #' dependent variable: 
  #' - releases + 1
  #' 
  #' predictors: 
  #' - all faultine ratios
  #' 
  #' controls: 
  #' - project age ***
  #'
  m.releases.glmnb.standard.4 <- function(oss){
    m <- glm.nb((releases) ~
                  rel_persistent + 
                  rel_extensive + 
                  issue_focus +
                  code_comment_focus +
                  issue_comment_focus +
                  techcontrib_focus +
                  rel_high_reputation +
                  rel_experienced
#                 + proj_age
                ,
                data = oss,
                control = param.glm.control)
    
    step <- stepAIC(m, direction="both")
    return(list(m, step))
  }
  
  #' Model specification:
  #' Negative Binomial GLM
  #' 
  #' dependent variable: 
  #' - releases + 1
  #' 
  #' predictors: 
  #' - all faultine ratios
  #' 
  #' controls: 
  #' - project size ***
  #'
  m.releases.glmnb.standard.5 <- function(oss){
    m <- glm.nb((releases) ~
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
    
    step <- stepAIC(m, direction="both")
    return(list(m, step))
  }
  
  #' Model specification:
  #' Negative Binomial GLM
  #' 
  #' dependent variable: 
  #' - releases + 1
  #' 
  #' predictors: 
  #' - all faultine ratios
  #' 
  #' controls:  ***
  #' - project age
  #' - proj size
  #' - releases control
  #'
  m.releases.glmnb.standard.6 <- function(oss){
    m <- glm.nb((releases) ~
                  rel_persistent + 
                  rel_extensive + 
                  issue_focus +
                  code_comment_focus +
                  issue_comment_focus +
                  techcontrib_focus +
                  rel_high_reputation +
                  rel_experienced +
                  (releases_control) +
#                  proj_age +
                  proj_size
                ,
                data = oss,
                control = param.glm.control)
    
    step <- stepAIC(m, direction="both")
    return(list(m, step))
  }
  
  out <- m.releases.glmnb.standard.2(oss)    # only predictors on (releases+1)
  out <- append(out, list(m.releases.glmnb.standard.3(oss)[[1]]))    # include releases_control
#  out <- append(out, list(m.releases.glmnb.standard.4(oss)[[1]]))    # include proj_age
  out <- append(out, list(m.releases.glmnb.standard.5(oss)[[1]]))    # include proj_size
  out <- append(out, m.releases.glmnb.standard.6(oss))    # all shares and controls
  
  m.to.table(out, 
             title = "Release models (standard indicators)",
             m_name = "glmnb_releases_standard") 
}

#### non-core models ####
estimate.noncore.glmnb.blau <- function(oss){
  
  #' Model specification:
  #' Negative Binomial GLM
  #' 
  #' dependent variable: 
  #' - number of non-core contributors ***
  #'  
  #' predictors: 
  #' - all faultine ratios (variety diversity indicators were blau transformed!)
  #' 
  #' controls: 
  #' - project size ***
  #'
  m.non_core.glmnb.blau.1 <- function(oss){
    m <- glm.nb((no_non_core) ~
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
    
    # process_model(m, "releases_glmnb_3", oss)
    
    step <- stepAIC(m, direction="both")
    return(list(m, step))
  }
  
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
  #' - project age ***
  #'
  m.non_core.glmnb.blau.2 <- function(oss){
    m <- glm.nb((no_non_core) ~
                  rel_persistent_blau + 
                  rel_extensive_blau + 
                  issue_focus_blau +
                  code_comment_focus_blau +
                  issue_comment_focus_blau +
                  techcontrib_focus_blau +
                  rel_high_reputation +
                  rel_experienced_blau
#                 + proj_age
                ,
                data = oss,
                control = param.glm.control)
    
    step <- stepAIC(m, direction="both")
    return(list(m, step))
  }
  
  
  #' Model specification:
  #' Negative Binomial GLM
  #' 
  #' dependent variable: 
  #' - number of non-core contributors 
  #' 
  #' predictors: 
  #' - all faultine ratios (variety diversity indicators were blau transformed!)
  #' 
  #' controls: ***
  #' - project age
  #' - project size 
  #'
  m.non_core.glmnb.blau.3 <- function(oss){
    m <- glm.nb((no_non_core) ~
                  rel_persistent_blau + 
                  rel_extensive_blau + 
                  issue_focus_blau +
                  code_comment_focus_blau +
                  issue_comment_focus_blau +
                  techcontrib_focus_blau +
                  rel_high_reputation +
                  rel_experienced_blau +
                  proj_size
#                 + proj_age
                ,
                data = oss,
                control = param.glm.control)
    
    p_disp <- pearson.dispersion(m)
    
    step <- stepAIC(m, direction="both")
    return(list(m, step))
  }
  
#  out <- m.non_core.glmnb.blau.1(oss)    # only predictors
#  out <- append(out, list(m.non_core.glmnb.blau.2(oss)[[1]]))    # 
#  out <- append(out, m.non_core.glmnb.blau.3(oss))        # incl. controls
  out <-  m.non_core.glmnb.blau.3(oss)
  
  
  m_name = "glmnb_non_core_blau"
    
  m.to.table(out, 
             title = "Community engagement models", 
             m_name = m_name
  ) 
  
}


#### main ####

main <- function(){
  oss = import.variables()
  independents <- get_independents(oss)
  dependents <- get_dependents(oss)
  
  # plot distributions
  if(param.plot.hist){
    hist.dep(dependents[, -1])
    hist.indep(independents[, -1])
  }
  
  if(param.plot.correlation){
    # create correlation matrices
    c.heatmap(independents[, -1], 
              title = "Pearson correlation",
              subtitle = "independent variables",
              file.path = "models/cor_independents.png")
    
    c.heatmap(independents[, c("issue_focus", "issue_comment_focus", "code_comment_focus", "techcontrib_focus")], 
              title = "Pearson correlation",
              subtitle = "subgroup shares",
              file.path = "models/cor_subgroup_shares.png",
              reorder = F)
    
    c.heatmap(dependents[, -1],
              title = "Pearson correlation",
              subtitle = "dependent variables",
              file.path = "models/cor_dependents.png")
    
    c.heatmap(oss[, -1],
              title = "Pearson correlation",
              subtitle = "all variables",
              file.path = "models/cor_all.png",
              reorder = F)
  }
  
  if(param.plot.facets){ 
    plot.facets(oss)
  }
  
  if(param.m.transform.blau){
    oss <- transform.blau(oss)
  }
  
  if(param.m.transform.controls){
    oss = transform.controls(oss)
  }
  
  # estimate some models
  if(param.models.estimate){
    
    if(T){
      estimate.releases.glmnb.standard(oss)
      estimate.releases.glmnb.blau(oss)
      
      # estimate.releases.zinb.blau/standard(oss) -> read about how to compare it here: https://statisticalhorizons.com/zero-inflated-models
    }
    
    if(T){
      estimate.noncore.glmnb.blau(oss)
    }
    
  }
}

main()