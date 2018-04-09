library(ggplot2)
library(reshape2)


param.plot.ops = F
param.plot.ind = T
param.plot.res = 300
param.plot.exp =  "/Users/Max/Desktop/MA/R/NetworkAnalyzer/faultlines/plots/"
param.plot.width = 12
param.plot.height = 8
param.plot.units = "cm"


old <- theme_set(theme_gray())
theme_update(axis.title = element_text(size = rel(0.65)))
theme_update(axis.text = element_text(size = rel(0.5)))

#'
#'
#'
dep.histogram <- function(df){
  dfmelt <- melt(df)
  
  p <- ggplot(dfmelt, aes(x = value)) +
    geom_histogram() +
    facet_wrap(~variable, scales = "free") +
    labs(title = "Distribution of dependent variables")
  
  save.plot(p, "histogram_dependents.png")
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
              "rel_experienced")]
  
  dfmelt <- melt(df, id = "releases")
  
  p <- ggplot(dfmelt, aes(x = value, y = dfmelt$releases)) +
    facet_wrap(~variable, ncol = 4) +
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
              "rel_experienced")]
  
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
import.dependents <- function(){
  dependents <- read.csv("/Users/Max/Desktop/MA/R/NetworkAnalyzer/faultlines/analysis/faultlines/model/success.csv")
  return(dependents)
}

main <- function(){
  
  dependents <- import.dependents()
  dep.histogram(dependents)
  
  
  oss = import.variables()
  
  generate.facet.releases(oss)
  generate.facet.core_ratio(oss)
  generate.facet.no_core(oss)
  generate.facet.no_non_core(oss)
  
}

main()