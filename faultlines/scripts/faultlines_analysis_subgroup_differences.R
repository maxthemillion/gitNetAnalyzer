#### libraries ####
library(data.table)
library(plyr)
library(ggplot2)
library(gridExtra)
library(metap)


#### parameters ####
param.analysis.groups.minsize = 2             # min group size which qualifies for further analysis

param.dataset = "sp180_c10" # can be "sp180_c10", "sp180_c20", "sp180_c50" or "sp90_c20" 

param.plot.res = 300
param.plot.width = 12
param.plot.height = 12
param.plot.units = "cm"

param.path.root = "/Users/Max/Desktop/MA/R/NetworkAnalyzer/faultlines/"
param.ops.in = paste(param.path.root, "data/variation/", param.dataset, "/ops_all.csv", sep = "")
param.table.out =  paste(param.path.root, "/analysis/", param.dataset, "/variation/tables/", sep = "")
param.plot.out = paste(param.path.root, "analysis/", param.dataset, "/variation/plots/", sep = "")

#### import ####
#' import operationalization csv file
#'
#' @param f:  project name
#' @return operationalizations data frame including group information
ops.import <- function(f) {

  ops <- read.csv(param.ops.in)

  return(ops)
}

#' selects groups according to project size
#' @param s     scalar of type numeric. Determines the minimum group size
#' 
ops.groups.select <- function(ops, s){
  
  select <- function(x, group.selection){
    
    y = group.selection[group.selection["project"] == x[1] ,]
    
    return(as.numeric(x[2]) %in% y$group)
  }
  
  group.count = plyr::ddply(ops, c("project", "group"), summarize, count=length(unique(gha_id)))

  L = group.count$count >= s
  group.selection = group.count[L ,]
  
  ops.L = apply(ops[, c("project", "group")], 1, function(x) select(x, group.selection))

  ops.new = ops[ops.L ,]
  
  return(ops.new)
}

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


subgroup.test <- function(sg){
  out <- tryCatch(
    {
      r = data.frame(p_persistency_sd = kruskal.test(sg$persistency_sd ~ sg$group)$p.value,
                     p_extent_sd = kruskal.test(sg$contribution_extent_sd ~ sg$group)$p.value,
                     p_proj_experience_sd = kruskal.test(sg$proj_experience_sd ~ sg$group)$p.value,
                     p_proximity_prestige_sd = kruskal.test(sg$proximity_prestige_sd ~ sg$group)$p.value,
                     p_ratio_issue_reports_discussion_sd = kruskal.test(sg$ratio_issue_reports_discussion_sd ~ sg$group)$p.value,
                     p_ratio_code_issue_sd = kruskal.test(sg$ratio_code_issue_sd ~ sg$group)$p.value,
                     p_ratio_code_review_contribution_sd = kruskal.test(sg$ratio_code_review_contribution_sd ~ sg$group)$p.value,
                     p_ratio_technical_discussion_sd = kruskal.test(sg$ratio_technical_discussion_sd ~ sg$group)$p.value
      ) 
    },
    error=function(cond) {
#      message(cond)
      r <- data.frame(p_persistency_sd = NA,
                      p_extent_sd = NA,
                      p_proj_experience_sd = NA,
                      p_proximity_prestige_sd = NA,
                      p_ratio_issue_reports_discussion_sd = NA,
                      p_ratio_code_issue_sd = NA,
                      p_ratio_code_review_contribution_sd = NA,
                      p_ratio_technical_discussion_sd = NA)
      return(r)
    }
  )    
  return(out)
}

#'
#'
#'
fishers.method <- function(x){
  res = sumlog(x)
  return(data.frame(variable = names(x), 
                    chisq = res$chisq, 
                    p = format.pval(res$p, digits = 3)
                    ))
}

#'
#'
#'
kruskal.shares <- function(x){
  s = sum(x <= 0.05, na.rm = T)
  is = sum(!(x<= 0.05), na.rm = T)
  
  return(data.frame(
    variable = names(x),
    no_significant = s,
    share = round(s/(s+is), digits = 3)
  ))
}

main <- function(){
  
  ops <- ops.import()
  
  ops.sel <- ops.groups.select(ops, s = param.analysis.groups.minsize)
  
  test.res <- ddply(ops.sel, "project", function(x) subgroup.test(x)) 
  
  #' summarizing the p-values using Fisher's combined probability test
  #' - H0: all the underlying H0-hypotheses are true
  #' - H1: at least one of the underlying H1-hypotheses are true
  #'
  sum.res <- adply(test.res[!is.na(test.res$p_persistency_sd),-1], 
                   2, 
                   function(x) fishers.method(x),
                   .id = NULL)
  
  test.shares <- adply(test.res[,-1],
                       2, 
                       function(x) kruskal.shares(x), 
                       .id = NULL)
  
  stargazer(merge(sum.res, test.shares, by = "variable"), 
            type = "text",
            title = "Aggregated results of Kruskal-Wallis test for subgroup differences",
            add.lines = c(paste("Number of significant tests per variable (p = .05, g_min = ", 
                          param.analysis.groups.minsize,
                          ")"), 
                          sep = ""),
            out = paste(param.table.out, "subgroup_differences_res.txt", sep = ""), 
            summary = F,
            initial.zero = F)
}

main()
