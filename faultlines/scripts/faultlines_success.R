
.libPaths(c(.libPaths(), '/home/rahnm/R/lib'))
library(RNeo4j)
library(dplyr)
library(futile.logger)
library(lubridate)
library(reshape2)
library(data.table)



neo = startGraph("http://localhost:7474/db/data/",
                 username = "max",
                 password = "1111")

flog.threshold(INFO)
flog.appender(appender.file('/home/rahnm/R/log/faultlines_success.log'))

# lenght of one standard period in days
param.analysis.period.length = 30 

# number of days for which operationalizations should be calculated
param.analysis.ops.period_length = 180

# number of days for which success measures should be retrieved
param.analysis.success.period_lenght  = 90

# number of days to spare between ops and success measures
param.analysis.success.time_lag = 0

# number of contributions per day that must be made on average 
# such that a developer belongs to the dev_core
param.analysis.dev_core.min = 20/180

#' retrieves all project names as list
#'   @return list of project logins
get_project_names <- function() {
    query = sprintf(
      "
      MATCH (o:OWNER)
      WHERE EXISTS ((o) <-- (:GHA_REPO))
      RETURN DISTINCT o.login as project;
      "
    )
    
    df = cypher(neo, query)
  
  return(df)
}


#' get date of first contribution to the project
#'
#'
get_proj_start <- function(p){
  query_start_proj = sprintf(
    "
  MATCH (o:OWNER{login: '%s'})
  WITH o
  MATCH (node:COMMENT)-[:to]->()-[:to]->()-[:belongs_to]->(o)
  WITH o, node.event_time as c_time ORDER BY c_time ASC LIMIT 1
  
  MATCH (node)-[:to]->()-[:belongs_to]->(o)
  WHERE (node:PULLREQUEST OR node:ISSUE OR node:COMMIT)
  WITH c_time, node.event_time as t_time ORDER BY t_time ASC LIMIT 1
  WITH COLLECT(c_time)+COLLECT(t_time) as times
  UNWIND times as r
  RETURN apoc.date.format(min(r), 'ms', 'yyyy-MM-dd');
  ", 
  p
  )
  
  proj_start = ymd(cypher(neo, query_start_proj))
  
  return(proj_start)
}


#' get date of last contribution to project
#'
#'
get_proj_end <- function(p){
  query_end_proj = sprintf(
    "
  MATCH (o:OWNER{login: '%s'})
  WITH o
  MATCH (node:COMMENT)-[:to]->()-[:to]->()-[:belongs_to]->(o)
  WITH o, node.event_time as c_time ORDER BY c_time DESC LIMIT 1
  
  MATCH (node)-[:to]->()-[:belongs_to]->(o)
  WHERE (node:PULLREQUEST OR node:ISSUE OR node:COMMIT)
  WITH c_time, node.event_time as t_time ORDER BY t_time DESC LIMIT 1
  WITH COLLECT(c_time)+COLLECT(t_time) as times
  UNWIND times as r
  RETURN apoc.date.format(min(r), 'ms', 'yyyy-MM-dd');
  ",
  p
  )
  
  proj_start = ymd(cypher(neo, query_end_proj))
  
  return(proj_start)
}


#' return the analysis period
#'
get_analysis_period <- function(p){
  p_start = ymd(get_proj_start(p))
  p_end = ymd(get_proj_end(p))
  
  analysis_mid = p_start + days(ceiling(interval(p_start, p_end)/days(1)/2))
  analysis_start = analysis_mid - days(param.analysis.ops.period_length/2)
  analysis_end = analysis_mid + days(param.analysis.ops.period_length/2)
  
  return(interval(analysis_start, analysis_end))
}


#' returns project age in no. of periods
#'
#'
get_proj_age <- function(proj_start){
  
}

#' returns a count of how many core and non core developers participate in the project
#' dev_core criteria:
#' developer has more than x contributions during total project lifetime
get_dev_core <- function(p, s_period){
    start = ymd(int_start(s_period))
    end = ymd(int_end(s_period))
  
    query = sprintf("
                    MATCH (o:OWNER{login: '%s'})
                    WITH o,
                      apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as start,
                      apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as end
                    // comments
                    MATCH (u:USER)-[:makes]->(node:COMMENT)-[:to]->()-[:to]->()-[:belongs_to]->(o)
                    WHERE node.event_time >= start AND 
                          node.event_time <= end
                    WITH start, end, o, COLLECT({u_id: u.gha_id, n_id: id(node)}) as comments
                    
                    // technicals
                    MATCH (u:USER)-->(node) -[:to]-> () -[:belongs_to]-> (o)
                    WHERE
                      (node:PULLREQUEST OR node:ISSUE OR node:COMMIT) AND
                      node.event_time >= start AND 
                      node.event_time <= end
                    WITH comments + COLLECT({u_id: u.gha_id, n_id: id(node)}) as allContributions
                    UNWIND allContributions as row
                    
                    RETURN row.u_id as u_id, COUNT(DISTINCT row.n_id) as count_contributions;
                    ",
                    p,
                    start,
                    end
                    
    )
    
  df = cypher(neo, query)
  
  if(!is.null(df)){
    df$core_member <- df$count_contributions/param.analysis.success.period_lenght >= param.analysis.dev_core.min  
    no_core = sum(df$core_member)
    no_non_core = nrow(df) - no_core
    ratio_core = no_core/nrow(df)
  } else {
    no_core = 0
    no_non_core = 0
    ratio_core = NA
  }
  
  result = data.frame(  
    no_core = no_core,
    no_non_core = no_non_core,
    ratio_core = ratio_core
  )
  
  return(result)
}

#' 
#'
#'
get_success_period <- function(a_start){
  a_start = ymd(a_start)
  
  s_start = a_start + 
    days(param.analysis.ops.period_length) +
    days(param.analysis.success.time_lag)
  
  s_end = a_start + 
    days(param.analysis.ops.period_length + 
           param.analysis.success.period_lenght)
  
  return(interval(s_start, s_end))
  
}

#' gets the number of releases
#' @param p project name
#' @param a_start date of the analysis period start
get_releases <- function(p, s_period){
  start = ymd(int_start(s_period))
  end = ymd(int_end(s_period))
  
  query = sprintf(
    "
      MATCH (o:OWNER{login:'%s'})
    WITH o, 
      apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as start,
      apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as end
    MATCH (r:RELEASE) -[:to]->()-[:belongs_to]->(o:OWNER)
    WITH r WHERE r.event_time >= start AND r.event_time <= end
    RETURN COUNT(DISTINCT r) as releases
    ",
    p,
    start,
    end
    )
  
  releases = cypher(neo, query)
  return(releases)
}


#' 
#'
#'
assemble <- function(p, releases, dev_core){
  df = data.frame(project = p,
                  releases = releases)
  
  df$releases[is.na(df$releases)] <- 0 # NA means project has no release, therefore set 0
  
  df = cbind(df, dev_core)
  
  return(df)
}


#'
#'
#'
main <- function(){
  projects <- get_project_names()
  
  result = list()
  i = 1
  
  # projects <- data.frame(projects = projects[1:10,])
  
  for (p in projects$project){
    res = tryCatch({
      analysis_interval <- get_analysis_period(p)
      
      success_interval <- get_success_period(ymd(int_start(analysis_interval)))
      
      releases <- get_releases(p, success_interval)
      dev_core <- get_dev_core(p, success_interval)
      
      res = assemble(p, releases, dev_core)
    },
    
    error = function(err){
      print(err)
      print(p)
      res = NULL
      return(res)
    })

    if(!is.null(res)){
      result[[i]] = res
      print(paste("projects handled: ", round(i/nrow(projects)*100, digits = 2), "%"))
      i = i + 1
    }
  }
  
  success = rbindlist(result)
  
  file.path = paste("/home/rahnm/R/analysis/faultlines/model/success.csv",
                    sep = "")
  write.csv(success, file = file.path, row.names = F)
  
}

main()