
.libPaths(c(.libPaths(), '/home/rahnm/R/lib'))
library(RNeo4j)
library(dplyr)
library(igraph)
library(car)
library(futile.logger)
library(lubridate)

neo = startGraph("http://localhost:7474/db/data/",
                 username = "max",
                 password = "1111")

flog.threshold(INFO)
flog.appender(appender.file('/home/rahnm/R/log/faultlines_success.log'))


#' retrieves all project names as list
#'   @return list of owner logins
get_project_names <- function() {
    query = sprintf(
      "
      MATCH (o:OWNER)
      WHERE EXISTS ((o) <-- (:GHA_REPO))
      RETURN DISTINCT o.login as names;
      "
    )
    
    df = cypher(neo, query)
  
  return(df)
}


#' get date of first contribution to the project
#'
#'
get_proj_start <- function(owner){
  query_start_proj = 
    "
  MATCH (o:OWNER{login: 'waffleio'})
  WITH o
  MATCH (node:COMMENT)-[:to]->()-[:to]->()-[:belongs_to]->(o)
  WITH o, node.event_time as c_time ORDER BY c_time ASC LIMIT 1
  
  MATCH (node)-[:to]->()-[:belongs_to]->(o)
  WHERE (node:PULLREQUEST OR node:ISSUE OR node:COMMIT)
  WITH c_time, node.event_time as t_time ORDER BY t_time ASC LIMIT 1
  WITH COLLECT(c_time)+COLLECT(t_time) as times
  UNWIND times as r
  RETURN apoc.date.format(min(r), 'ms', 'yyyy-MM-dd');
  "
  proj_start = ymd(cypher(neo, query_start_proj))
  
  return(proj_start)
}


#' gets the number of releases at project end
#'
#'
get_releases <- function(){
  query = 
    "
    MATCH (r:RELEASE) -[:to]->()-[:belongs_to]->(o:OWNER)
    RETURN o.login as names, COUNT(DISTINCT r) as no_releases
    "
  
  releases = cypher(neo, query)
  return(releases)
}

#' 
#'
#'
assemble <- function(names, releases){
  result <- merge(x = names, y = releases, by = 'names', all.x = T)
  result[is.na(result)] <- 0 # NA means project has no release, therefore set 0
  
  return(result)
}

#'
#'
#'
main <- function(){
  names <- get_project_names()
  releases <- get_releases()
  
  success = assemble(names, releases)
  
  file.path = paste("/home/rahnm/R/analysis/faultlines/model/success.csv",
                    sep = "")
  write.csv(success, file = file.path)
  
}