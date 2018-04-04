# Script calculates variable operationalizations per project and saves them 
# to csv files separately

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
flog.appender(appender.file('/home/rahnm/R/log/faultlines.log'))

##### parameters start #####

## select projects to analyze
# if param.analysis.all == T, all projects in the DB are being considered
param.analysis.all = T
# name of the project to analyze, if param.analysis.all == F
param.analysis.single = 'waffleio' 

## set analysis period
param.analysis.dt_start = "2017-01-01"
param.analysis.dt_end = "2017-08-01"

## set persistency parameters
# period length in months
param.analysis.period.length = 1 
# no. of contributions. defines how many contributions must be made in a period 
# such that it counts as active
param.analysis.active.min = 1 

## set dev_core parameters
# rule for dev_core selection. If 'contributions', dev_core will be selected according to a minimum
# number of contributions. If 'collaborators', dev_core will be selected according to the collaborator
# status.
param.analysis.dev_core.rule = "contributions"

# rule for network selection. Can be 'both' or 'one' 
# If 'both', source and target need to fulfill the dev_core criteria for the connection to be considered.
# If 'one', just one of both needs to fulfill the criteria.
param.analysis.filter.rule = "both"

# number of contributions. defines how many contributions must be made in total 
# such that a developer belongs to the dev_core
param.analysis.dev_core.min = 5

#### parameters end ####

#' retrieves the comment subgraph per project
#'   @param owner:      specific project
#'   @param dt_start:   start date
#'   @param dt_end:     end date
#'   @return df:        result of the neo4j query as data frame and the following columns
#'                           source    gha_id of source node
#'                           target    gha_id of target node
#'                           weight    count of connections between source and target
get_comment_subgraph <- function(owner, dt_start, dt_end) {
  query = sprintf(
    "
    MATCH (o:OWNER{login: '%s'})
    WITH o,
    apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as start,
    apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as end
    
    MATCH (node:COMMENT)-[:to]->()-[:to]->()-[:belongs_to]->(o)
    WHERE node.event_time >= start AND node.event_time <= end
    
    WITH node
    MATCH p = (source:USER) -[:makes]-> (node) --> (target:USER)
    WHERE id(source) <> id(target)

    WITH source,
    target,
    p
    
    RETURN
    source.gha_id as source,
    target.gha_id as target,
    count(p) as weight
    "
    ,
    owner,
    dt_start,
    dt_end
  )
  
  df = cypher(neo, query)
  return(df)
}

#' returns a list of dev_core developers from the data base
#' dev_core criteria:
#'  developer has more than x contributions during total project lifetime
get_dev_core <- function(owner, proj_start){
  if(param.analysis.dev_core.rule == "contributions"){
  query = sprintf("
      MATCH (o:OWNER{login: '%s'})
      WITH o
      // comments
      MATCH (u:USER)-[:makes]->(node:COMMENT)-[:to]->()-[:to]->()-[:belongs_to]->(o)
      WITH o, COLLECT({u_id: u.gha_id, n_id: id(node)}) as comments
                  
      // technicals
      MATCH (u:USER)-->(node) -[:to]-> () -[:belongs_to]-> (o)
      WHERE
      (node:PULLREQUEST OR node:ISSUE OR node:COMMIT)
      WITH comments + COLLECT({u_id: u.gha_id, n_id: id(node)}) as allContributions
      UNWIND allContributions as row
            
      WITH row.u_id as u_id, COUNT(DISTINCT row.n_id) as count_contributions
      WHERE count_contributions >= %s
      RETURN u_id as gha_id;",
      owner,
      param.analysis.dev_core.min
      )
  } else if(param.analysis.dev_core.rule == "collaborator"){
    # TODO
    query = sprintf("
    MATCH (o:OWNER{login: '%s'})
    WITH o
    MATCH (u:USER)-[:becomes]->(node:COLLABORATOR)-[:to]->(:REPO)-[:belongs_to]->(o)
                    
    RETURN DISTINCT u.gha_id as gha_id;                    
    ",
    owner)
  }
  
  dev_core = cypher(neo, query)
  return(dev_core)
}

#' returns all dev_core developers which appear in the provided data frame
#' (filters sporadic contributors)
#'
filter_dev_core <- function(df, dev_core){
  if(param.analysis.filter.rule == "both"){
    df = df[(df$source %in% dev_core$gha_id & df$target %in% dev_core$gha_id) ,]
  } else if(param.analysis.dev_core.rule == "one"){
    df = df[(df$source %in% dev_core$gha_id | df$target %in% dev_core$gha_id) ,]
  }
  return(df)
}

#' retrieves all project names as list
#'   @return list of owner logins
get_project_names <- function() {
  if (param.analysis.all) {
    query = sprintf(
      "
      MATCH (o:OWNER)
      WHERE EXISTS ((o) <-- (:GHA_REPO))
      RETURN DISTINCT o.login as names;
      "
    )
    
    df = cypher(neo, query)
    
  } else {
    df = data.frame(names = c(param.analysis.single))
  }
  return(df)
}

#' retrieves the count of technical contributions per type and user to a specific project
#' only those users are considered, who also contributed comments to the project
#'   @param owner:      specific project
#'   @param dt_start:   start date
#'   @param dt_end:     end date
#'   @return df:        result of the neo4j query as data frame and the following columns
#'                           gha_id                  identifies the user
#'                           no_issues_reported      no of issues reported by the user to the specified project
#'                           no_pullreq_requested    no of pullrequests requested by the user to the specified project
#'                           no_commits_committed    no of commits commmitted by the user to the specified project
get_count_technicals <- function(owner, dt_start, dt_end) {
    query = sprintf(
      "
      MATCH (o:OWNER{login: '%s'})
      WITH o,
      apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as start,
      apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as end
      
      MATCH (node) -[:to]-> () -[:belongs_to]-> (o)
      WHERE 
        (node:PULLREQUEST OR node:ISSUE OR node:COMMIT) AND 
        node.event_time >= start AND 
        node.event_time <= end
      
      MATCH (source:USER) --> (node)
      
      WITH COLLECT(node) as technicals, source.gha_id as gha_id
      
      RETURN gha_id,
      SIZE(FILTER(node in technicals WHERE 'ISSUE' in labels(node))) as no_issues_reported,
      SIZE(FILTER(node in technicals WHERE 'PULLREQUEST' in labels(node))) as no_pullrequests_requested,
      SIZE(FILTER(node in technicals WHERE 'COMMIT' in labels(node))) as no_commits_committed;
      ",
      owner,
      dt_start,
      dt_end
    )
  df = cypher(neo, query)
  
  df[is.na(df)] <- 0
  
  df$total_technicals =
    df$no_issues_reported +
    df$no_pullrequests_requested +
    df$no_commits_committed
  
  return(df)
}

#' returns the number of issue, commit and pull request comments per type and user to a specific project
#'   @param owner:      specific project
#'   @param dt_start:   start date
#'   @param dt_end:     end date
#'   @return result of the neo4j query as data frame and the following columns
#'                      gha_id              id identifies user
#'                      issue_comments      no. of issue comments made by the user
#'                      pullreq_comments    no of pullrequest comments made by the user
#'                      commit_comments     no of commit comments made by the user
get_count_comment_types <- function(owner, dt_start, dt_end) {
  query = sprintf(
    "
    MATCH (o:OWNER{login: '%s'})
    WITH 
    o,
    apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as start,
    apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as end

    MATCH (node:COMMENT)-[:to]->()-[:to]->()-[:belongs_to]->(o)
    WHERE node.event_time >= start AND node.event_time <= end

    WITH node
    MATCH (u:USER)-->(node)

    WITH u, COLLECT(DISTINCT node) as all
    RETURN u.gha_id as gha_id,
    SIZE(FILTER(node in all WHERE 'I_COMMENT' in labels(node))) as issue_comments,
    SIZE(FILTER(node in all WHERE 'PR_COMMENT' in labels(node))) as pullreq_comments,
    SIZE(FILTER(node in all WHERE 'C_COMMENT' in labels(node))) as commit_comments;
    ",
    owner,
    dt_start,
    dt_end
  )
  
  df <- tryCatch({
    df = cypher(neo, query)
  },
  error = function(err){
    print(err)
    df = data.frame(issue_comments = c(0), pullreq_comments = c(0), commit_comments = c(0))
    return(df)
  })
  
  df$total_comments =
    df$issue_comments +
    df$pullreq_comments +
    df$commit_comments
  
  return(df)
}

#' returns gha_ids of those users which became collaborators previous to the current period
#' 
#'   
get_collaborator_status <- function(owner, dt_start, dt_end){
  query = sprintf(
    "
    MATCH (o:OWNER{login: '%s'})
    WITH
    o,
    apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as end
    
    MATCH (u:USER)-[:becomes]->(node:COLLABORATOR)-[:to]->(:REPO)-[:belongs_to]->(o)

    WHERE node.event_time <= end
    RETURN DISTINCT u.gha_id as gha_id;
    ",
    owner,
    dt_end
  )
  
  df = cypher(neo, query)
  
  return(df)
}

#' calculates variable operationalizations
#' 1. Collects comment and technicals count
#' 2. merges counts together based on the developers' ids
#' 3. fills NAs with 0, since NAs occur when a developer has not made a contribution of a specific type
#' 4. calculate activity ratios
#'    i.    code vs. issue related activity
#'    ii.   relative code vs. issue related activity
#'    iii.  code contribution vs code reviewing
#'    iv.   relative code contribution vs code reviewing
#'    v.    issue reporting vs issue discussing
#'    vi.   relative issue reporting vs issue discussing
#'    vii.  technical contribution vs discussion
#'    viii. relaltive technical contribution vs discussion
#' 5. calculate activity extent
#'    i.    total no. contributions in the current period
#'    ii.   share of own contributions in total project contributions
#'    
#'   @param owner:      specific project
#'   @param dt_start:   start date
#'   @param dt_end:     end date
#'   @return operationalization data frame
ops.calculate.ratios <- function(owner, dt_start, dt_end) {
  tech = tryCatch({
    tech = get_count_technicals(owner, dt_start, dt_end)
  },
  error = function(err){
    df = data.frame(no_issues_reported = c(0),
                  no_pullrequests_requested = c(0),
                  no_commits_committed = c(0))
    return(df)
  })

  comment_types = get_count_comment_types(owner, dt_start, dt_end)
  
  df <- merge(x = tech,
              y = comment_types,
              by = "gha_id",
              all = TRUE)
  
  df[is.na(df)] <- 0

  #### 4. ####
  # i.
  # ratio: code vs issue related activity
  # share of issue related activity in total activity per user
  df$ratio_code_issue =
    (
      (df$no_issues_reported + df$issue_comments) 
      / 
      (df$total_technicals + df$total_comments)
    )
  
  # ii.
  # ratio: relative activity focus
  # share of issue related activity in total activity per user in relation to the overall project
  # ratio of issue to total activity.
  df$ratio_code_issue_sd = (df$ratio_code_issue - mean(df$ratio_code_issue, na.rm = T))/ sd(df$ratio_code_issue, na.rm = T)
  
  
  # iii.
  # ratio: code contributing vs code reviewing
  df$ratio_code_review_contribution =
    (
      (df$commit_comments + df$pullreq_comments) 
      /
      (df$commit_comments + df$pullreq_comments + 
         df$no_pullrequests_requested + df$no_commits_committed)
    )
  
  # iv.
  # ratio: relative code contributions vs reviews
  df$ratio_code_review_contribution_sd = 
    (df$ratio_code_review_contribution - mean(df$ratio_code_review_contribution, na.rm = T)) / sd(df$ratio_code_review_contribution, na.rm = T)
    
  
  # v.
  # ratio: issue reporting vs issue discussing
  df$ratio_issue_reports_discussion =
    (df$issue_comments / (df$issue_comments + df$no_issues_reported))
  
  # vi.
  # ratio: relative issue reporting vs issue discussing
  df$ratio_issue_reports_discussion_sd =
    ((df$ratio_issue_reports_discussion) - mean(df$ratio_issue_reports_discussion, na.rm = T)) / sd(df$ratio_issue_reports_discussion, na.rm = T)
  
  
  # vii.
  # ratio: technical contribution vs discussion
  df$ratio_technical_discussion =
    (
      df$total_technicals 
      / 
      (df$total_technicals + df$total_comments)
    )
  
  # viii.
  # ratio: relaltive technical contribution vs discussion
  
  df$ratio_technical_discussion_sd =
    ((df$ratio_technical_discussion) - mean(df$ratio_technical_discussion, na.rm = T)) / sd(df$ratio_technical_discussion, na.rm = T)
  
  
  #### 5 ###
  # i. total no. contributions p. period
  total_contributions = df$total_technicals + df$total_comments
  
  # ii. individuals' share in total project contributions
  df$contribution_extent = total_contributions/sum(total_contributions, na.rm = T)
  
  # ii. individuals' share in total project contributions (deviation from mean measured in sd's)
  df$contribution_extent_sd = (df$contribution_extent - mean(df$contribution_extent, na.rm = T))/sd(df$contribution_extent, na.rm = T)
  
  return(df)
}


ops.calculate.network_measures <- function(graph_d){
  
  # undirected measures
  # degree_centrality = degree(graph_d, mode = "all", normalized = T)         # Wassermann (1998) p. 178 ff
  degree_centrality = strength(graph_d, mode = "all")/(vcount(graph_d)-1)
  betweenness_centrality = betweenness(graph_d, directed = F, normalized = T) # Wassermann (1998) p. 188 ff
  closeness_centrality = closeness(graph_d, mode = "all", normalized = T)     # Wassermann (1998) p. 183 ff
  
  # directed measures
  degree_prestige = strength(graph_d, mode = "in")/(vcount(graph_d)-1) # Wassermann (1998) p. 202 ff
  
  # proximity prestige : no. reachable nodes normalized by group size to average distance of reachable nodes 
  dist = distances(graph_d, mode="in", weights = NA)
  dist[is.infinite(dist)] <- 0 
  
  I = dist
  I[I>0] <- 1
  I = apply(I, 2, sum)
  
  proximity_prestige = ((I/(vcount(graph_d)-1))/(apply(dist, 2, sum)/I))
  
  return(data.frame(gha_id = V(graph_d)$name,
                    degree_centrality = degree_centrality,
                    betweenness_centrality = betweenness_centrality,
                    closeness_centrality = closeness_centrality,
                    degree_prestige = degree_prestige,
                    proximity_prestige = proximity_prestige))
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


#' get the persistency operationalization of active periods per developer
#'
#'
get_persistency <- function(owner, dt_start, dt_end, proj_start){
  
  
  # how many periods have passed since then?
  proj_time <- interval(proj_start, ymd(dt_end))
  periods_passed <- ceiling(proj_time/months(param.analysis.period.length))
  
  # get date of each users' first comment to the project
  query_1=sprintf(
    "
    MATCH (o:OWNER{login: '%s'})
    MATCH (u:USER)-[:makes]->(node:COMMENT)-[:to]->()-[:to]->()-[:belongs_to]->(o)
    WITH u.gha_id as gha_id, node.event_time as e_time ORDER BY u, e_time ASC
    RETURN gha_id, apoc.date.format(head(COLLECT(e_time)), 'ms', 'yyyy-MM-dd') as comment_min_time
    ", 
    owner)
  
  date.comment.min  = cypher(neo, query_1)
  date.comment.min$comment_min_time = ymd(date.comment.min$comment_min_time)
  
  # get date of each users' first technical contribution
  query_2 = sprintf(
    "    
    MATCH (o:OWNER{login: '%s'})
    MATCH (u:USER) --> (node) -[:to]-> () -[:belongs_to]-> (o)
    WHERE (node:PULLREQUEST OR node:ISSUE OR node:COMMIT)
    WITH u.gha_id as gha_id, node.event_time as e_time ORDER BY u, e_time ASC
    RETURN gha_id, apoc.date.format(head(COLLECT(e_time)), 'ms', 'yyyy-MM-dd') as techcontrib_min_time
    ",
    owner)
  
  date.techcontrib.min = cypher(neo, query_2)
  date.techcontrib.min$techcontrib_min_time = ymd(date.techcontrib.min$techcontrib_min_time)
  
  # select the earliest of both dates
  temp = merge(x = date.comment.min, y = date.techcontrib.min, by = 'gha_id', all = TRUE)
  date.all= data.frame(
    min_date = apply(temp[, -1], 1, FUN = min, na.rm = T),
    gha_id = temp$gha_id
    )
  
  # get start dates of periods to iterate over
  dt_e = ymd(dt_end)
  
  # get activity data since first contribution to the project
  activity = list()
  dt_ce = dt_e
  dt_cs = dt_ce - months(param.analysis.period.length) + days(1)
  while (dt_ce > proj_start) {
    # calculate the number of active periods since the users' first contribution 
  # do something
    query = sprintf(
      " 
      MATCH (o:OWNER{login: '%s'})
      WITH o,
      apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as end,
      apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as start
      
      // comments
      MATCH (u:USER)-[:makes]->(node:COMMENT)-[:to]->()-[:to]->()-[:belongs_to]->(o)
      WHERE
      node.event_time >= start AND
      node.event_time <= end
      WITH o, start, end, COLLECT({u_id: u.gha_id, n_id: id(node)}) as comments
      
      // technicals
      MATCH (u:USER)-->(node) -[:to]-> () -[:belongs_to]-> (o)
      WHERE
      (node:PULLREQUEST OR node:ISSUE OR node:COMMIT) AND
      node.event_time >= start AND
      node.event_time <= end
      WITH comments + COLLECT({u_id: u.gha_id, n_id: id(node)}) as allContributions
      UNWIND allContributions as row
      
      WITH row.u_id as u_id, COUNT(DISTINCT row.n_id) as count_contributions
      WHERE count_contributions >= %s
      RETURN u_id as gha_id, 1 as active;",
      owner,
      dt_ce,
      dt_cs,
      param.analysis.active.min)
    
    str = paste(year(dt_ce),month(dt_ce), day(dt_ce), sep = "-")
    activity[[str]] = cypher(neo, query)
  
  # set dt_ce and dt_cs
  dt_ce = dt_ce - months(param.analysis.period.length)
  dt_cs = dt_ce - months(param.analysis.period.length) + days(1)
  }
  
  # merge all dataframes
  df = activity %>%
    Reduce(function(dtf1,dtf2) full_join(dtf1,dtf2,by="gha_id"), .)
  # set all NAs to 0, since NA means no activity
  df[is.na(df)] <- 0
  
  # sum over columns
  active = data.frame(
    gha_id = df$gha_id, 
    sum.active = apply(df[-1], 1, sum))
  
  # calculate the number of active periods since the users' first contribution
  temp = merge(x = active, y = date.all, by = 'gha_id', all.x = T)
  
  temp$periods_since_first = ceiling(interval(temp$min_date, ymd(dt_end))/months(param.analysis.period.length))
  
  active$persistency = temp$sum.active / temp$periods_since_first
  active$persistency_sd = (active$persistency - mean(active$persistency, na.rm = T))/sd(active$persistency, na.rm = T)
  
  # calculate the share of active periods in periods since the project start (experience)
  active$proj_experience = active$sum.active/periods_passed 
  active$proj_experience_sd = (active$proj_experience - mean(active$proj_experience))/sd(active$proj_experience, na.rm = T) 
         
return(active)
}


#' joins operationalization df with groups
#' those users who did not contribute or have not been referenced by any comments will not be clustered into groups,
#' since the clustering is based on comment relations. These users will be removed in the curse of the merging process
#'   @param op:     operationalization data frame
#'   @param c:      igraph communities object
#'   @return merged data frame including group information per user
ops.assemble <- function(communities, ratios, collabs, persistency, network_measures) {
    df = merge(x = communities, y = ratios, by = "gha_id", all.x = T)
  df = merge(x = df, y = network_measures, by = "gha_id", all.x = T)
  df[, 'collaborator_status'] = df$gha_id %in% collabs$gha_id
  df = merge(x = df, y = persistency, by = "gha_id", all.x = T)
  
  return (df)
}

#'
#'
#'
get_communities <- function(graph_u){
  communities = cluster_louvain(graph_u)  
  com <- cbind(V(graph_u)$name, communities$membership)
  com <- setNames(as.data.frame(com), c("gha_id", "group"))
  return(com)
}

#' toggles the operationalizations construction process
#' 1. retrieves comment subgraph
#' 2. creates igraph object from comment subgraph information
#' 3. clusters the graph
#' 4. creates operationalization data frame 
#'   @param owner:      specific project
#'   @param dt_start:   start date
#'   @param dt_end:     end date
#'   @return operationalizations data frame
ops.get <- function(owner, dt_start, dt_end) {
  
  # get comment subgraph
  comments = get_comment_subgraph(owner, dt_start, dt_end)

  if (is.data.frame(comments) && !nrow(comments) == 0) {
    # get the project start date
    proj_start = get_proj_start(owner)
    
    # get the dev_core team
    dev_core = get_dev_core(owner)
    
    # filter sporadic contributors and continue with dev_core team data
    comments = filter_dev_core(comments, dev_core)
    
    # create an undirected graph from comment relations for clustering
    graph_u = graph_from_data_frame(comments, directed = F)
    communities = get_communities(graph_u)
    
    # get network measures
    graph_d = graph_from_data_frame(comments, directed = T)
    network_measures = ops.calculate.network_measures(graph_d)
    
    # get ratios
    ratios = ops.calculate.ratios(owner, dt_start, dt_end)
    
    # get the contributor status
    collaborators = get_collaborator_status(owner, dt_start, dt_end)
    
    # get contribution persistency
    persistency = get_persistency(owner, dt_start, dt_end, proj_start)
    
    # merge ratios, community information, persistency and collaborator status
    res <- tryCatch({  
      ops = ops.assemble(communities = communities, 
                         ratios = ratios, 
                         collabs = collaborators,
                         persistency = persistency, 
                         network_measures = network_measures)
    },
    error = function(err){
      print(err)
      ops = NULL
      return (ops)
    })
    
    ops = res
    
  } else {
    ops = NULL
  }
  return(ops)
}

#' saves an operationalizations csv file per project
main <- function () {

  skip = 140
  projects <- get_project_names()
  if(!is.na(skip)){
    projects = projects[skip+1:length(projects)]
  }
  
  for (project in projects$names) {
      ops <- ops.get(project, 
                     param.analysis.dt_start, 
                     param.analysis.dt_end)
    
    if (!is.null(ops)) {
      file.path.var = paste("/home/rahnm/R/analysis/faultlines/variation/op_df_",
                            project,
                            ".csv",
                            sep = "")
      write.csv(ops, file = file.path.var)
    }
  }
}

# ftry(main())
main()