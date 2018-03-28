# Script calculates variable operationalizations per project and saves them 
# to csv files separately

.libPaths(c(.libPaths(), '/home/rahnm/R/lib'))
library(RNeo4j)
library(igraph)
library(car)
library(futile.logger)

neo = startGraph("http://localhost:7474/db/data/",
                 username = "max",
                 password = "1111")

flog.threshold(INFO)
flog.appender(appender.file('/home/rahnm/R/log/faultlines.log'))

param.analysis.all = F
param.analysis.single = 'waffleio'

param.analysis.dt_start = "2014-01-01"
param.analysis.dt_end = "2017-08-01"

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
    
    MATCH (node:COMMENT)-[:to]->()-[:to]->(:REPO)-[:belongs_to]->(o)
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
  ops.get <- function(owner, dt_start, dt_end) {
    query = sprintf(
      "
      MATCH (o:OWNER{login: '%s'})
      WITH o,
      apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as start,
      apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as end
      
      MATCH (node) -[:to]-> (:REPO) -[:belongs_to]-> (o)
      WHERE (node:PULLREQUEST OR node:ISSUE OR node:COMMIT) AND node.event_time >= start AND node.event_time <= end
      
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
    return(df)
  }
  
  df = ops.get(owner, dt_start, dt_end)
  
  df[is.na(df)] <- 0
  
  df["total_technicals"] =
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
#'                      pullreq_comments    no of pullrequ comments made by the user
#'                      commit_comments     no of commit comments made by the user
get_count_comment_types <- function(owner, dt_start, dt_end) {
  query = sprintf(
    "
    MATCH (o:OWNER{login: '%s'})
    WITH 
    o,
    apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as start,
    apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as end

    MATCH (node:COMMENT)-[:to]->()-[:to]->(:REPO)-[:belongs_to]->(o)
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
  
  df = cypher(neo, query)
  
  df["total_comments"] =
    df$issue_comments +
    df$pullreq_comments +
    df$commit_comments
  
  return(df)
}


#' calculates variable operationalizations
#' 1. Collects comment and technicals count
#' 2. merges counts together based on the developers' ids
#' 3. fills NAs with 0, since NAs occur when a developer has not made a contribution of a specific type
#' 4. calculate ratios
#'    i.    code vs. issue related activity
#'    ii.   relative code vs. issue related activity
#'    iii.  code contribution vs code reviewing
#'    iv.   relative code contribution vs code reviewing
#'    v.    issue reporting vs issue discussing
#'    vi.   relative issue reporting vs issue discussing
#'    vii.  technical contribution vs discussion
#'    viii. relaltive technical contribution vs discussion
#'    
#'   @param owner:      specific project
#'   @param dt_start:   start date
#'   @param dt_end:     end date
#'   @return operationalization data frame
ops.calculate <- function(owner, dt_start, dt_end) {
  tech = get_count_technicals(owner, dt_start, dt_end)
  comment_types = get_count_comment_types(owner, dt_start, dt_end)
  
  df <- merge(x = tech,
              y = comment_types,
              by = "gha_id",
              all = TRUE)
  
  df[is.na(df)] <- 0
  
  # i.
  # ratio: code vs issue related activity
  # share of issue related activity in total activity per user
  df[, "ratio_code_issue"] =
    (
      (df$no_issues_reported + df$issue_comments) 
      / 
      (df$total_technicals + df$total_comments)
    )
  
  # ii.
  # ratio: relative activity focus
  # share of issue related activity in total activity per user in relation to the overall project
  # ratio of issue to total activity.
  df[, "ratio_rel_code_issue"] =
    (
      (
        (df$no_issues_reported + df$issue_comments) 
        / 
        (df$total_technicals + df$total_comments)
      )
      -
      (
        sum(df$no_issues_reported + df$issue_comments) 
        / 
        sum(df$total_technicals + df$total_comments)
      )
    )
  
  # iii.
  # ratio: code contributing vs code reviewing
  df[, "ratio_code_review_contribution"] =
    (
      (df$commit_comments + df$pullreq_comments) 
      /
      (df$commit_comments + df$pullreq_comments + 
         df$no_pullrequests_requested + df$no_commits_committed)
    )
  
  # iv.
  # ratio: relative code contributions vs reviews
  df[, "ratio_rel_code_review_contribution"] =
    (
      (
        (df$commit_comments + df$pullreq_comments) 
        /
        (df$commit_comments + df$pullreq_comments + 
           df$no_pullrequests_requested + df$no_commits_committed)
      )
      -
      (
        sum(df$commit_comments + df$pullreq_comments)
        /
        sum(df$commit_comments + df$pullreq_comments + 
              df$no_pullrequests_requested + df$no_commits_committed)
      )
    )
  
  # v.
  # ratio: issue reporting vs issue discussing
  df[, "ratio_issue_reports_discussion"] =
    (df$issue_comments / (df$issue_comments + df$no_issues_reported))
  
  # vi.
  # ratio: relative issue reporting vs issue discussing
  df[, "ratio_rel_issue_reports_discussion"] =
    (
      (
        df$issue_comments 
        / 
        (df$issue_comments + df$no_issues_reported)
      )
      -
      (
        sum(df$issue_comments) 
        / 
        sum(df$issue_comments + df$no_issues_reported)
      )
    )
  
  
  # vii.
  # ratio: technical contribution vs discussion
  df[, "ratio_technical_discussion"] =
    (
      df$total_technicals 
      / 
      (df$total_technicals + df$total_comments)
    )
  
  # viii.
  # ratio: relaltive technical contribution vs discussion
  df[, "ratio_rel_technical_discussion"] =
    (
      (
        df$total_technicals 
        / 
        (df$total_technicals + df$total_comments)
      )
      -
      (
        sum(df$total_technicals) 
        / 
        sum(df$total_technicals + df$total_comments)
      )
    )
  
  return(df)
}

#' joins operationalization df with groups
#' those users who did not contribute or have not been referenced by any comments will not be clustered into groups,
#' since the clustering is based on comment relations. These users will be removed in the curse of the merging process
#'   @param op:     operationalization data frame
#'   @param c:      igraph communities object
#'   @return merged data frame including group information per user
join_ops_df_with_groups <- function(ops, c, g) {
  com <- cbind(V(g)$name, c$membership)
  com <- setNames(as.data.frame(com), c("gha_id", "group"))
  
  df = merge(x = ops, y = com, by = "gha_id", all.y = T)
  return (df)
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
    # create directed graph from comment relations
    g = graph_from_data_frame(comments, directed = F)
    
    # cluster the graph
    c = cluster_louvain(g)
    
    # create operationalizations table
    ops = ops.calculate(owner, dt_start, dt_end)
    
    # add group information from louvain clustering
    ops = join_ops_df_with_groups(ops, c, g)
  } else {
    ops = NULL
  }
  return(ops)
}

#' saves an operationalizations csv file per project
main <- function () {

  
  projects <- get_project_names()
  
  for (project in projects$names) {
    tryCatch({
      ops <- ops.get(project, 
                     param.analysis.dt_start, 
                     param.analysis.dt_end)
    },
    
    error = function(cond) {
      message("Error occurred while processing")
      message(project)
      message(cond)
      return(NULL)
    })
    
    if (!is.null(ops)) {
      file.path.var = paste("/home/rahnm/R/analysis/faultlines/variation/op_df_",
                            project,
                            ".csv",
                            sep = "")
      write.csv(ops, file = file.path.var)
    }
  }
}

ftry(main(), error = warning)
