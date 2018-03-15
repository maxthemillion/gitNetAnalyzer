# install.packages(RNeo4j)
library(RNeo4j)
library(igraph)
library(car)
library(multcomp)



# retrieves the comment subgraph per project
get_comment_subgraph <- function(owner, dt_start, dt_end) {
  query = sprintf(
    "
    MATCH (o:OWNER{login:'%s'})
    WITH o,
    apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as start,
    apoc.date.parse('%s', 'ms','yyyy-MM-dd') as end
    
    CALL ga.timetree.events.range({start: start, end: end}) YIELD node
    WITH o,
    node
    WHERE (node: COMMENT) --> (:ISSUE) --> (:GHA_REPO) --> (o)
    OR (node: COMMENT) --> (:PULLREQUEST) --> (:GHA_REPO) --> (o)
    OR (node: COMMENT) --> (:COMMIT) --> (:GHT_REPO) --> (o)
    
    MATCH (source:USER) --> (node)
    MATCH p = (node) --> (target:USER)
    
    WITH source,
    target,
    p,
    (CASE WHEN 'I_COMMENT' in labels(node) THEN 'issue'
    WHEN 'PR_COMMENT' in labels(node) THEN 'pullreq'
    WHEN 'C_COMMENT' in labels(node) THEN  'commit'
    ELSE 'no_label' END) as type
    
    RETURN
    source.gha_id as source,
    target.gha_id as target,
    type,
    count(p) as weight;
    ORDER BY
    source,
    target,
    type
    ",
    owner,
    dt_start,
    dt_end
  )
  
  
  df = cypher(neo, query)
  return(df)
}


# returns count of technical contributions per type and user to a specific project
# only those are considered, who also contributed comments to the project
# table format:
#   gha_id                  identifies the user
#   no_issues_reported      no of issues reported by the user to the specified project
#   no_pullreq_requested    no of pullrequests requested by the user to the specified project
#   no_commits_committed    no of commits commmitted by the user to the specified project

get_count_technicals <- function(owner, dt_start, dt_end) {
  # retrieves count of issues and pullrequest contributions to a project per user
  get_count_ip <- function(owner, dt_start, dt_end) {
    query = sprintf(
      "
      MATCH (o:OWNER{login: '%s'})
      WITH o,
      apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as start,
      apoc.date.parse('%s', 'ms','yyyy-MM-dd') as end
      
      CALL ga.timetree.events.range({start: start, end: end})
      YIELD node
      WITH o,
      node
      WHERE (node:ISSUE) -[:to]-> (:GHA_REPO) --> (o) OR
      (node:PULLREQUEST) -[:to]-> (:GHA_REPO) --> (o)
      
      MATCH (source:USER) --> (node)
      
      WITH COLLECT(node) as technicals, source.gha_id as gha_id
      
      RETURN gha_id,
      SIZE(FILTER(node in technicals WHERE 'ISSUE' in labels(node))) as no_issues_reported,
      SIZE(FILTER(node in technicals WHERE 'PULLREQUEST' in labels(node))) as no_pullrequests_requested;
      ",
      owner,
      dt_start,
      dt_end
    )
    
    df = cypher(neo, query)
    return(df)
  }
  
  # retrieves commit count per user to a certain project
  get_count_c <- function(owner, dt_start, dt_end) {
    query = sprintf(
      "MATCH (o:OWNER{login: '%s'})
      WITH o,
      apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as start,
      apoc.date.parse('%s', 'ms','yyyy-MM-dd') as end
      
      CALL ga.timetree.events.range({start: start, end: end})
      YIELD node
      WITH o,
      node
      
      WHERE (node:COMMIT) -[:to]-> (:GHT_REPO) --> (o)
      MATCH (source:USER) <-[:is]- (:GHT_USER) -[:authored]-> (node)
      
      WITH COLLECT(node) as technicals, source.gha_id as gha_id
      RETURN gha_id,
      SIZE(FILTER(node in technicals WHERE 'COMMIT' in labels(node))) as no_commits_committed;"
      ,
      owner,
      dt_start,
      dt_end
    )
    
    
    df = cypher(neo, query)
    return(df)
  }
  
  df1 = get_count_ip(owner, dt_start, dt_end)
  df2 = get_count_c(owner, dt_start, dt_end)
  
  df = merge(x = df1,
             y = df2,
             by = "gha_id",
             all = TRUE)
  
  df["total_technicals"] =
    df$no_issues_reported +
    df$no_pullreq_requested +
    df$no_commits_committed
  
  return(df)
}


# returns the number of issue and pull request comments per type and user to a specific project
# table format:
#   gha_id              id identifies user
#   issue_comments      no. of issue comments made by the user
#   pullreq_comments    no of pullrequ comments made by the user
#   commit_comments     no of commit comments made by the user

get_comment_types_ip <- function(owner, dt_start, dt_end) {
  query = sprintf(
    "
    MATCH (o:OWNER{login: '%s'})
    WITH apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as start,
    apoc.date.parse('%s', 'ms','yyyy-MM-dd') as end
    CALL ga.timetree.events.range({start: start, end: end}) YIELD node
    WITH o, node
    WHERE  (node:COMMENT)-[:to]->(x:ISSUE)-[:to]->(r:GHA_REPO)-->(o) OR
    (node:COMMENT)-[:to]->(x:PULLREQUEST)-[:to]->(r:GHA_REPO)-->(o) OR
    (node:COMMENT)-[:to]->(x:COMMIT)-[:to]->(r:GHT_REPO)-->(o)
    MATCH (u:USER)-->(node)
    
    WITH u, COLLECT(DISTINCT node) as all
    RETURN u.gha_id as gha_id,
    SIZE(FILTER(node in all WHERE 'I_COMMENT' in labels(node))) as issue_comments,
    SIZE(FILTER(node in all WHERE 'PR_COMMENT' in labels(node))) as pullreq_comments,
    SIZE(FILTER(node in all WHERE 'C_COMMENT' in labels(node))) as commit_comments,
    ",
    dt_start,
    dt_end,
    owner
  )
  df = cypher(neo, query)
  
  df["total_comments"] =
    df$issue_comments +
    df$pullreq_comments +
    df$commit_comments
  
  return(df)
}

# calculate variable operationalizations and put them into a dataframe
create_op_df <- function(dt_start, dt_end) {
  tech = get_technicals(owner, dt_start, dt_end)
  comment_types = get_comment_types(owner, dt_start, dt_end)
  
  df <- merge(x = tech,
              y = comment_types,
              by = "gha_id",
              all = TRUE)
  
  df[is.na(df)] <- 0
  df[, "c_i_ratio"] = df[, "no_commits_committed"] / (df[, "total_technical"])
  return(df)
}

# clusters the given graph using the louvain algorithm
cluster <- function(graph) {
  # transforming to undirected
  g_u = graph_from_data_frame(comments[, c("source", "target", "weight")])
  g_u = as.undirected(g_u, mode = "mutual", edge.attr.comb = "sum")
  c = cluster_louvain(g_u)
  
  return(c)
}

# joins   operationalization df with groups
# op:     operationalization data frame
# c:      group data frame
join_op_df_with_groups <- function(op, c) {
  com <- cbind(V(g_d)$name, c$membership)
  com <- setNames(as.data.frame(com), c("gha_id", "group"))
  
  df = merge(x = op, y = com, by = "gha_id")
  
  return (df)
}


get_data <- function(owner, dt_start, dt_end) {
  # get comment subgraph
  comments = get_comment_subgraph(owner, dt_start, dt_end)
  
  # create directed graph from comment relations
  g_d = graph_from_data_frame(comments)
  
  # cluster the graph
  c = cluster(g_d)
  
  # create operationalizations table
  op = create_op_df(dt_start, dt_end)
  
  # add group information from louvain clustering
  op = join_op_df_with_groups(op, c)
  
  return(op)
}


# performs an anova for the specified variable and tests anova assumptions
# parameters:
#   op      operationalization dataframe
#   var     column name of the variable to analyze
# returns: -
perform_anova <- function(op, var) {
  # -- ANOVA --
  # http://www.sthda.com/english/wiki/one-way-anova-test-in-r#what-is-one-way-anova-test
  # estimate anova on various operationalizations
  res.aov.c_i_ratio <- aov(c_i_ratio ~ group, data = op)
  #...
  
  # since we do not know, which groups differ, calculate Tukey Honest Significant Differences
  TukeyHSD(res.aov.c_i_ratio)
  #...
  
  # further comparisons can be made with package 'multcomp'
  # https://cran.r-project.org/web/packages/multcomp/multcomp.pdf
  
  # check assumptions of ANOVA
  # 1. homogeieity of variance
  setwd('/home/rahnm/R/plots')
  png('plot_aov_homogeneity.png')
  plot(res.aov, 1)
  dev.off()
  
  leveneTest(c_i_ratio ~ group, data = op)
  
  # 2. normality assumption
  setwd('/home/rahnm/R/plots')
  png('plot_aov_normality.png')
  plot(res.aov, 2)
  dev.off()
  
  # shapiro wilk test   
  aov_residuals <- residuals(object = res.aov.c_i_ratio )
  shapiro.test(x = aov_residuals )
  
}


# performs a kruskal test for the specified variable
# parameters:
#     op      operationalization data frame
#     var     column name of the variable to analyze
# returns: -
perform_kruskal <- function (op, var){
  # -- Kruskal Test --
  # to be used, when assumptions of one-way anova are not met
  # http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
  # estimate kruskal test on various operationalizations
  res.kruskal.c_i_ratio <- kruskal.test(c_i_ratio ~ group, data = op) 
  #...
  
  # since we do not know, which groups differ, calculate pairwise wilcox test
  pairwise.wilcox.test(op$c_i_ratio, op$group, p.adjust.method = "BH")
  #...
  
}
  
main <- function() {
  neo = startGraph("http://localhost:7474/db/data/",
                   username = "max",
                   password = "1111")
  
  dt_start = "2017-01-01"
  dt_end = "2017-02-01"
  owner = "OneDrive"
  
  op <- get_data(owner, dt_start, dt_end)
  perform_anova(op, 'c_i_ratio')
  perform_kruskal(op, 'c_i_ratio')
}


main()
