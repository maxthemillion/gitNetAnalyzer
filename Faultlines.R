# install.packages(RNeo4j)
library(RNeo4j)
library(igraph)

neo = startGraph("http://localhost:7474/db/data/", 
                   username = "max", 
                   password = "1111")

dt_start = "2015-10-01"
dt_end = "2016-10-01"
owner = "Homebrew"

get_comment_subgraph <- function(owner, dt_start, dt_end) {
  query = sprintf("
                  WITH apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as start,
                  apoc.date.parse('%s', 'ms','yyyy-MM-dd') as end
                  CALL ga.timetree.events.range({start: start, end: end}) YIELD node
                  
                  WITH node
                  MATCH(r: REPO)-->(o: OWNER{name: '%s'})
                  WHERE(node: COMMENT)-->(r)
                  
                  WITH DISTINCT node as comment
                  MATCH (source:USER) --> (comment)
                  MATCH p = (comment) --> (target:USER)
                  RETURN source.login as source, target.login as target, comment.thread_type as type, count(p) as weight ORDER BY source, target, type
                  ", dt_start, dt_end, owner)
  df = cypher(neo, query)
  return(df)
}

get_commits <- function(owner, dt_start, dt_end){
  query = sprintf("
                  WITH apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as start,
                  apoc.date.parse('%s', 'ms','yyyy-MM-dd') as end
                  CALL ga.timetree.events.range({start: start, end: end}) YIELD node
                  
                  WITH node
                  MATCH(r: REPO)-->(o: OWNER{name: '%s'})
                  WHERE(node: COMMIT)-->(r)
                  
                  WITH DISTINCT node as commit
                  MATCH p = (u:USER) --> (commit)
                  RETURN u.login as login, count(p) as no_commits ORDER BY login
                  ", dt_start, dt_end, owner)
  
  df = cypher(neo, query)
  return(df)
}

get_issues <- function(owner, dt_start, dt_end){
  query = sprintf("
                  WITH apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as start,
                  apoc.date.parse('%s', 'ms','yyyy-MM-dd') as end
                  CALL ga.timetree.events.range({start: start, end: end}) YIELD node
                  
                  WITH node
                  MATCH(r: REPO)-->(o: OWNER{name: '%s'})
                  WHERE(node: ISSUE)-->(r)
                  
                  WITH DISTINCT node as issue
                  MATCH p = (u:USER) --> (issue)
                  RETURN u.login as login, count(p) as no_issues ORDER BY login
                  ", dt_start, dt_end, owner)
  
  df = cypher(neo, query)
  return(df)
}

get_comment_types <- function(owner, dt_start, dt_end) {
  query = sprintf("
                  WITH apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as start,
                  apoc.date.parse('%s', 'ms','yyyy-MM-dd') as end
                  CALL ga.timetree.events.range({start: start, end: end}) YIELD node
                  MATCH (node:COMMENT)
                  MATCH (u:USER)-->(node)-->(:REPO)-->(o:OWNER)
                  WHERE o.name = '%s'
                  WITH u, COLLECT(Distinct node) as all
                  RETURN u.login as login, 
                  SIZE(all) as total, 
                  SIZE(FILTER(node in all WHERE node.thread_type = 'issue' )) as issue,
                  SIZE(FILTER(node in all WHERE node.thread_type = 'pullreq')) as pullreq,
                  SIZE(FILTER(node in all WHERE node.thread_type = 'commit')) as commit
                  ", dt_start, dt_end, owner)
  df = cypher(neo, query)
  return(df)
}

# calculate variable operationalizations and put them into a dataframe
create_ot <- function(dt_start, dt_end){
  commits = get_commits(owner, dt_start, dt_end)
  issues = get_issues(owner, dt_start, dt_end)
  comment_types = get_comment_types(owner, dt_start, dt_end)
  
  df <- merge(x = commits, y = issues, by = "login", all = TRUE)
  print(head(df))
  print(head(comment_types))
  df <- merge(x = df, y = comment_types, by = "login", all = TRUE)
  df[is.na(df)] <- 0
  df[,"c_i_ratio"] = df[,"no_commits"]/(df[,"no_issues"]+df[,"no_commits"])
  return(df)
}

cluster <- function(graph){
  
  # transforming to undirected
  g_u = graph_from_data_frame(comments[,c("source","target","weight")])
  g_u = as.undirected(g_u, mode="mutual", edge.attr.comb="sum")
  c = cluster_louvain(g_u)
  
  return(c)
}

join_ot_with_groups <- function(ot, c){
  
  
}

# get comments from neo4j
comments = get_comment_subgraph(owner, dt_start, dt_end)

# create directed graph from comment relations
g_d = graph_from_data_frame(comments)

# cluster the graph
c = cluster(g_d)

# create operationalizations table
ot = create_ot(dt_start, dt_end)


