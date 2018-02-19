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

get_technicals <- function(owner, dt_start, dt_end){
  query = sprintf("
                  WITH apoc.date.parse('%s', 'ms', 'yyyy-MM-dd') as start,
                  apoc.date.parse('%s', 'ms','yyyy-MM-dd') as end
                  CALL ga.timetree.events.range({start: start, end: end}) YIELD node
                  MATCH (u:USER)-->(node)-->(:REPO)-->(o:OWNER)
                  WHERE o.name = '%s' and node:ISSUE or node:COMMIT

                  WITH u.login as login, COLLECT(node) as all
                  RETURN login, 
                  SIZE(all) as total_technical,
                  SIZE(FILTER(node in all WHERE labels(node)[0] ='ISSUE')) as no_issues_reported,
                  SIZE(FILTER(node in all WHERE labels(node)[0] ='COMMIT')) as no_commits_committed
                  ORDER BY login
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
                  SIZE(all) as total_comments, 
                  SIZE(FILTER(node in all WHERE node.thread_type = 'issue' )) as issue_comments,
                  SIZE(FILTER(node in all WHERE node.thread_type = 'pullreq')) as pullreq_comments,
                  SIZE(FILTER(node in all WHERE node.thread_type = 'commit')) as commit_comments
                  ", dt_start, dt_end, owner)
  df = cypher(neo, query)
  return(df)
}

# calculate variable operationalizations and put them into a dataframe
create_ot <- function(dt_start, dt_end){
  tech = get_technicals(owner, dt_start, dt_end)
  comment_types = get_comment_types(owner, dt_start, dt_end)
  
  df <- merge(x = tech, y = comment_types, by = "login", all = TRUE)
  
  df[is.na(df)] <- 0
  df[,"c_i_ratio"] = df[,"no_commits_committed"]/(df[,"total_technical"])
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
  com <- cbind(V(g_d)$name,c$membership)
  com <- setNames(as.data.frame(com), c("login", "group"))
  
  df = merge(x=ot, y=com, by="login")
  
  return (df)
}

# get comments from neo4j
comments = get_comment_subgraph(owner, dt_start, dt_end)

# create directed graph from comment relations
g_d = graph_from_data_frame(comments)

# cluster the graph
c = cluster(g_d)

# create operationalizations table
ot = create_ot(dt_start, dt_end)
ot = join_ot_with_groups(ot, c)

