
.libPaths(c(.libPaths(), '/home/rahnm/R/lib'))
library(RNeo4j)
library(plyr)
library(ggplot2)

neo = startGraph("http://localhost:7474/db/data/",
                 username = "max",
                 password = "1111")

old <- theme_set(theme_gray())
theme_update(axis.title = element_text(size = rel(0.65)))
theme_update(axis.text = element_text(size = rel(0.5)))

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

get_contribution_count <- function(owner){
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
                RETURN row.u_id as u_id, COUNT(row.n_id) as ct_contrib;",
                owner)

df = cypher(neo, query)

return(df)
}

pnames = get_project_names()

df = data.frame(u_id = NA, ct_contrib = NA)
i = 1
for (name in pnames$names){
  t = get_contribution_count(name)
  df = rbind(df, t)
  print(i)
  i = i + 1
}

ct = count(df, 'ct_contrib')

png(
  filename = "/home/rahnm/R/plots/frequ_contrib.png",
  res = 300,
  width = 12,
  height = 12,
  units = 'cm'
)
p <- ggplot(ct, aes(y = freq, x = ct_contrib)) +
      geom_point(na.rm = TRUE, alpha = 1/10, size = 1) +
      scale_y_log10() +
      scale_x_log10() +
      ylab("frequency (scale: log10)") +
      xlab("contribution count per developer (scale: log10)")+
      labs(title = "Frequency of contribution count\nper developer") +
      geom_vline(aes(xintercept = 10), color = "red", linetype= "dashed")+
      theme(panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"))
print(p)
dev.off()
