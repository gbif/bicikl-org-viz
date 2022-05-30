# POC: Visualizing network of relationships between organizations

library(igraph)    # network library

library(tidyverse) # tidy style

library(sigmajs) # visualize network: http://sigmajs.john-coene.com/

library(skimr) # describe data

library(readr) # read data

library(abbreviate) # to get unique abbreviations

library(threejs) # another visualization library: https://bwlewis.github.io/rthreejs/

# There are many possible choices for visualization libraries besides the ones
# tested here: visNetwork, gggraph, networkD3, ggnet, statnet, among others  

# Read data
# Example with LifeWatch ERIC (organisation 1717, abbreviated as it LifeWa)
org_1717_1 <- read_csv("data/org_1717.1.csv")
org_1717_2 <- read_csv("data/org_1717.2.csv")
org_list <- read_csv("data/org_list.csv")

full_org_list <- read_csv("~/extendo/gbif-fairsharing/data/full_org_list.csv")

# There seems to be more names than abbr for y ....?
org_1717_2 %>%
  skim()

# Get the nodes or vertices  of the network
nodes <- org_1717_2 %>% 
  group_by(organisation_id.y) %>% 
  tally() %>% 
  mutate(id = organisation_id.y,
         size = n) %>% 
  select(id,size)

# Get the names of the organizations
nodes <- nodes %>% 
  left_join(full_org_list)

# Abbreviate names and used them as labels
nodes$abbr <-abbreviate_text(nodes$name,minlength = 5)
nodes <- nodes %>% 
  rename(label = abbr)

# Get the edges

edges <- org_1717_2 %>% 
  mutate(from = organisation_id.x,
         to = organisation_id.y) %>% 
  select(from,to)


# create the network object
network <- graph_from_data_frame(d=edges, directed=F, vertices = nodes) 

# See vertices
V(network)

# See edges
E(network)


# Plot using igraph with tinkering too much
plot(network)

# It is better to use something better to visualize, like Sigma.js

# It seems that there are two cluster detected so
color_pal2 <- rainbow(2, alpha=.5) 


sigmajs() %>%
  sg_from_igraph(network) %>%
  #sg_settings(drawLabels = TRUE, drawEdgeLabels = FALSE) %>% 
  sg_layout(layout = igraph::layout_nicely) %>%
  sg_cluster(colors = hcl.colors(10,"Set 2"))  %>%
  sg_settings(
    minNodeSize = 1,
    maxNodeSize = 5.0,
    edgeColor = "default",
    defaultEdgeColor = "#d3d3d3",
    labelThreshold = 3
  ) %>%
  sg_neighbours()


# If using igraph

color_pal3 <- rainbow(3, alpha=.3) 

x_comun <- edge.betweenness.community(network)
i <- membership(x_comun)
g <- set_vertex_attr(network, "color", value = color_pal3[i])
plot(g)

# If using threejs
graphjs(g)


