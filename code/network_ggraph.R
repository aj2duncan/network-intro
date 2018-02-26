# ---------------------------
# ---------------------------
# Visualising complexity attendees
# Bipartite explorer
# ---------------------------
# ---------------------------

library(tidyverse)
library(RColorBrewer)
library(igraph)
library(ggraph)

df = read_csv("data/dummy_data.csv")

colnames(df) = sub("Skills", "", colnames(df))
df = select(df, Name, R, Python, Excel, SQL, Spatial, D3)

df_edges = df %>%
   gather(key = skill, value = level, -Name) %>% 
   filter(level != "I don't use it")

users = data_frame(Name = unique(df_edges$Name), 
                   num_users = 1,
                   user_software = "user",
                   type = TRUE) # for bipartite graph

software = df_edges %>% 
  count(skill) %>%
  rename(Name = skill, num_users = n) %>%
  mutate(user_software = "software",
         type = FALSE) # for bipartite graph

df_vertices = bind_rows(software, users)

gg_graph = graph.data.frame(df_edges, df_vertices, directed = FALSE)

ggraph(gg_graph, layout = "kk") +
  geom_edge_link(aes(width = level)) + 
  geom_node_point(aes(size = num_users, col = user_software)) +
  geom_node_text(aes(label = sub("Person ", "", name))) +
  scale_edge_width_manual(values = c(0.1, 0.9, 0.5))

ggraph(gg_graph, layout = "bipartite") +
  geom_edge_link() +
  geom_node_point(aes(col = user_software)) +
  geom_node_text(aes(label = sub("Person [0-9]{,2}", "", name)), 
                 nudge_y = 0.05)
                 