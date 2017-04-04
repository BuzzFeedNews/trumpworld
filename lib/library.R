plotNet <- function (graph, file) {
  edge_betweenness <- 
    graph %>% 
    edge_betweenness(directed = FALSE) %>% 
    scale(center = FALSE) %>% 
    as.vector
  edges <-
    graph %>% 
    as_edgelist %>% 
    data.frame %>% 
    rename(source = X1,
           target = X2) %>% 
    mutate(source = as.numeric(source) - 1,
           target = as.numeric(target) - 1,
           value = edge_betweenness + 1)
  community <- 
    graph %>% 
    edge.betweenness.community %>% 
    membership %>% 
    as.vector
  vertices <- 
    graph %>% 
    vertex_attr("id") %>% 
    data.frame %>% 
    mutate(group = community)
  names(vertices) <- c("label", "group")
  N <- forceNetwork(Links = edges, 
                    Nodes = vertices,
                    Source = "source", 
                    Target = "target", 
                    Value = "value",
                    NodeID = "label", 
                    Group = "group",
                    fontSize = 16, fontFamily = "sans-serif", zoom = TRUE)
  N %>% saveNetwork(file)
  file.info(file)
}
