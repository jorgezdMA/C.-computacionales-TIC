
library(grid)
library(gridExtra)
library(webshot)
library(png)
library(visNetwork)
library(tidyverse)

#
#---- Preparacion de paradas ----
#


load("basis/paradas_y_aristas_trole.RData")

nodes <- Stop %>% slice(1:5) %>% 
  mutate(id = `stop-id`, label = `short-name`, title = `short-name`, x = `x-coordinate`, 
         y = `y-coordinate` , color="#D35400") %>%
  select(id, label, title, x, y,color)

# Generar matriz de diseño
layout_matrix <- as.matrix(nodes[, c("x", "y")])

# Graficar usando visNetwork
antes <- visNetwork(nodes, data.frame(from = integer(0), to = integer(0))) %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = F) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix) 


load("Trole ida y vuelta ptn/paradas_y_aristas_trole_dirigido.RData")

colnames(edges)<- c("link_index", "Origen", "Destino", "length", "lower_bound", "upper_bound")
# Crear nodos
nodes <- Stop %>%
  mutate(id = `stop-id`, label = `short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`,
         color="#D35400" , shape="dot") %>%
  select(id, label, title, x, y,color,shape)

# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(edges$Origen),
                      to = c(edges$Destino),
                      #label= as.character(edges$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#2E4053", nrow(edges))))


layout_matrix <- as.matrix(nodes[, c("x", "y")]) * scale_factor
# Graficar usando visNetwork con ajustes adicionales para una imagen más compacta
visNetwork(nodes, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot", borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = TRUE, type = "curvedCW", roundness = 0.1), arrows = "to") %>%
  visPhysics(stabilization = FALSE,
             barnesHut = list(avoidOverlap = 1, gravitationalConstant = -2000, centralGravity = 0.3, springLength = 100, springConstant = 0.05)) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = F) %>%
  visLayout(randomSeed = 88) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)
#+++++++ Ecovia +++++++++

load("basis2/paradas_y_aristas_ecovia.RData")

colnames(edges)<- c("link_index", "Origen", "Destino", "length", "lower_bound", "upper_bound")

nodes <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`,color="#D35400") %>%
  select(id, label, title, x, y,color)
# Definir las conexiones entre nodes
aristas <- data.frame(from = c(edges$Origen),
                      to = c(edges$Destino),
                      #label= as.character(edges$link_index),
                      font = list(size = 17, color = "black"),
                      color = "#2E4053")



layout_matrix <- as.matrix(nodes[, c("x", "y")])
# Graficar usando visNetwork
visNetwork(nodes, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = F) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)





load("Ecovia ida y vuelta ptn/paradas_y_aristas_ecovia_dirigida.RData")

colnames(edges)<- c("link_index", "Origen", "Destino", "length", "lower_bound", "upper_bound")

nodes <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`,color="#D35400") %>%
  select(id, label, title, x, y,color)
# Definir las conexiones entre nodes
aristas <- data.frame(from = c(edges$Origen),
                      to = c(edges$Destino),
                      #label= as.character(edges$link_index),
                      font = list(size = 17, color = "black"),
                      color = "#2E4053")



layout_matrix <- as.matrix(nodes[, c("x", "y")])
# Graficar usando visNetwork
visNetwork(nodes, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot", borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = TRUE, type = "curvedCW", roundness = 0.1), arrows = "to") %>%
  visPhysics(stabilization = FALSE,
             barnesHut = list(avoidOverlap = 1, gravitationalConstant = -2000, centralGravity = 0.3, springLength = 100, springConstant = 0.05)) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = F) %>%
  visLayout(randomSeed = 88) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)



load("basis integrada/paradas_y_aristas_integrado.RData")

colnames(edges)<- c("link_index", "Origen", "Destino", "length", "lower_bound", "upper_bound")

nodes <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`,color="#D35400") %>%
  select(id, label, title, x, y,color)
# Definir las conexiones entre nodes
aristas <- data.frame(from = c(edges$Origen),
                      to = c(edges$Destino),
                      #label= as.character(edges$link_index),
                      font = list(size = 17, color = "black"),
                      color = "#2E4053")



layout_matrix <- as.matrix(nodes[, c("x", "y")])
# Graficar usando visNetwork
visNetwork(nodes, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot", borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = TRUE, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE,
             barnesHut = list(avoidOverlap = 1, gravitationalConstant = -2000, centralGravity = 0.3, springLength = 100, springConstant = 0.05)) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = F) %>%
  visLayout(randomSeed = 88) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)



