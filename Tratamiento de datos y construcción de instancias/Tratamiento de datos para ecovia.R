############################################################

#                         Paradas ecovia
############################################################
library(data.table)
library(tidyverse)
library(readxl)
library(visNetwork)
paradas <-  data.table(read_excel("Excels ecovia/paradasEco.xlsx",sheet = 1))
Demand <- copy(paradas)

Stopcolname <- c("stop-id","short-name","long-name","x-coordinate","y-coordinate")
Stop <- paradas[,(Stopcolname):= .(paradas$ind,paradas$name, paradas$name, paradas$X, paradas$Y)]
Stop[, c("ind", "name","X","Y","Embarque") := NULL]

demandcolname <- c("demand-id","short-name","long-name","x-coordinate","y-coordinate","demand")
Demand <- Demand[,(demandcolname):= .(Demand$ind,Demand$name, Demand$name, Demand$X, Demand$Y,ceiling(Demand$Embarque))]

Demand[, c("ind", "name","X","Y","Embarque") := NULL]
Demand  

write.table(Stop, file = "basis ecovia/Stop.giv", sep = "; ", row.names = FALSE, quote = FALSE)
write.table(Demand, file = "basis ecovia/Demand.giv", sep = "; ", row.names = FALSE, quote = FALSE)


############################################################

#                       Matriz Od
############################################################

OD <- read_excel("Excels ecovia/paradasEco.xlsx",sheet = 2)
OD_largo_ent <- OD %>%
  pivot_longer(cols = -name, names_to = "Destino", values_to = "Valor") %>%
  rename(Origen = name)%>% mutate(Valor=ceiling(Valor))
colnames(OD_largo_ent) <- c("left-stop-id", "right-stop-id", "customers")
OD <- OD_largo_ent
indice_paradas <- setNames(Stop$`stop-id`, Stop$`long-name`)

OD$`left-stop-id` <- indice_paradas[OD$`left-stop-id`]
OD$`right-stop-id` <- indice_paradas[OD$`right-stop-id`]
OD <- OD %>% arrange(`left-stop-id`)
write.table(OD, file = "basis ecovia/OD.giv", sep = "; ", row.names = FALSE, quote = FALSE)

sum(OD$customers)
############################################################

#                         EDGES 
############################################################

df_linea <- function(linea){
  linea$ind <- indice_paradas[linea$name]
  df_linea <- data.frame(
    Origen = linea$ind[-nrow(linea)],
    Destino = linea$ind[-1],
    dist = linea$`dist(km)`[-nrow(linea)] 
  )
  return(df_linea)
}
linea1 <-  data.table(read_excel("Excels ecovia/paradasEco.xlsx",sheet = 3))
linea2 <-  data.table(read_excel("Excels ecovia/paradasEco.xlsx",sheet = 4))

linea1 <-  df_linea(linea1)
linea2 <-  df_linea(linea2)

edges <- data.table(unique(rbind(linea1,linea2)) %>% arrange(Origen,Destino))
edges <- edges %>%
  mutate(link_index=seq(1,nrow(edges),by=1),lower_bound=floor(((dist/18)*60)/2), upper_bound=ceiling((dist/18)*60)*2) 

edges<- edges[,c(4,1,2,3,5,6)]
colnames(edges) <- c("link_index", "from_stop", "to_stop", "length", "lower_bound", "upper_bound")
edges
edges[,length:= (length/18)*60]  # Descomentar solo en usar lc_traveling_time_cg
write.table(edges, file = "basis ecovia/Edge.giv", sep = "; ", row.names = FALSE, quote = FALSE)
save(Stop,edges,file = "basis ecovia/paradas_y_aristas_ecovia.RData")


############################################################

#                         POOL 
############################################################
library(data.table)
edges
colnames(edges)<- c("link_index", "Origen", "Destino", "length", "lower_bound", "upper_bound")

df_linea2 <- function(linea){
  linea$ind <- indice_paradas[linea$name]
  df_linea <- data.frame(
    Origen = linea$ind[-nrow(linea)],
    Destino = linea$ind[-1]
  )
  return(df_linea)
}

procesar_linea <- function(edges, df_linea, line_id) {
  df_linea %>%
    left_join(edges, by = c("Origen", "Destino")) %>%
    select(link_index) %>%
    mutate('line-id' = line_id,
           'edge-order' = seq(1, nrow(.)),
           .after = link_index) %>%
    select(`line-id`, `edge-order`, link_index)
  
}

E1_ciclo <-  data.table(read_excel("Excels ecovia/paradasEco.xlsx",sheet = 5))
ga <- df_linea2(E1_ciclo)%>% left_join(edges, by = c("Origen", "Destino"))
nodes <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)
# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))


layout_matrix <- as.matrix(nodes[, c("x", "y")])

# Graficar usando visNetwork
visNetwork(nodes, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)



E1_ciclo <- df_linea2(E1_ciclo)
E1_ciclo <- procesar_linea(edges,E1_ciclo,1)





E2_ida <-  data.table(read_excel("Excels ecovia/paradasEco.xlsx",sheet = 6))

ga <- df_linea2(E2_ida)%>% left_join(edges, by = c("Origen", "Destino"))
nodes <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)
# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))


layout_matrix <- as.matrix(nodes[, c("x", "y")])

# Graficar usando visNetwork
visNetwork(nodes, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)



E2_ida <- df_linea2(E2_ida)
E2_ida <- procesar_linea(edges,E2_ida,2)


E1M_ida <-  data.table(read_excel("Excels ecovia/paradasEco.xlsx",sheet = 7))
ga <- df_linea2(E1M_ida)%>% left_join(edges, by = c("Origen", "Destino"))
nodes <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)
# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))


layout_matrix <- as.matrix(nodes[, c("x", "y")])

# Graficar usando visNetwork
visNetwork(nodes, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)



E1M_ida <- df_linea2(E1M_ida)
E1M_ida <- procesar_linea(edges,E1M_ida,3)


E3_ida <-  data.table(read_excel("Excels ecovia/paradasEco.xlsx",sheet = 8))

ga <- df_linea2(E3_ida)%>% left_join(edges, by = c("Origen", "Destino"))
nodes <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)
# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))


layout_matrix <- as.matrix(nodes[, c("x", "y")])

# Graficar usando visNetwork
visNetwork(nodes, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)


E3_ida <- df_linea2(E3_ida)
E3_ida <- procesar_linea(edges,E3_ida,4)


E4_ida <-  data.table(read_excel("Excels ecovia/paradasEco.xlsx",sheet = 9))
ga <- df_linea2(E4_ida)%>% left_join(edges, by = c("Origen", "Destino"))
nodes <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`,color="#D35400") %>%
  select(id, label, title, x, y,color)
# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      #label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#2E4053", nrow(ga))))


layout_matrix <- as.matrix(nodes[, c("x", "y")])

# Graficar usando visNetwork
visNetwork(nodes, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = F) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)


E4_ida <- df_linea2(E4_ida)
E4_ida <- procesar_linea(edges,E4_ida,5)

Pool <- rbind(E1_ciclo,
              E2_ida,
              E1M_ida,
              E3_ida,
              E4_ida
)
Pool <- Pool %>% arrange(`line-id`)
write.table(Pool, file = "basis ecovia/Pool.giv", sep = "; ", row.names = FALSE, quote = FALSE)



# ---- Red de quito ----

# Ajustar las columnas del dataframe edges
colnames(edges) <- c("link_index", "Origen", "Destino", "length", "lower_bound", "upper_bound")


label <- case_when(
  Stop$`short-name` == "Guamani" ~ "Guamaní",
  Stop$`short-name` == "Quitumbe" ~ "Quitumbe",
  Stop$`short-name` == "Recreo" ~ "Recreo",
  Stop$`short-name` == "Playon de la Marin" ~ "Playón de la Marín",
  Stop$`short-name` == "Marin Central" ~ "Marín Central",
  Stop$`short-name` == "Rio Coca" ~ "Rio Coca",
  TRUE ~ NA_character_  # Usamos NA en lugar de NULL
)


# Crear nodos
nodes <- Stop %>%
  mutate(id = `stop-id`, label = label, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`+350,
         color="#D35400" , shape="dot",front =17 )%>%
  select(id, label, title, x, y,color,shape,front)

# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(edges$Origen),
                      to = c(edges$Destino),
                      label= as.character(edges$link_index),
                      font = list(size = 15, color = "black"),
                      color = c(rep("#2E4053", nrow(edges))))

scale_factor <- 0.1  # Ajusta este factor para acercar los nodos (menor valor = más cerca)
layout_matrix <- as.matrix(nodes[, c("x", "y")]) * scale_factor
# Graficar usando visNetwork con ajustes adicionales para una imagen más compacta
visNetwork(nodes, aristas, width = "100%", height = "400px") %>%
  visNodes(shape = "dot", borderWidth = 1, size = 12,font = list(size = 49))%>%
  visEdges(smooth = list(enabled = TRUE, type = "curvedCW",size=5),width = 4) %>%
  visPhysics(stabilization = FALSE,
             barnesHut = list(avoidOverlap = 1, gravitationalConstant = -2000, centralGravity = 0.3, springLength = 100, springConstant = 0.05)) %>%
  visOptions(highlightNearest = F, nodesIdSelection = F) %>%
  visLayout(randomSeed = 88) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)

############################################################

#                       ----Ecovia estresado  ----
############################################################

OD_estresada <- data.table(OD)[,customers := customers*10]


colnames(edges)<- c("link_index", "Origen", "Destino", "length", "lower_bound", "upper_bound")


E1 <-  data.table(read_excel("Excels instancias estresadas/Ecovia estresado.xlsx",sheet = 1))
ga <- df_linea2(E1)%>% left_join(edges, by = c("Origen", "Destino"))
estaciones <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)
# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))
layout_matrix <- as.matrix(estaciones[, c("x", "y")])

visNetwork(estaciones, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)

E1 <- df_linea2(E1)
E1 <- procesar_linea(edges,E1,1)





E2 <-  data.table(read_excel("Excels instancias estresadas/Ecovia estresado.xlsx",sheet = 2))
ga <- df_linea2(E2)%>% left_join(edges, by = c("Origen", "Destino"))
estaciones <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)
# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))
layout_matrix <- as.matrix(estaciones[, c("x", "y")])

visNetwork(estaciones, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)

E2 <- df_linea2(E2)
E2 <- procesar_linea(edges,E2,2)





E3 <-  data.table(read_excel("Excels instancias estresadas/Ecovia estresado.xlsx",sheet = 3))
ga <- df_linea2(E3)%>% left_join(edges, by = c("Origen", "Destino"))
estaciones <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)
# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))
layout_matrix <- as.matrix(estaciones[, c("x", "y")])

visNetwork(estaciones, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)

E3 <- df_linea2(E3)
E3 <- procesar_linea(edges,E3,3)


E4 <-  data.table(read_excel("Excels instancias estresadas/Ecovia estresado.xlsx",sheet = 4))
ga <- df_linea2(E4)%>% left_join(edges, by = c("Origen", "Destino"))
estaciones <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)
# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))
layout_matrix <- as.matrix(estaciones[, c("x", "y")])

visNetwork(estaciones, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)

E4 <- df_linea2(E4)
E4 <- procesar_linea(edges,E4,4)



E5 <-  data.table(read_excel("Excels instancias estresadas/Ecovia estresado.xlsx",sheet = 5))
ga <- df_linea2(E5)%>% left_join(edges, by = c("Origen", "Destino"))
estaciones <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)
# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))
layout_matrix <- as.matrix(estaciones[, c("x", "y")])

visNetwork(estaciones, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)

E5 <- df_linea2(E5)
E5 <- procesar_linea(edges,E5,5)



E6 <-  data.table(read_excel("Excels instancias estresadas/Ecovia estresado.xlsx",sheet = 6))
ga <- df_linea2(E6)%>% left_join(edges, by = c("Origen", "Destino"))
estaciones <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)
# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))
layout_matrix <- as.matrix(estaciones[, c("x", "y")])

visNetwork(estaciones, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)

E6 <- df_linea2(E6)
E6 <- procesar_linea(edges,E6,6)




E7 <-  data.table(read_excel("Excels instancias estresadas/Ecovia estresado.xlsx",sheet = 7))
ga <- df_linea2(E7)%>% left_join(edges, by = c("Origen", "Destino"))
estaciones <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)
# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))
layout_matrix <- as.matrix(estaciones[, c("x", "y")])

visNetwork(estaciones, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)

E7 <- df_linea2(E7)
E7 <- procesar_linea(edges,E7,7)



E8 <-  data.table(read_excel("Excels instancias estresadas/Ecovia estresado.xlsx",sheet = 8))
ga <- df_linea2(E8)%>% left_join(edges, by = c("Origen", "Destino"))
estaciones <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)
# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))
layout_matrix <- as.matrix(estaciones[, c("x", "y")])

visNetwork(estaciones, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)

E8 <- df_linea2(E8)
E8 <- procesar_linea(edges,E8,8)



E9 <-  data.table(read_excel("Excels instancias estresadas/Ecovia estresado.xlsx",sheet = 9))
ga <- df_linea2(E9)%>% left_join(edges, by = c("Origen", "Destino"))
estaciones <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)
# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))
layout_matrix <- as.matrix(estaciones[, c("x", "y")])

visNetwork(estaciones, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)

E9 <- df_linea2(E9)
E9 <- procesar_linea(edges,E9,9)


E10 <-  data.table(read_excel("Excels instancias estresadas/Ecovia estresado.xlsx",sheet = 10))
ga <- df_linea2(E10)%>% left_join(edges, by = c("Origen", "Destino"))
estaciones <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)
# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))
layout_matrix <- as.matrix(estaciones[, c("x", "y")])

visNetwork(estaciones, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)

E10 <- df_linea2(E10)
E10 <- procesar_linea(edges,E10,10)


E11 <-  data.table(read_excel("Excels instancias estresadas/Ecovia estresado.xlsx",sheet = 11))
ga <- df_linea2(E11)%>% left_join(edges, by = c("Origen", "Destino"))
estaciones <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)
# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))
layout_matrix <- as.matrix(estaciones[, c("x", "y")])

visNetwork(estaciones, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)

E11 <- df_linea2(E11)
E11 <- procesar_linea(edges,E11,11)


E12 <-  data.table(read_excel("Excels instancias estresadas/Ecovia estresado.xlsx",sheet = 12))
ga <- df_linea2(E12)%>% left_join(edges, by = c("Origen", "Destino"))
estaciones <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)
# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))
layout_matrix <- as.matrix(estaciones[, c("x", "y")])

visNetwork(estaciones, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)

E12 <- df_linea2(E12)
E12 <- procesar_linea(edges,E12,12)



E13 <-  data.table(read_excel("Excels instancias estresadas/Ecovia estresado.xlsx",sheet = 13))
ga <- df_linea2(E13)%>% left_join(edges, by = c("Origen", "Destino"))
estaciones <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)
# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))
layout_matrix <- as.matrix(estaciones[, c("x", "y")])

visNetwork(estaciones, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)

E13 <- df_linea2(E13)
E13 <- procesar_linea(edges,E13,13)



E14 <-  data.table(read_excel("Excels instancias estresadas/Ecovia estresado.xlsx",sheet = 14))
ga <- df_linea2(E14)%>% left_join(edges, by = c("Origen", "Destino"))
estaciones <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)
# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))
layout_matrix <- as.matrix(estaciones[, c("x", "y")])

visNetwork(estaciones, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)

E14 <- df_linea2(E14)
E14 <- procesar_linea(edges,E14,14)





Pool_estresado <- rbind(E1,E2,E3,E4,E5,E6,E7,E8,E9,E10,E11,E12,E13,E14)
Pool_estresado <- Pool_estresado %>% arrange(`line-id`)

write.table(Pool_estresado, file = "basis ecovia  estresado/Pool.giv", sep = "; ", row.names = FALSE, quote = FALSE)
write.table(Stop, file = "basis ecovia  estresado/Stop.giv", sep = "; ", row.names = FALSE, quote = FALSE)
write.table(Demand, file = "basis ecovia  estresado/Demand.giv", sep = "; ", row.names = FALSE, quote = FALSE)
write.table(edges, file = "basis ecovia  estresado/Edge.giv", sep = "; ", row.names = FALSE, quote = FALSE)
write.table(OD_estresada, file = "basis ecovia  estresado/OD.giv", sep = "; ", row.names = FALSE, quote = FALSE)

sum(OD_estresada$customers)

