library(tidyverse)
library(readxl)
library(openxlsx)
library(data.table)
library(readxl)
library(visNetwork)
paradas <-  data.table(read_excel("Excels trole ecovia integrada/paradasIntegrada.xlsx",sheet = 1))
Demand <- copy(paradas)

Stopcolname <- c("stop-id","short-name","long-name","x-coordinate","y-coordinate")
Stop <- paradas[,(Stopcolname):= .(paradas$ind,paradas$name, paradas$name, paradas$X, -paradas$Y)]


Stop <- unique(Stop)
nodes <- Stop %>%
  mutate(id = ind, label = name, title = name, x = X, y = Y, color="#A6E1DA") %>%
  select(id, label, title, x, y,color)

# Generar matriz de diseño
layout_matrix <- as.matrix(nodes[, c("x", "y")])

# Graficar usando visNetwork
visNetwork(nodes, data.frame(from = integer(0), to = integer(0))) %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)


Stop[, c("ind", "name","X","Y","Embarque") := NULL]
Stop <- unique(Stop)
demandcolname <- c("demand-id","short-name","long-name","x-coordinate","y-coordinate","demand")
Demand <- Demand[,(demandcolname):= .(Demand$ind,Demand$name, Demand$name, Demand$X, Demand$Y,ceiling(Demand$Embarque))]

Demand[, c("ind", "name","X","Y","Embarque") := NULL]
Demand  

write.table(Stop, file = "basis integrada/Stop.giv", sep = "; ", row.names = FALSE, quote = FALSE)
write.table(Demand, file = "basis integrada/Demand.giv", sep = "; ", row.names = FALSE, quote = FALSE)




############################################################

#                         EDGES 
############################################################
indice_paradas <- setNames(Stop$`stop-id`, Stop$`long-name`)

linea1trolebus <-  data.table(read_excel("Excels trole ecovia integrada/paradasIntegrada.xlsx",sheet =2 ))
linea1trolebus <- df_linea(linea1trolebus)

linea1ecovia<-  data.table(read_excel("Excels trole ecovia integrada/paradasIntegrada.xlsx",sheet = 3))
linea2ecovia <-  data.table(read_excel("Excels trole ecovia integrada/paradasIntegrada.xlsx",sheet = 4))

linea1ecovia <-  df_linea(linea1ecovia)
linea2ecovia <-  df_linea(linea2ecovia)

edges <- data.table(unique(rbind(linea1trolebus,linea1ecovia,linea2ecovia)) %>% arrange(Origen,Destino))
edges <- edges %>%
  mutate(link_index=seq(1,nrow(edges),by=1),lower_bound=floor(((dist/18)*60)/2), upper_bound=ceiling((dist/18)*60)*2) 


edges<- edges[,c(4,1,2,3,5,6)]
colnames(edges) <- c("link_index", "from_stop", "to_stop", "length", "lower_bound", "upper_bound")
edges

arcos_circuitos <- data.table(link_index =c(73,74), from_stop =c(39,1),to_stop =c(72,7),length =c(2.422124,6.46906)) %>% 
  mutate(lower_bound=floor(((length/18)*60)/2), upper_bound=ceiling((length/18)*60)*2)

edges <-rbind(edges,arcos_circuitos)

edges[,length:= (length/18)*60]  # Descomentar solo en usar lc_traveling_time_cg
edges
write.table(edges, file = "basis integrada/Edge.giv", sep = "; ", row.names = FALSE, quote = FALSE)
save(Stop,edges,file = "basis integrada/paradas_y_aristas_integrado.RData")


#####################################################
#             ----Red de transporte quito integrada----
#####################################################


# Ajustar las columnas del dataframe edges
colnames(edges) <- c("link_index", "Origen", "Destino", "length", "lower_bound", "upper_bound")


# Crear nodos
label <- case_when(
  Stop$`short-name` == "Guamani" ~ "Guamaní",
  Stop$`short-name` == "Quitumbe" ~ "Quitumbe",
  Stop$`short-name` == "Recreo" ~ "Recreo",
  Stop$`short-name` == "Playon de la Marin" ~ "Playón de la Marín",
  Stop$`short-name` == "Marin Central" ~ "Marín Central",
  Stop$`short-name` == "Rio Coca" ~ "Rio Coca",
  Stop$`short-name` == "Moran Valverde" ~ "Morán Valverde",
  Stop$`short-name` == "El Labrador" ~ "El Labrador",
  TRUE ~ NA_character_  # Usamos NA en lugar de NULL
)
nodes <- Stop %>%
  mutate(id = `stop-id`, label = label, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`+350,
         color="#D35400" , shape="dot",front =17 )%>%
  select(id, label, title, x, y,color,shape,front)

# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(edges$Origen),
                      to = c(edges$Destino),
                      label= as.character(edges$link_index),
                      font = list(size = 8, color = "black"),
                      color = c(rep("#2E4053", nrow(edges))))

scale_factor <- 0.1  # Ajusta este factor para acercar los nodos (menor valor = más cerca)
layout_matrix <- as.matrix(nodes[, c("x", "y")]) * scale_factor
# Graficar usando visNetwork con ajustes adicionales para una imagen más compacta
visNetwork(nodes, aristas, width = "100%", height = "400px") %>%
  visNodes(shape = "dot", borderWidth = 1, size = 12,font = list(size = 39))%>%
  visEdges(smooth = list(enabled = TRUE, type = "curvedCW",size=5), arrows = "to",width = 4) %>%
  visPhysics(stabilization = FALSE,
             barnesHut = list(avoidOverlap = 1, gravitationalConstant = -2000, centralGravity = 0.3, springLength = 100, springConstant = 0.05)) %>%
  visOptions(highlightNearest = F, nodesIdSelection = F) %>%
  visLayout(randomSeed = 88) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)


############################################################

#                         POOL 
############################################################
edges
colnames(edges)<- c("link_index", "Origen", "Destino", "length", "lower_bound", "upper_bound")

########################## Trolebus ####################################


c1 <-  data.table(read_excel("Excels trole ecovia integrada/paradasIntegrada.xlsx",sheet = 5))
ga <- df_linea2(c1)%>% left_join(edges, by = c("Origen", "Destino"))
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

# Crear el grafo
visNetwork(estaciones, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)

c1 <- df_linea2(c1)
c1 <- procesar_linea(edges,c1,1)


c2 <-  data.table(read_excel("Excels trole ecovia integrada/paradasIntegrada.xlsx",sheet = 6))
ga <- df_linea2(c2)%>% left_join(edges, by = c("Origen", "Destino"))
estaciones <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)
# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))
# Crear el grafo
visNetwork(estaciones, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)

c2 <- df_linea2(c2)
c2 <- procesar_linea(edges,c2,2)

c4 <-  data.table(read_excel("Excels trole ecovia integrada/paradasIntegrada.xlsx",sheet =7))
ga <- df_linea2(c4)%>% left_join(edges, by = c("Origen", "Destino"))
estaciones <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)
# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))
# Crear el grafo
visNetwork(estaciones, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)

c4 <- df_linea2(c4)
c4 <- procesar_linea(edges,c4,3)





c6<-  data.table(read_excel("Excels trole ecovia integrada/paradasIntegrada.xlsx",sheet = 8))
ga <- df_linea2(c6)%>% left_join(edges, by = c("Origen", "Destino"))
estaciones <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)
# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))
# Crear el grafo
visNetwork(estaciones, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1)) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)

c6 <- df_linea2(c6)
c6 <- procesar_linea(edges,c6,4)

########################## Ecovia ####################################

E1_ciclo <-  data.table(read_excel("Excels trole ecovia integrada/paradasIntegrada.xlsx",sheet = 9))
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
E1_ciclo <- procesar_linea(edges,E1_ciclo,5)





E2_ida <-  data.table(read_excel("Excels trole ecovia integrada/paradasIntegrada.xlsx",sheet = 10))

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
E2_ida <- procesar_linea(edges,E2_ida,6)


E1M_ida <-  data.table(read_excel("Excels trole ecovia integrada/paradasIntegrada.xlsx",sheet = 11))
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
E1M_ida <- procesar_linea(edges,E1M_ida,7)


E3_ida <-  data.table(read_excel("Excels trole ecovia integrada/paradasIntegrada.xlsx",sheet = 12))

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
E3_ida <- procesar_linea(edges,E3_ida,8)


E4_ida <-  data.table(read_excel("Excels trole ecovia integrada/paradasIntegrada.xlsx",sheet = 13))
ga <- df_linea2(E4_ida)%>% left_join(edges, by = c("Origen", "Destino"))
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


E4_ida <- df_linea2(E4_ida)
E4_ida <- procesar_linea(edges,E4_ida,9)

integracion_G_Q <-data.table('line-id'= 10,'edge-order'=1, link_index=74)
integracion_R_L <-data.table('line-id'= 11,'edge-order'=1, link_index=73)

Pool <- rbind(c1,c2,c4,c6,E1_ciclo,
              E2_ida,
              E1M_ida,
              E3_ida,
              E4_ida,
              integracion_G_Q,
              integracion_R_L)
Pool <- Pool %>% arrange(`line-id`)
write.table(Pool, file = "basis integrada/Pool.giv", sep = "; ", row.names = FALSE, quote = FALSE)










# Leer archivo GIV delimitado por comas
OD_editar <- data.table(read.csv("basis integrada/OD.giv", header = TRUE, sep = ";"))
sum(OD_editar$customers)
colnames(OD_editar)<-c("left-stop-id","right-stop-id","customers")
head(OD_editar)
indice_paradas
OD_editar$"left-stop-id"<- names(indice_paradas)[OD_editar$"left-stop-id"]
OD_editar$"right-stop-id"<- names(indice_paradas)[OD_editar$"right-stop-id"]

OD_editar
OD_editarl <- OD_editar %>% pivot_wider(names_from = `right-stop-id`, values_from = customers, values_fill = list(customers = 0))

wb <- createWorkbook()

# Agregar la primera hoja y escribir el dataframe original
addWorksheet(wb, "Original")
writeData(wb, sheet = "Original", OD_editar)

# Agregar la segunda hoja y escribir el dataframe transformado
addWorksheet(wb, "Transformado")
writeData(wb, sheet = "Transformado", OD_editarl)

# Guardar el workbook en un archivo
saveWorkbook(wb, file = "Excels trole ecovia integrada/OD_integrada_lintim.xlsx", overwrite = TRUE)





############################################################

#                       ----Red integrada estresado  ----
############################################################


colnames(edges)<- c("link_index", "Origen", "Destino", "length", "lower_bound", "upper_bound")


T1 <-  data.table(read_excel("Excels instancias estresadas/Trolebús estresado.xlsx",sheet = 1))
ga <- df_linea2(T1)%>% left_join(edges, by = c("Origen", "Destino"))
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

T1 <- df_linea2(T1)
T1 <- procesar_linea(edges,T1,1)





T2 <-  data.table(read_excel("Excels instancias estresadas/Trolebús estresado.xlsx",sheet = 2))
ga <- df_linea2(T2)%>% left_join(edges, by = c("Origen", "Destino"))
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

T2 <- df_linea2(T2)
T2 <- procesar_linea(edges,T2,2)





T3 <-  data.table(read_excel("Excels instancias estresadas/Trolebús estresado.xlsx",sheet = 3))
ga <- df_linea2(T3)%>% left_join(edges, by = c("Origen", "Destino"))
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

T3 <- df_linea2(T3)
T3 <- procesar_linea(edges,T3,3)


T4 <-  data.table(read_excel("Excels instancias estresadas/Trolebús estresado.xlsx",sheet = 4))
ga <- df_linea2(T4)%>% left_join(edges, by = c("Origen", "Destino"))
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

T4 <- df_linea2(T4)
T4 <- procesar_linea(edges,T4,4)



T5 <-  data.table(read_excel("Excels instancias estresadas/Trolebús estresado.xlsx",sheet = 5))
ga <- df_linea2(T5)%>% left_join(edges, by = c("Origen", "Destino"))
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

T5 <- df_linea2(T5)
T5 <- procesar_linea(edges,T5,5)



T6 <-  data.table(read_excel("Excels instancias estresadas/Trolebús estresado.xlsx",sheet = 6))
ga <- df_linea2(T6)%>% left_join(edges, by = c("Origen", "Destino"))
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

T6 <- df_linea2(T6)
T6 <- procesar_linea(edges,T6,6)




T7 <-  data.table(read_excel("Excels instancias estresadas/Trolebús estresado.xlsx",sheet = 7))
ga <- df_linea2(T7)%>% left_join(edges, by = c("Origen", "Destino"))
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

T7 <- df_linea2(T7)
T7 <- procesar_linea(edges,T7,7)



T8 <-  data.table(read_excel("Excels instancias estresadas/Trolebús estresado.xlsx",sheet =8))
ga <- df_linea2(T8)%>% left_join(edges, by = c("Origen", "Destino"))
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

T8 <- df_linea2(T8)
T8 <- procesar_linea(edges,T8,8)



T9 <-  data.table(read_excel("Excels instancias estresadas/Trolebús estresado.xlsx",sheet = 9))
ga <- df_linea2(T9)%>% left_join(edges, by = c("Origen", "Destino"))
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

T9 <- df_linea2(T9)
T9 <- procesar_linea(edges,T9,9)

T10 <-  data.table(read_excel("Excels instancias estresadas/Trolebús estresado.xlsx",sheet = 10))
ga <- df_linea2(T10)%>% left_join(edges, by = c("Origen", "Destino"))
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

T10 <- df_linea2(T10)
T10 <- procesar_linea(edges,T10,10)


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
E1 <- procesar_linea(edges,E1,11)





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
E2 <- procesar_linea(edges,E2,12)





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
E3 <- procesar_linea(edges,E3,13)


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
E4 <- procesar_linea(edges,E4,14)



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
E5 <- procesar_linea(edges,E5,15)



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
E6 <- procesar_linea(edges,E6,16)




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
E7 <- procesar_linea(edges,E7,17)



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
E8 <- procesar_linea(edges,E8,18)



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
E9 <- procesar_linea(edges,E9,19)


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
E10 <- procesar_linea(edges,E10,20)


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
E11 <- procesar_linea(edges,E11,21)


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
E12 <- procesar_linea(edges,E12,22)



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
E13 <- procesar_linea(edges,E13,23)



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
E14 <- procesar_linea(edges,E14,24)


integracion_G_Q <-data.table('line-id'= 25,'edge-order'=1, link_index=74)
integracion_R_L <-data.table('line-id'= 26,'edge-order'=1, link_index=73)



Pool_estresado <- rbind(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,
                        E1,E2,E3,E4,E5,E6,E7,E8,E9,E10,E11,E12,E13,E14,integracion_G_Q,
                        integracion_R_L)
Pool_estresado <- Pool_estresado %>% arrange(`line-id`)


#OD_estresada <- data.table(OD)[,customers := customers*10]



write.table(Pool_estresado, file = "basis integrada estresada/Pool.giv", sep = "; ", row.names = FALSE, quote = FALSE)
write.table(Stop, file = "basis integrada estresada/Stop.giv", sep = "; ", row.names = FALSE, quote = FALSE)
write.table(Demand, file = "basis integrada estresada/Demand.giv", sep = "; ", row.names = FALSE, quote = FALSE)
write.table(edges, file = "basis integrada estresada/Edge.giv", sep = "; ", row.names = FALSE, quote = FALSE)


# Leer archivo GIV delimitado por comas
OD <- data.table(read.csv("basis integrada estresada/OD.giv", header = TRUE, sep = ";"))
colnames(OD_editar)<-c("left-stop-id","right-stop-id","customers")
sum(OD$customers)
OD_estresada <- data.table(OD)[,customers:= customers*10]
write.table(OD_estresada, file = "basis integrada estresada/OD.giv", sep = "; ", row.names = FALSE, quote = FALSE)


