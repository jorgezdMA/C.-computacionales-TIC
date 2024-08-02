library(tidyverse)
library(readxl)
library(openxlsx)
library(data.table)
library(readxl)
library(tidyverse)
library(visNetwork)

############################################################

#                         Paradas
############################################################

paradas <-  data.table(read_excel("Excels trolebus/paradas.xlsx",sheet = 1))
Demand <- copy(paradas)

Stopcolname <- c("stop-id","short-name","long-name","x-coordinate","y-coordinate")
Stop <- paradas[,(Stopcolname):= .(paradas$ind,paradas$name, paradas$name, paradas$X, paradas$Y)]


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

demandcolname <- c("demand-id","short-name","long-name","x-coordinate","y-coordinate","demand")
Demand <- Demand[,(demandcolname):= .(Demand$ind,Demand$name, Demand$name, Demand$X, Demand$Y,ceiling(Demand$Embarque))]

Demand[, c("ind", "name","X","Y","Embarque") := NULL]
Demand  

write.table(Stop, file = "basis trolebus/Stop.giv", sep = "; ", row.names = FALSE, quote = FALSE)
write.table(Demand, file = "basis trolebus/Demand.giv", sep = "; ", row.names = FALSE, quote = FALSE)


############################################################

#                       Matriz Od
############################################################

OD <- read_excel("Excels trolebus/paradas.xlsx",sheet = 8)
OD_largo_ent <- OD %>%
  pivot_longer(cols = -name, names_to = "Destino", values_to = "Valor") %>%
  rename(Origen = name)%>% mutate(Valor=ceiling(Valor))
colnames(OD_largo_ent) <- c("left-stop-id", "right-stop-id", "customers")
OD <- OD_largo_ent
indice_paradas <- setNames(Stop$`stop-id`, Stop$`long-name`)

OD$`left-stop-id` <- indice_paradas[OD$`left-stop-id`]
OD$`right-stop-id` <- indice_paradas[OD$`right-stop-id`]
OD <- OD %>% arrange(`left-stop-id`,`right-stop-id`)
#OD %>% mutate(n= seq(1,nrow(OD)))%>% filter(`left-stop-id` ==35 & `right-stop-id` ==36)

write.table(OD, file = "basis trolebus/OD.giv", sep = "; ", row.names = FALSE, quote = FALSE)

dim(OD %>% filter(customers>0))
sum(OD$customers)
############################################################

#                         EDGES 
############################################################
indice_paradas <- setNames(Stop$`stop-id`, Stop$`long-name`)

linea1 <-  data.table(read_excel("Excels trolebus/paradas.xlsx",sheet = 2,range="A1:C35"))

lineaida <- df_linea(linea1)

edges <- data.table(lineaida) %>% arrange(Origen,Destino)
edges <- edges %>% mutate(link_index=seq(1,nrow(edges),by=1),lower_bound=floor(((dist/16.34)*60)/2), upper_bound=ceiling((dist/16.34)*60)*2) 
  
edges<- edges[,c(4,1,2,3,5,6)]
colnames(edges) <- c("link_index", "from_stop", "to_stop", "length", "lower_bound", "upper_bound")
edges
edges[,length:= (length/18)*60]  # Descomentar solo en usar lc_traveling_time_cg
write.table(edges, file = "basis trolebus/Edge.giv", sep = "; ", row.names = FALSE, quote = FALSE)
save(Stop,edges,file = "basis trolebus/paradas_y_aristas_trole.RData")




############################################################

#                         POOL 
############################################################
library(data.table)
edges
colnames(edges)<- c("link_index", "Origen", "Destino", "length", "lower_bound", "upper_bound")


c1 <-  data.table(read_excel("Excels trolebus/paradas.xlsx",sheet = 4))
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


c2 <-  data.table(read_excel("Excels trolebus/paradas.xlsx",sheet = 5))
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

c4 <-  data.table(read_excel("Excels trolebus/paradas.xlsx",sheet =6))
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





c6<-  data.table(read_excel("Excels trolebus/paradas.xlsx",sheet = 7))
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



Pool <- rbind(c1,c2,c4,c6)
Pool <- Pool %>% arrange(`line-id`)
write.table(Pool, file = "basis trolebus/Pool.giv", sep = "; ", row.names = FALSE, quote = FALSE)




#########################################
#              Gráficos
#########################################
# Instalar y cargar la biblioteca necesaria
#install.packages("visNetwork")
library(visNetwork)
# Crear los datos de nodos y aristas
nodes <- data.frame(id = 1:4, 
                    label = c("1", "2", "3","4"),  # Eliminamos la etiqueta exterior
                    title =c("1", "2", "3","4"), # Colocamos el nombre dentro del nodo
                    shape = "circle", 
                    color = rep("#CAA5B1",4), 
                    size = 17, # Tamaño ajustado
                    font = list(size = 19, color = "black"))
edge <- data.frame(from = c(1, 3, 3), 
                    to = c(3, 2, 4),
                    width = 1.5, # Flechas más delgadas
                    color = "#1E92FA")

# Visualizar el grafo con visNetwork con personalización
visNetwork(nodes, edge) %>%
  visNodes(borderWidth = 2, label = NULL) %>%
  visEdges(arrows = list(to = list(enabled = FALSE)), length = 1) %>% 
  visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE) %>%
  visLayout(randomSeed = 42) %>%
  visInteraction(hover = TRUE) %>%
  visPhysics(stabilization = FALSE)

nodes <- data.frame(id = 1:9, 
                    label = c("(2,0)", "(2,l1)", "(1,0)", "(1,l1)", "(1,l2)", "(3,l1)", "(3,l2)","(4,l2)", "(4,0)"),  
                    shape = "oval", 
                    color = c("#CAA5B1", "#C1E1C1", "#CAA5B1", "#C1E1C1", "#C1E1C1", "#C1E1C1", "#C1E1C1", "#C1E1C1", "#CAA5B1"), 
                    size = 20, 
                    font = list(size = 20, color = "black"))

edge <- data.frame(from = c(1, 2, 3, 3, 8, 4, 6, 5, 7, 2, 6, 4, 6, 5, 7, 7, 8), 
                    to =   c(2, 1, 4, 5, 9, 5, 7, 4, 6, 6, 2, 6, 4, 7, 5, 8, 7),
                    color = c(rep("#1E92FA", 5), rep("#FA9412", 4), rep("#F70D57", 8)),
                    style = c(rep("solid", 5), rep("dashed", 4), rep("dotted", 8)),
                    arrows = c(rep("to", 17)),
                    smooth = list(enabled = TRUE, type = "curvedCCW", roundness = 0.2))

changego <- visNetwork(nodes, edge, width = "100%", height = "600px") %>%
  visNodes(borderWidth = 2) %>%
  visEdges(smooth = list(enabled = TRUE, type = "curvedCW", roundness = 0.2)) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE) %>%
  visInteraction(hover = TRUE) %>%
  visPhysics(enabled = F)



### RED DE QUITO 

# Ajustar las columnas del dataframe edges
colnames(edges) <- c("link_index", "Origen", "Destino", "length", "lower_bound", "upper_bound")


# Crear nodos
label <- case_when(
  Stop$`short-name` == "Quitumbe" ~ "Quitumbe",
  Stop$`short-name` == "Recreo" ~ "Recreo",
  Stop$`short-name` == "Moran Valverde" ~ "Morán Valverde",
  Stop$`short-name` == "El Labrador" ~ "El Labrador",
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







# # Leer archivo GIV delimitado por comas
# OD_editar <- data.table(read.csv("basis trolebus/OD.giv", header = TRUE, sep = ";"))
# colnames(OD_editar)<-c("left-stop-id","right-stop-id","customers")
# head(OD_editar)
# indice_paradas
# OD_editar$"left-stop-id"<- names(indice_paradas)[OD_editar$"left-stop-id"]
# OD_editar$"right-stop-id"<- names(indice_paradas)[OD_editar$"right-stop-id"]
# 
# OD_editar
# OD_editarl <- OD_editar %>% pivot_wider(names_from = `right-stop-id`, values_from = customers, values_fill = list(customers = 0))
# 
# wb <- createWorkbook()
# 
# # Agregar la primera hoja y escribir el dataframe original
# addWorksheet(wb, "Original")
# writeData(wb, sheet = "Original", OD_editar)
# 
# # Agregar la segunda hoja y escribir el dataframe transformado
# addWorksheet(wb, "Transformado")
# writeData(wb, sheet = "Transformado", OD_editarl)
# 
# # Guardar el workbook en un archivo
# saveWorkbook(wb, file = "Excels trolebus/OD_trole_lintim.xlsx", overwrite = TRUE)
# 


############################################################

#                       ----POOL LINTIM----
############################################################


# Leer archivo GIV delimitado por comas
# loadlintim <- data.table(read.csv("basis trolebus/Pool.giv", header = TRUE, sep = ";"))
# colnames(loadlintim) <- c("lineid","order","link_index")
# lineadelintim <- loadlintim %>% filter(lineid==1)
# 
# ga <- lineadelintim%>% left_join(edges, by = c("link_index"))
# estaciones <- Stop %>%
#   mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
#   select(id, label, title, x, y)
# # Definir las conexiones entre estaciones
# aristas <- data.frame(from = c(ga$Origen),
#                       to = c(ga$Destino),
#                       label= as.character(ga$link_index),
#                       font = list(size = 17, color = "black"),
#                       color = c(rep("#F70D57", nrow(ga))))
# # Crear el grafo
# visNetwork(estaciones, aristas, width = "100%", height = "600px") %>%
#   visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
#   visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1),arrows = "to") %>%
#   visPhysics(stabilization = FALSE) %>%
#   visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
#   visLayout(randomSeed = 42) %>%
#   visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)
# 




############################################################

#                       ----Tolebus estresado  ----
############################################################
library(data.table)
Stop
OD_estresada <- data.table(OD)[,customers := customers*10]


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


Pool_estresado <- rbind(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10)
Pool_estresado <- Pool_estresado %>% arrange(`line-id`)

write.table(Pool_estresado, file = "basis trole  estresado/Pool.giv", sep = "; ", row.names = FALSE, quote = FALSE)
write.table(Stop, file = "basis trole  estresado/Stop.giv", sep = "; ", row.names = FALSE, quote = FALSE)
write.table(Demand, file = "basis trole  estresado/Demand.giv", sep = "; ", row.names = FALSE, quote = FALSE)
write.table(edges, file = "basis trole  estresado/Edge.giv", sep = "; ", row.names = FALSE, quote = FALSE)
write.table(OD_estresada, file = "basis trole  estresado/OD.giv", sep = "; ", row.names = FALSE, quote = FALSE)

sum(OD_estresada$customers)

