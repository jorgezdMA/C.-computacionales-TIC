library(tidyverse)
library(readxl)
library(openxlsx)
library(visNetwork)
library(data.table)

############################################################

#                     -----STOP-----
############################################################
paradas <-  data.table(read_excel("Excels ecovia/paradasEco - dir y no dirg.xlsx",sheet = 1))

lineaidaQ_R <-   data.table(read_excel("Excels ecovia/paradasEco - dir y no dirg.xlsx",sheet = 4))
lineaidaG_U <-  data.table(read_excel("Excels ecovia/paradasEco - dir y no dirg.xlsx",sheet = 8))

StoplineaidaQ_R<-paradas[paradas$name %in%lineaidaQ_R$name ,][,name:=paste0(name,"|>")]
StoplineaidaG_U<-paradas[paradas$name %in%lineaidaG_U$name ,][,name:=paste0(name,"|>")]

linevueltaR_Q <-   data.table(read_excel("Excels ecovia/paradasEco - dir y no dirg.xlsx",sheet = 6))
linevueltaU_G <-  data.table(read_excel("Excels ecovia/paradasEco - dir y no dirg.xlsx",sheet = 10))

StoplinevueltaR_Q <-paradas[paradas$name %in%linevueltaR_Q$name ,][,name:=paste0(name,"<|")][,Y:= Y+250][,X:= X-150]
StoplinevueltaU_G<-paradas[paradas$name %in%linevueltaU_G$name ,][,name:=paste0(name,"<|")][,Y:= Y+250][,X:= X-150]
#quitar universidadesvuelta
#StoplinevueltaU_G <- StoplinevueltaU_G[-nrow(StoplinevueltaU_G),]

Stop <- rbind(StoplineaidaQ_R,StoplineaidaG_U,StoplinevueltaR_Q,StoplinevueltaU_G)
Stop <- unique(Stop)
Stop <- Stop[order(ind)][,ind:= seq(1,nrow(Stop))]


nodes <- Stop %>%
  mutate(id = ind, label = name, title = ind, x = X, y = Y) %>%
  select(id, label, title, x, y)

# Generar matriz de diseño
layout_matrix <- as.matrix(nodes[, c("x", "y")])

# Graficar usando visNetwork
visNetwork(nodes, data.frame(from = integer(0), to = integer(0))) %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)

Stopcolname <- c("stop-id","short-name","long-name","x-coordinate","y-coordinate")
Stop <- Stop[,(Stopcolname):= .(Stop$ind,Stop$name, Stop$name, Stop$X, Stop$Y)]
Stop[, c("ind", "name","X","Y") := NULL]

write.table(Stop, file = "basis ecovia dirigido/Stop.giv", sep = "; ", row.names = FALSE, quote = FALSE)


############################################################

#                         EDGES 
############################################################
indice_paradas <- setNames(Stop$`stop-id`, Stop$`long-name`)

lineaidaQ_R <-   data.table(read_excel("Excels ecovia/paradasEco - dir y no dirg.xlsx",sheet = 5))
lineaidaG_U <-  data.table(read_excel("Excels ecovia/paradasEco - dir y no dirg.xlsx",sheet = 9))

lineaidaQ_R  <-  df_linea(lineaidaQ_R )
lineaidaG_U<-  df_linea(lineaidaG_U )

linevueltaR_Q <-   data.table(read_excel("Excels ecovia/paradasEco - dir y no dirg.xlsx",sheet = 7))
linevueltaU_G <-  data.table(read_excel("Excels ecovia/paradasEco - dir y no dirg.xlsx",sheet = 11))

linevueltaR_Q  <-  df_linea(linevueltaR_Q )
linevueltaU_G<-  df_linea(linevueltaU_G )

edges <- data.table(unique(rbind(lineaidaQ_R,lineaidaG_U,linevueltaR_Q,linevueltaU_G)) %>% arrange(Origen,Destino))

edges <- edges %>%
  mutate(link_index=seq(1,nrow(edges),by=1),lower_bound=floor(((dist/18)*60)/2), upper_bound=ceiling((dist/18)*60)*2) 

edges<- edges[,c(4,1,2,3,5,6)]
colnames(edges) <- c("link_index", "from_stop", "to_stop", "length", "lower_bound", "upper_bound")


arcos_circuitos <- data.table(link_index =c(83,84,85,86,87,88), from_stop =c(47,45,46,83,19,20),to_stop =c(48,46,45,84,20,19),length =0,lower_bound=0 ,upper_bound=1)
edges <-rbind(edges,arcos_circuitos)

edges[,length:= (length/18)*60]
edges

write.table(edges, file = "basis ecovia dirigido/Edge.giv", sep = "; ", row.names = FALSE, quote = FALSE)
save(Stop,edges,file = "basis ecovia dirigido/paradas_y_aristas_ecovia_dirigida.RData")



#####################################################
#             ----Red de transporte quito ecovia IDA ----
#####################################################


colnames(edges)<- c("link_index", "Origen", "Destino", "length", "lower_bound", "upper_bound")

ga <- data.table(unique(rbind(lineaidaQ_R,lineaidaG_U)))
ga<- ga %>% left_join(edges, by = c("Origen", "Destino"))


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
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1),arrows = "to") %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)



#####################################################
#             ----Red de transporte quito ecovia REGRESO----
#####################################################


colnames(edges)<- c("link_index", "Origen", "Destino", "length", "lower_bound", "upper_bound")

ga <- data.table(unique(rbind(linevueltaR_Q,linevueltaU_G)))
ga<- ga %>% left_join(edges, by = c("Origen", "Destino"))


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
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1),arrows = "to") %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)

#####################################################
#             ----Red de transporte quito ecovia COMPLETO----
#####################################################

# Ajustar las columnas del dataframe edges
colnames(edges) <- c("link_index", "Origen", "Destino", "length", "lower_bound", "upper_bound")

label <- case_when(
  Stop$`short-name` == "Guamani|>" ~ "Guamaní",
  Stop$`short-name` == "Guamani<|" ~ "Guamaní<|",
  Stop$`short-name` == "Quitumbe|>" ~ "Quitumbe|>",
  Stop$`short-name` == "Quitumbe<|" ~ "Quitumbe<|",
  Stop$`short-name` == "Recreo|>" ~ "Recreo|>",
  Stop$`short-name` == "Recreo<|" ~ "Recreo<|",
  Stop$`short-name` == "Playon de la Marin|>" ~ "Playón de la Marín|>",
  Stop$`short-name` == "Playon de la Marin<|" ~ "Playón de la Marín<|",
  Stop$`short-name` == "Marin Central|>" ~ "Marín Central|>",
  Stop$`short-name` == "Marin Central<|" ~ "Marín Central<|",
  Stop$`short-name` == "Rio Coca|>" ~ "Rio Coca|>",
  Stop$`short-name` == "Rio Coca<|" ~ "Rio Coca<|",
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
  visEdges(smooth = list(enabled = TRUE, type = "curvedCW",size=5), arrows = "to",width = 4) %>%
  visPhysics(stabilization = FALSE,
             barnesHut = list(avoidOverlap = 1, gravitationalConstant = -2000, centralGravity = 0.3, springLength = 100, springConstant = 0.05)) %>%
  visOptions(highlightNearest = F, nodesIdSelection = F) %>%
  visLayout(randomSeed = 88) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)

############################################################

#                         POOL 
############################################################

colnames(edges)<- c("link_index", "Origen", "Destino", "length", "lower_bound", "upper_bound")


######################E1 ciclo############################


E1_ciclo <-  data.table(read_excel("Excels ecovia/paradasEco - dir y no dirg.xlsx",sheet = 13))

ga <- df_linea2(E1_ciclo)%>% left_join(edges, by = c("Origen", "Destino"))
estaciones <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)
# Definir las conexiones entre estaciones
aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))

visNetwork(nodes, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1),arrows = "to") %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)

E1_ciclo <- df_linea2(E1_ciclo)
E1_ciclo <- procesar_linea(edges,E1_ciclo,1)



######################E2 CERRADA############################


E2_ida <-  data.table(read_excel("Excels ecovia/paradasEco - dir y no dirg.xlsx",sheet = 15))

ga <- df_linea2(E2_ida)%>% left_join(edges, by = c("Origen", "Destino"))
estaciones <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)
aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))

visNetwork(nodes, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1),arrows = "to") %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)


E2_ida <- df_linea2(E2_ida)
E2_ida <- procesar_linea(edges,E2_ida,2)





E2_vuelta <-  data.table(read_excel("Excels ecovia/paradasEco - dir y no dirg.xlsx",sheet = 17))

ga <- df_linea2(E2_vuelta)%>% left_join(edges, by = c("Origen", "Destino"))
estaciones <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)

aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))

visNetwork(nodes, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1),arrows = "to") %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)



E2_vuelta <- df_linea2(E2_vuelta)
E2_vuelta <- procesar_linea(edges,E2_vuelta,3)

######################E1M ciclo############################


E1M_ciclo <-  data.table(read_excel("Excels ecovia/paradasEco - dir y no dirg.xlsx",sheet = 20))

ga <- df_linea2(E1M_ciclo)%>% left_join(edges, by = c("Origen", "Destino"))
estaciones <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)

aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))

visNetwork(nodes, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1),arrows = "to") %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)


E1M_ciclo <- df_linea2(E1M_ciclo)
E1M_ciclo <- procesar_linea(edges,E1M_ciclo,4)


######################E3 ciclo############################

E3_ciclo <-  data.table(read_excel("Excels ecovia/paradasEco - dir y no dirg.xlsx",sheet = 23))

ga <- df_linea2(E3_ciclo)%>% left_join(edges, by = c("Origen", "Destino"))
estaciones <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)

aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))

visNetwork(nodes, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1),arrows = "to") %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)




E3_ciclo <- df_linea2(E3_ciclo)
E3_ciclo <- procesar_linea(edges,E3_ciclo,5)


######################E4 ciclo############################

E4_ciclo <-  data.table(read_excel("Excels ecovia/paradasEco - dir y no dirg.xlsx",sheet = 26))

ga <- df_linea2(E4_ciclo)%>% left_join(edges, by = c("Origen", "Destino"))
estaciones <- Stop %>%
  mutate(id = `stop-id`, label =`short-name`, title = `stop-id`, x = `x-coordinate`, y = `y-coordinate`) %>%
  select(id, label, title, x, y)

aristas <- data.frame(from = c(ga$Origen),
                      to = c(ga$Destino),
                      label= as.character(ga$link_index),
                      font = list(size = 17, color = "black"),
                      color = c(rep("#F70D57", nrow(ga))))

visNetwork(nodes, aristas, width = "100%", height = "600px") %>%
  visNodes(shape = "dot",borderWidth = 1, size = 8) %>%
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1),arrows = "to") %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)


E4_ciclo <- df_linea2(E4_ciclo)
E4_ciclo <- procesar_linea(edges,E4_ciclo,6)

integracionsur_capuli1 <-data.table('line-id'= 7,'edge-order'=1, link_index=87)
integracionsur_capuli2 <-data.table('line-id'= 8,'edge-order'=1, link_index=88)

Pool <- rbind(E1_ciclo,
              E2_ida,E2_vuelta,
              E1M_ciclo,
              E3_ciclo,
              E4_ciclo,
              integracionsur_capuli1,
              integracionsur_capuli2
)
Pool <- Pool %>% arrange(`line-id`)
write.table(Pool, file = "basis ecovia dirigido/Pool.giv", sep = "; ", row.names = FALSE, quote = FALSE)

############################################################

#                       Matriz Od
############################################################
# 
# OD <- read_excel("Excels ecovia/paradasEco - dir y no dirg.xlsx",sheet = 27)
# row.names<-unname(unlist(OD[,1]))
# OD <- as.matrix(OD[,-1])
# rownames(OD)<- row.names
# OD
# 
# direccion_vuelta <- paste0(row.names,"<|")
# direccion_ida <- paste0(row.names,"|>")
# paradas_completas <- c(direccion_ida, direccion_vuelta)
# 
# 
# triangular_superior <- OD
# triangular_superior[!upper.tri(OD, diag = TRUE)] <- 0 
# 
# triangular_inferior <- OD
# triangular_inferior[!lower.tri(OD, diag = TRUE)] <- 0  # Mantener solo la parte triangular inferior
# 
# t(triangular_inferior)
# 
# OD_nueva <- matrix(0,nrow = 2*nrow(OD),ncol = 2*nrow(OD))
# colnames(OD_nueva)<-paradas_completas
# rownames(OD_nueva)<-paradas_completas
# 
# OD_nueva[1:nrow(OD), 
#               1:ncol(OD)] <-triangular_superior
# OD_nueva[(nrow(OD)+1):(2*nrow(OD)), 
#          (nrow(OD)+1):(2*nrow(OD))] <-triangular_inferior
# 
# OD_nueva <- data.frame(OD_nueva)
# colnames(OD_nueva) <- paradas_completas
# OD_nueva$name <- paradas_completas
# write.xlsx(OD_nueva, "OD_NUEVA_ECO.xlsx", rowNames = TRUE, colNames = TRUE)
# 
# 
# OD_largo_ent <- OD_nueva %>%
#   pivot_longer(cols = -name, names_to = "Destino", values_to = "Valor") %>%
#   rename(Origen = name)%>% mutate(Valor=ceiling(Valor))
# colnames(OD_largo_ent) <- c("left-stop-id", "right-stop-id", "customers")
# OD_nueva <- OD_largo_ent
# indice_paradas <- setNames(Stop$`stop-id`, Stop$`long-name`)
# 
# OD_nueva$`left-stop-id` <- indice_paradas[OD_nueva$`left-stop-id`]
# OD_nueva$`right-stop-id` <- indice_paradas[OD_nueva$`right-stop-id`]
# OD_nueva <- OD_nueva %>% arrange(`left-stop-id`) %>% filter(customers!=0)
# 
# write.table(OD_nueva, file = "basis ecovia dirigido/OD.giv", sep = "; ", row.names = FALSE, quote = FALSE)
# 
OD <- read_excel("Excels ecovia/paradasEco - dir y no dirg.xlsx",sheet = 27)
OD_largo_ent <- OD %>%
  pivot_longer(cols = -name, names_to = "Destino", values_to = "Valor") %>%
  rename(Origen = name)%>% mutate(Valor=ceiling(Valor))
colnames(OD_largo_ent) <- c("left-stop-id", "right-stop-id", "customers")
OD <- OD_largo_ent
indice_paradas <- setNames(Stop$`stop-id`, Stop$`long-name`)

OD$`left-stop-id` <- indice_paradas[OD$`left-stop-id`]
OD$`right-stop-id` <- indice_paradas[OD$`right-stop-id`]
OD <- OD %>% arrange(`left-stop-id`,`right-stop-id`)

sum(OD$customers)
write.table(OD, file = "basis ecovia dirigido/OD.giv", sep = "; ", row.names = FALSE, quote = FALSE)



############################################################

#                       ----POOL LINTIM----
############################################################


# Leer archivo GIV delimitado por comas
loadlintim <- data.table(read.csv("basis ecovia dirigido/Pool.giv", header = TRUE, sep = ";"))
colnames(loadlintim) <- c("lineid","order","link_index")
lineadelintim <- loadlintim %>% filter(lineid==4)

ga <- lineadelintim%>% left_join(edges, by = c("link_index"))
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
  visEdges(smooth = list(enabled = T, type = "curvedCW", roundness = 0.1),arrows = "to") %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visIgraphLayout(layout = "layout.norm", layoutMatrix = layout_matrix)










