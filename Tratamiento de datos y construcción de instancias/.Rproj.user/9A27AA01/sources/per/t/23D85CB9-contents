
df_linea <- function(linea){
  linea$ind <- indice_paradas[linea$name]
  df_linea <- data.frame(
    Origen = linea$ind[-nrow(linea)],
    Destino = linea$ind[-1],
    dist = linea$`dist(km)`[-nrow(linea)] 
  )
  return(df_linea)
}


df_linea2 <- function(linea){
  linea$ind <- indice_paradas[linea$name]
  df_linea <- data.frame(
    Origen = linea$ind[-nrow(linea)],
    Destino = linea$ind[-1]
  )
  return(df_linea)
}

df_linea3 <- function(linea){
  df_linea <- data.frame(
    linea=  linea$X..line.id[-nrow(linea)],
    Origen = linea$edge.id[-nrow(linea)],
    Destino = linea$edge.id[-1]
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
