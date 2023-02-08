dijkstra <- function(graph, start){
  #' A função retorna o comprimento do passeio mais curto.
  #' O algoritmo não suporta arestas de valor negativo.
  #' O tempo de execução aumenta com a quantidade de nós (vértíces) lidos.
  #' @param graph recebe a matriz de adjacência de um grafo.
  #' @param start nó início (númerico)
  
  
  
  # Define distâncias do nó inicial para todos os outros nós
  distances = rep(Inf, nrow(graph))
  
  # Define a distância do nó inicial para si mesmo como 0
  distances[start] = 0
  
  # Objeto que irá comparar visitas à um determinado nó
  visited = rep(FALSE, nrow(graph))
  
  
  # Execute sempre que houver um nó para ser visitado
  repeat{
    
    # Encontre o nó com a menor distância atual do nó inicial
    shortest_distance = Inf
    shortest_index = -1
    for(i in seq_along(distances)) {
      # Compare com todos os nós que ainda não foram visitados
      if(distances[i] < shortest_distance && !visited[i]){
        shortest_distance = distances[i]
        shortest_index = i
      }
    }
    
    cat(" ► Visitando o nó ", dimnames(graph)[[1]][shortest_index], " com distância atual ", shortest_distance, "\n\n")
    
    if(shortest_index == -1){
      # Os nós foram visitados, resultado arrumado
      a= print("**********************************")
      a
      print('Algoritmo Dijkstra' )
      for(j in seq_along(graph[1,])){
        print(paste("A distância de ", dimnames(graph)[[1]][start], " até ",  dimnames(graph)[[1]][j], " é ", distances[j]  ))
        
      }
      return (a)
    }
    # Para todos os nós vizinhos que ainda não foram visitados
    for(i in seq_along(graph[shortest_index,])) {
      # o caminho dessa aresta é o mais curto?
      if(graph[shortest_index,i] != 0 && distances[i] > distances[shortest_index] + graph[shortest_index,i]){
        # se sim, salve como o caminho mais curto
        distances[i] = distances[shortest_index] + graph[shortest_index,i]
        cat(" ▲ Atualizando a distância do nó ", i, "º para ", distances[i], "\n")
      }
      # Marque o nó como visitado.
      visited[shortest_index] = TRUE
      cat("nós visitados: ", dimnames(graph)[[1]][visited], "\n")
      cat("Distâncias mais baixas:  ", distances, "\n\n")
    }
    
  }
}