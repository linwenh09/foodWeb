setwd(
  "C:/Users/Bryce/OneDrive/Documents/myLife/NTU_PHD/92FoodWeb/FoodWebDataset/new/networkDL/"
)
library(igraph)
library(stringr)

# 1 = undirected, raw
# 2 = undirected, standardized
# 3 = directed, raw
# 4 = directed, standardized
cata = 3

if (cata == 3) {
  # 3 = directed, raw
  for (i in c(
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    10,
    11,
    12,
    13,
    14,
    15,
    16,
    17,
    18,
    19,
    20,
    21,
    22,
    23,
    24,
    25,
    26,
    27,
    28,
    29,
    30,
    31,
    32,
    33,
    34,
    35,
    36,
    37,
    38,
    39,
    40,
    41,
    42,
    43,
    44,
    45,
    46,
    47,
    48,
    49,
    50,
    51,
    52,
    53,
    54,
    55,
    56,
    57,
    58,
    59,
    60,
    61,
    62,
    63,
    64,
    65,
    66,
    67,
    68,
    69,
    70,
    71,
    72,
    73,
    74,
    75,
    76,
    77,
    78,
    79,
    80,
    81,
    82,
    83,
    84,
    85,
    86,
    87,
    88,
    89,
    90,
    91,
    92
  ))
  {
    print(i)
    
    
    # for directed
    network  <- read_graph(str_c("FW", i, ".txt"), format = "dl")
    undirectNetwork <- as.undirected(network, mode = "collapse")
    directNetwork <- as.directed(network, mode = "arbitrary")
    
    DC <-
      as.numeric(degree(directNetwork, mode = "all")[order(names(degree(directNetwork, mode = "all")))])
    
    DC_in <-
      as.numeric(degree(directNetwork, mode = "in")[order(names(degree(directNetwork, mode = "in")))])
    
    DC_out <-
      as.numeric(degree(directNetwork, mode = "out")[order(names(degree(directNetwork, mode = "out")))])

    BC <-
      as.numeric(betweenness(directNetwork,directed = TRUE)[order(names(betweenness(directNetwork,directed = TRUE)))])

    # mark here if directed

    CC <-
      as.numeric(closeness(directNetwork, mode = "total")[order(names(closeness(directNetwork, mode = "total")))])
    CC_in <-
      as.numeric(closeness(directNetwork, mode = "in")[order(names(closeness(directNetwork, mode = "in")))])
    CC_out <-
      as.numeric(closeness(directNetwork, mode = "out")[order(names(closeness(directNetwork, mode = "out")))])
 
    AC <-
      as.numeric(alpha.centrality(directNetwork)[order(names(alpha.centrality(directNetwork)))])

    EC <-
      as.numeric(eigen_centrality(directNetwork, directed = F)$vector[order(names(eigen_centrality(directNetwork, directed = F)$vector))])

    KNN <-
      as.numeric(knn(directNetwork, mode = "total",neighbor.degree.mode="total")$knn[order(names(knn(directNetwork, mode = "total",neighbor.degree.mode="total")$knn))])
    KNN_in <-
      as.numeric(knn(directNetwork, mode = "in",neighbor.degree.mode="total")$knn[order(names(knn(directNetwork, mode = "in",neighbor.degree.mode="total")$knn))])
    KNN_out <-
      as.numeric(knn(directNetwork, mode = "out",neighbor.degree.mode="total")$knn[order(names(knn(directNetwork, mode = "out",neighbor.degree.mode="total")$knn))])

    HC <-
      as.numeric(harmonic_centrality(directNetwork, mode = "total")[order(names(harmonic_centrality(directNetwork, mode = "total")))])
    HC_in <-
      as.numeric(harmonic_centrality(directNetwork, mode = "in")[order(names(harmonic_centrality(directNetwork, mode = "in")))])
    HC_out <-
      as.numeric(harmonic_centrality(directNetwork, mode = "out")[order(names(harmonic_centrality(directNetwork, mode = "out")))])
    
    KC <-
      as.numeric(hub_score(directNetwork)$vector[order(names(hub_score(directNetwork)$vector))])
    
    ST <-
      as.numeric(strength(directNetwork, mode = "total")[order(names(strength(directNetwork, mode = "total")))])
    ST_in <-
      as.numeric(strength(directNetwork, mode = "in")[order(names(strength(directNetwork, mode = "in")))])
    ST_out <-
      as.numeric(strength(directNetwork, mode = "out")[order(names(strength(directNetwork, mode = "out")))])

    
    
    write.table(
      "ID,Degree, inDegree, outDegree, Betweenness,Closeness,inCC,outCC,Alpha_Centrality,Eigen_centrality,KNN, inKNN, outKNN, Harmonic_centrality,Kleinberg's hub centrality,Strength, inStrength, outStrength",
      
      file = str_c(i, ".csv"),
      sep = ",",
      append = T,
      col.names = FALSE,
      row.names = FALSE,
      quote = FALSE
      
    )
    sortedNames <- V(network)$name[order(V(network)$name)]
    
    write.table(
      #directed raw
      paste0(
        sortedNames,
        ",",
        DC,
        ",",
        DC_in,
        ",",
        DC_out,
        ",",
        BC,
        ",",
        CC,
        ",",
        CC_in,
        ",",
        CC_out,
        ",",
        AC,
        ",",
        EC,
        ",",
        KNN,
        ",",
        KNN_in,
        ",",
        KNN_out,
        ",",
        HC,
        ",",
        KC,
        ",",
        ST,
        ",",
        ST_in,
        ",",
        ST_out
      ),
      
      
      file = str_c(i, ".csv"),
      sep = ",",
      append = T,
      col.names = FALSE,
      row.names = FALSE,
      quote = FALSE
      
    )
  }
}

