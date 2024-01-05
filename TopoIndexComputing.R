setwd("C:/Users/Bryce/OneDrive/Documents/myLife/NTU_PHD/92FoodWeb/FoodWebDataset/new/networkDL/")
library(igraph)
library(stringr)

# 1 = undirected, raw
# 2 = undirected, standardized
# 3 = directed, raw
# 4 = directed, standardized
cata = 2


if (cata == 1) {
  # 1 = undirected, raw
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
    
    # for undirected
   
    network1  <- read_graph(str_c("FW", i, ".txt"), format = "dl")
    network <- as.undirected(network1, mode = "collapse", edge.attr.comb = "ignore")
  
    
    
    DC  <-
      as.data.frame(degree(network)[order(names(degree(network)))])

    DC1 <-
      as.numeric(degree(network)[order(names(degree(network)))])

    BC  <-
      as.data.frame(betweenness(network)[order(names(betweenness(network)))])
    BC1 <-
      as.numeric(betweenness(network)[order(names(betweenness(network)))])
    
    # mark here if directed
    CC  <-
      as.data.frame(closeness(network)[order(names(closeness(network)))])
    CC1 <-
      as.numeric(closeness(network)[order(names(closeness(network)))])
    
    AC  <-
      as.data.frame(alpha.centrality(network)[order(names(alpha.centrality(network)))])
    AC1 <-
      as.numeric(alpha.centrality(network)[order(names(alpha.centrality(network)))])
    
    #ECC <- as.data.frame(eccentricity(network))
    #ECC1<- as.numeric(eccentricity(network))
    
    EC  <-
      as.data.frame(eigen_centrality(network)$vector[order(names(eigen_centrality(network)$vector))])
    EC1 <-
      as.numeric(eigen_centrality(network)$vector[order(names(eigen_centrality(network)$vector))])
    
    
    KNN <-
      as.data.frame(knn(network)$knn[order(names(knn(network)$knn))]) ######################### Average nearest neighbor degree
    KNN1 <-
      as.numeric(knn(network)$knn[order(names(knn(network)$knn))])
    
    HC  <-
      as.data.frame(harmonic_centrality(network)[order(names(harmonic_centrality(network)))])
    HC1 <-
      as.numeric(harmonic_centrality(network)[order(names(harmonic_centrality(network)))])
    
    KC  <-
      as.data.frame(hub_score(network)$vector[order(names(hub_score(network)$vector))]) ################ Kleinberg's hub centrality scores.
    KC1 <-
      as.numeric(hub_score(network)$vector[order(names(hub_score(network)$vector))])
    
    
    ST  <-
      as.data.frame(strength(network)[order(names(strength(network)))]) ######################## Summing up the edge weights of the adjacent edges for each vertex.
    ST1 <-
      as.numeric(strength(network)[order(names(strength(network)))])
    
    
    
    write.table(
      # undirected output string
      "ID,Degree,Betweenness,Closeness,Alpha_Centrality,Eigen_centrality,KNN (Average nearest neighbor degree),Harmonic_centrality,Kleinberg's hub centrality,Strength (Summing up the edge weights of the adjacent edges for each vertex)",
      #"ID,Degree,Betweenness,Closeness,Alpha_Centrality,KNN (Average nearest neighbor degree),Harmonic_centrality,Kleinberg's hub centrality,Strength (Summing up the edge weights of the adjacent edges for each vertex)",
      
      file = str_c(i, ".csv"),
      sep = ",",
      append = T,
      col.names = FALSE,
      row.names = FALSE,
      quote = FALSE
      
    )
    sortedNames <- V(network)$name[order(V(network)$name)]
    
    write.table(
      # undirected raw
      paste0(
        sortedNames,
        ",",
        round(DC1,8),
        ",",
        round(BC1,8),
        ",",
        round(CC1,8),
        ",",
        round(AC1,8),
        ",",
        round(sqrt(EC1),8),
        ",",
        round(KNN1,8),
        ",",
        round(HC1,8),
        ",",
        round(KC1,8),
        ",",
        round(ST1,8)
      ),
      file = str_c(i, ".csv"),
      sep = ",",
      append = T,
      col.names = FALSE,
      row.names = FALSE,
      quote = FALSE
      
    )
  }
  
} else if (cata == 2) {
  # 2 = undirected, standardized
  
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

    # for undirected
    network1  <- read_graph(str_c("FW", i, ".txt"), format = "dl")
    network <- as.undirected(network1, mode = "collapse", edge.attr.comb = "ignore")
    
    
    DC  <-
      as.data.frame(degree(network)[order(names(degree(network)))])
    #sdd <-
    #  function(x)
    #    (x - median(x)) / (quantile(x, 0.75) - quantile(x, 0.25))

    DC1 <-
      as.numeric(degree(network)[order(names(degree(network)))])
    #DC2 <- as.numeric(t(apply(DC, 2, sdd)))
    #DC2 <- as.numeric(t(mapply(sdd, t(DC), sample(1:50, 1) ,sample(1:50, 1))))
    #DC2 <- (DC1-mean(DC1))/sd(DC1)
    DC2 <- DC1/mean(DC1)
    
    BC  <-
      as.data.frame(betweenness(network)[order(names(betweenness(network)))])
    BC1 <-
      as.numeric(betweenness(network)[order(names(betweenness(network)))])
    #BC2 <- as.numeric(t(apply(BC, 2, sdd)))
    #BC2 <- as.numeric(t(mapply(sdd, t(BC), sample(1:50, 1) ,sample(1:50, 1))))
    #BC2 <- (BC1-mean(BC1))/sd(BC1)
    BC2 <- BC1/mean(BC1)
    
    # mark here if directed
    CC  <-
      as.data.frame(closeness(network)[order(names(closeness(network)))])
    CC1 <-
      as.numeric(closeness(network)[order(names(closeness(network)))])
    #CC2 <- as.numeric(t(apply(CC, 2, sdd)))
    #CC2 <- as.numeric(t(mapply(sdd, t(CC), sample(1:50, 1) ,sample(1:50, 1))))
    #CC2 <- (CC1-mean(CC1))/sd(CC1)
    CC2 <- CC1/mean(CC1)
    
    
    AC  <-
      as.data.frame(alpha.centrality(network)[order(names(alpha.centrality(network)))])
    AC1 <-
      as.numeric(alpha.centrality(network)[order(names(alpha.centrality(network)))])
    #AC2 <- as.numeric(t(apply(AC, 2, sdd)))
    #AC2 <- as.numeric(t(mapply(sdd, t(AC), sample(1:50, 1) ,sample(1:50, 1))))
    #AC2 <- (AC1-mean(AC1))/sd(AC1)
    AC2 <- AC1/mean(AC1)
    
    #ECC <- as.data.frame(eccentricity(network)[order(names(eccentricity(network)))])
    #ECC1<- as.numeric(eccentricity(network)[order(names(eccentricity(network)))])
    #ECC2 <- as.numeric(t(apply(ECC, 2, sdd)))
    #ECC2 <- (ECC1-mean(ECC1))/sd(ECC1)
    
    EC  <-
      as.data.frame(eigen_centrality(network)$vector[order(names(eigen_centrality(network)$vector))])
    EC1 <-
      as.numeric(eigen_centrality(network)$vector[order(names(eigen_centrality(network)$vector))])
    #EC2 <- as.numeric(t(apply(EC, 2, sdd)))
    #EC2 <- as.numeric(t(mapply(sdd, t(EC), sample(1:50, 1) ,sample(1:50, 1))))
    EC1 <- sqrt(EC1)
    #EC2 <- (EC1-mean(EC1))/sd(EC1)
    EC2 <- EC1/mean(EC1)
    
    KNN <-
      as.data.frame(knn(network)$knn[order(names(knn(network)$knn))]) ######################### Average nearest neighbor degree
    KNN1 <-
      as.numeric(knn(network)$knn[order(names(knn(network)$knn))])
    #KNN2 <- as.numeric(t(apply(KNN, 2, sdd)))
    #KNN2 <- as.numeric(t(mapply(sdd, t(KNN), sample(1:50, 1) ,sample(1:50, 1))))
    #KNN2 <- (KNN1-mean(KNN1))/sd(KNN1)
    KNN2 <- KNN1/mean(KNN1)
    
    HC  <-
      as.data.frame(harmonic_centrality(network)[order(names(harmonic_centrality(network)))])
    HC1 <-
      as.numeric(harmonic_centrality(network)[order(names(harmonic_centrality(network)))])
    #HC2 <- as.numeric(t(apply(HC, 2, sdd)))
    #HC2 <- as.numeric(t(mapply(sdd, t(HC), sample(1:50, 1) ,sample(1:50, 1))))
    #HC2 <- (HC1-mean(HC1))/sd(HC1)
    HC2 <- HC1/mean(HC1)
    
    KC  <-
      as.data.frame(hub_score(network)$vector[order(names(hub_score(network)$vector))]) ################ Kleinberg's hub centrality scores.
    KC1 <-
      as.numeric(hub_score(network)$vector[order(names(hub_score(network)$vector))])
    #KC2 <- as.numeric(t(apply(KC, 2, sdd)))
    #KC2 <- as.numeric(t(mapply(sdd, t(KC), sample(1:50, 1) ,sample(1:50, 1))))
    #KC2 <- (KC1-mean(KC1))/sd(KC1)
    KC2 <- KC1/mean(KC1)
    
    ST  <-
      as.data.frame(strength(network)[order(names(strength(network)))]) ######################## Summing up the edge weights of the adjacent edges for each vertex.
    ST1 <-
      as.numeric(strength(network)[order(names(strength(network)))])
    #ST2 <- as.numeric(t(apply(ST, 2, sdd)))
    #ST2 <- as.numeric(t(mapply(sdd, t(ST), sample(1:50, 1) ,sample(1:50, 1))))
    #ST2 <- (ST1-mean(ST1))/sd(ST1)
    ST2 <- ST1/mean(ST1)
    
    write.table(
      # undirected output string
      "ID,Degree,Betweenness,Closeness,Alpha_Centrality,Eigen_centrality,KNN (Average nearest neighbor degree),Harmonic_centrality,Kleinberg's hub centrality,Strength (Summing up the edge weights of the adjacent edges for each vertex)",
      #"ID,Degree,Betweenness,Closeness,Alpha_Centrality,KNN (Average nearest neighbor degree),Harmonic_centrality,Kleinberg's hub centrality,Strength (Summing up the edge weights of the adjacent edges for each vertex)",
      
      file = str_c(i, ".csv"),
      sep = ",",
      append = T,
      col.names = FALSE,
      row.names = FALSE,
      quote = FALSE
      
    )
    sortedNames <- V(network)$name[order(V(network)$name)]
    
    write.table(
      # undirected standardized
      paste0(
        sortedNames,
        ",",
        round(DC2,8),
        ",",
        round(BC2,8),
        ",",
        round(CC2,8),
        ",",
        round(AC2,8),
        ",",
        round(EC2,8),
        ",",
        round(KNN2,8),
        ",",
        round(HC2,8),
        ",",
        round(KC2,8),
        ",",
        round(ST2,8)
      ),
      
      file = str_c(i, ".csv"),
      sep = ",",
      append = T,
      col.names = FALSE,
      row.names = FALSE,
      quote = FALSE
      
    )
  }
  
} else if (cata == 3) {
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
    
    DC  <-
      as.data.frame(degree(network)[order(names(degree(undirectNetwork)))])
    sdd <-
      function(x)
        (x - median(x)) / (quantile(x, 0.75) - quantile(x, 0.25))
    DC1 <-
      as.numeric(degree(network)[order(names(degree(undirectNetwork)))])
    DC2 <- as.numeric(t(apply(DC, 2, sdd)))
    
    BC  <-
      as.data.frame(betweenness(undirectNetwork)[order(names(betweenness(undirectNetwork)))])
    BC1 <-
      as.numeric(betweenness(undirectNetwork)[order(names(betweenness(undirectNetwork)))])
    BC2 <- as.numeric(t(apply(BC, 2, sdd)))
    
    # mark here if directed
    CC  <-
      as.data.frame(closeness(undirectNetwork)[order(names(closeness(undirectNetwork)))])
    CC1 <-
      as.numeric(closeness(undirectNetwork)[order(names(closeness(undirectNetwork)))])
    CC2 <- as.numeric(t(apply(CC, 2, sdd)))
    
    AC  <-
      as.data.frame(alpha.centrality(network)[order(names(alpha.centrality(network)))])
    AC1 <-
      as.numeric(alpha.centrality(network)[order(names(alpha.centrality(network)))])
    AC2 <- as.numeric(t(apply(AC, 2, sdd)))
    
    #ECC <- as.data.frame(eccentricity(network))
    #ECC1<- as.numeric(eccentricity(network))
    #ECC2 <- as.numeric(t(apply(ECC, 2, sdd)))
    
    EC  <-
      as.data.frame(eigen_centrality(network)$vector[order(names(eigen_centrality(network)$vector))])
    EC1 <-
      as.numeric(eigen_centrality(network)$vector[order(names(eigen_centrality(network)$vector))])
    EC2 <- as.numeric(t(apply(EC, 2, sdd)))
    
    KNN <-
      as.data.frame(knn(network)$knn[order(names(knn(network)$knn))]) ######################### Average nearest neighbor degree
    KNN1 <-
      as.numeric(knn(network)$knn[order(names(knn(network)$knn))])
    KNN2 <- as.numeric(t(apply(KNN, 2, sdd)))
    
    HC  <-
      as.data.frame(harmonic_centrality(network)[order(names(harmonic_centrality(network)))])
    HC1 <-
      as.numeric(harmonic_centrality(network)[order(names(harmonic_centrality(network)))])
    HC2 <- as.numeric(t(apply(HC, 2, sdd)))
    KC  <-
      as.data.frame(hub_score(network)$vector[order(names(hub_score(network)$vector))]) ################ Kleinberg's hub centrality scores.
    KC1 <-
      as.numeric(hub_score(network)$vector[order(names(hub_score(network)$vector))])
    KC2 <- as.numeric(t(apply(KC, 2, sdd)))
    
    ST  <-
      as.data.frame(strength(network)[order(names(strength(network)))]) ######################## Summing up the edge weights of the adjacent edges for each vertex.
    ST1 <-
      as.numeric(strength(network)[order(names(strength(network)))])
    ST2 <- as.numeric(t(apply(ST, 2, sdd)))
    
    
    write.table(
      "ID,Degree,Betweenness,Closeness,Alpha_Centrality,Eigen_centrality,KNN (Average nearest neighbor degree),Harmonic_centrality,Kleinberg's hub centrality,Strength (Summing up the edge weights of the adjacent edges for each vertex)",
      
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
        DC1,
        ",",
        BC1,
        ",",
        CC1,
        ",",
        AC1,
        ",",
        EC1,
        ",",
        KNN1,
        ",",
        HC1,
        ",",
        KC1,
        ",",
        ST1
      ),
      
      
      file = str_c(i, ".csv"),
      sep = ",",
      append = T,
      col.names = FALSE,
      row.names = FALSE,
      quote = FALSE
      
    )
  }
  
} else if (cata == 4) {
  # 4 = directed, standardized
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
    
    DC  <-
      as.data.frame(degree(undirectNetwork)[order(names(degree(undirectNetwork)))])
    sdd <-
      function(x)
        (x - median(x)) / (quantile(x, 0.75) - quantile(x, 0.25))
    DC1 <-
      as.numeric(degree(undirectNetwork)[order(names(degree(undirectNetwork)))])
    DC2 <- as.numeric(t(apply(DC, 2, sdd)))
    
    BC  <-
      as.data.frame(betweenness(undirectNetwork)[order(names(betweenness(undirectNetwork)))])
    BC1 <-
      as.numeric(betweenness(undirectNetwork)[order(names(betweenness(undirectNetwork)))])
    BC2 <- as.numeric(t(apply(BC, 2, sdd)))
    
    # mark here if directed
    CC  <-
      as.data.frame(closeness(undirectNetwork)[order(names(closeness(undirectNetwork)))])
    CC1 <-
      as.numeric(closeness(undirectNetwork)[order(names(closeness(undirectNetwork)))])
    CC2 <- as.numeric(t(apply(CC, 2, sdd)))
    
    AC  <-
      as.data.frame(alpha.centrality(network)[order(names(alpha.centrality(network)))])
    AC1 <-
      as.numeric(alpha.centrality(network)[order(names(alpha.centrality(network)))])
    AC2 <- as.numeric(t(apply(AC, 2, sdd)))
    
    #ECC <- as.data.frame(eccentricity(network))
    #ECC1<- as.numeric(eccentricity(network))
    #ECC2 <- as.numeric(t(apply(ECC, 2, sdd)))
    
    EC  <-
      as.data.frame(eigen_centrality(network)$vector[order(names(eigen_centrality(network)$vector))])
    EC1 <-
      as.numeric(eigen_centrality(network)$vector[order(names(eigen_centrality(network)$vector))])
    EC2 <- as.numeric(t(apply(EC, 2, sdd)))
    
    KNN <-
      as.data.frame(knn(network)$knn[order(names(knn(network)$knn))]) ######################### Average nearest neighbor degree
    KNN1 <-
      as.numeric(knn(network)$knn[order(names(knn(network)$knn))])
    KNN2 <- as.numeric(t(apply(KNN, 2, sdd)))
    
    HC  <-
      as.data.frame(harmonic_centrality(network)[order(names(harmonic_centrality(network)))])
    HC1 <-
      as.numeric(harmonic_centrality(network)[order(names(harmonic_centrality(network)))])
    HC2 <- as.numeric(t(apply(HC, 2, sdd)))
    KC  <-
      as.data.frame(hub_score(network)$vector[order(names(hub_score(network)$vector))]) ################ Kleinberg's hub centrality scores.
    KC1 <-
      as.numeric(hub_score(network)$vector[order(names(hub_score(network)$vector))])
    KC2 <- as.numeric(t(apply(KC, 2, sdd)))
    
    ST  <-
      as.data.frame(strength(network)[order(names(strength(network)))]) ######################## Summing up the edge weights of the adjacent edges for each vertex.
    ST1 <-
      as.numeric(strength(network)[order(names(strength(network)))])
    ST2 <- as.numeric(t(apply(ST, 2, sdd)))
    
    
    write.table(
      "ID,Degree,Betweenness,Closeness,Alpha_Centrality,Eigen_centrality,KNN (Average nearest neighbor degree),Harmonic_centrality,Kleinberg's hub centrality,Strength (Summing up the edge weights of the adjacent edges for each vertex)",
      file = str_c(i, ".csv"),
      sep = ",",
      append = T,
      col.names = FALSE,
      row.names = FALSE,
      quote = FALSE
      
    )
    sortedNames <- V(network)$name[order(V(network)$name)]
    
    write.table(
      #directed standardized
      paste0(
        sortedNames,
        ",",
        DC2,
        ",",
        BC2,
        ",",
        CC2,
        ",",
        AC2,
        ",",
        EC2,
        ",",
        KNN2,
        ",",
        HC2,
        ",",
        KC2,
        ",",
        ST2
      ),
      
      file = str_c(i, ".csv"),
      sep = ",",
      append = T,
      col.names = FALSE,
      row.names = FALSE,
      quote = FALSE
      
    )
  }
  
  
} else if (cata == 999999) {
  for (i in c(1))
  {
    print(i)
    
    
    # for undirected
    network1  <- read_graph(str_c("FW", i, ".txt"), format = "dl")
    network <- as.undirected(network1, mode = "collapse")
    
    # for directed
    #network  <- read_graph(str_c("FW", i, ".txt"), format = "dl")
    
    DC  <-
      as.data.frame(degree(network)[order(names(degree(network)))])
    sdd <-
      function(x)
        (x - median(x)) / (quantile(x, 0.75) - quantile(x, 0.25))
    DC1 <-
      as.numeric(degree(network)[order(names(degree(network)))])
    DC2 <- as.numeric(t(apply(DC, 2, sdd)))
    
    BC  <-
      as.data.frame(betweenness(network)[order(names(betweenness(network)))])
    BC1 <-
      as.numeric(betweenness(network)[order(names(betweenness(network)))])
    BC2 <- as.numeric(t(apply(BC, 2, sdd)))
    
    # mark here if directed
    #CC  <- as.data.frame(closeness(network)[order(names(closeness(network)))])
    #CC1 <- as.numeric(closeness(network)[order(names(closeness(network)))])
    #CC2 <- as.numeric(t(apply(CC, 2, sdd)))
    
    AC  <-
      as.data.frame(alpha.centrality(network)[order(names(alpha.centrality(network)))])
    AC1 <-
      as.numeric(alpha.centrality(network)[order(names(alpha.centrality(network)))])
    AC2 <- as.numeric(t(apply(AC, 2, sdd)))
    
    #ECC <- as.data.frame(eccentricity(network))
    #ECC1<- as.numeric(eccentricity(network))
    #ECC2 <- as.numeric(t(apply(ECC, 2, sdd)))
    
    EC  <-
      as.data.frame(eigen_centrality(network)$vector[order(names(eigen_centrality(network)$vector))])
    EC1 <-
      as.numeric(eigen_centrality(network)$vector[order(names(eigen_centrality(network)$vector))])
    EC2 <- as.numeric(t(apply(EC, 2, sdd)))
    
    KNN <-
      as.data.frame(knn(network)$knn[order(names(knn(network)$knn))]) ######################### Average nearest neighbor degree
    KNN1 <-
      as.numeric(knn(network)$knn[order(names(knn(network)$knn))])
    KNN2 <- as.numeric(t(apply(KNN, 2, sdd)))
    
    HC  <-
      as.data.frame(harmonic_centrality(network)[order(names(harmonic_centrality(network)))])
    HC1 <-
      as.numeric(harmonic_centrality(network)[order(names(harmonic_centrality(network)))])
    HC2 <- as.numeric(t(apply(HC, 2, sdd)))
    KC  <-
      as.data.frame(hub_score(network)$vector[order(names(hub_score(network)$vector))]) ################ Kleinberg's hub centrality scores.
    KC1 <-
      as.numeric(hub_score(network)$vector[order(names(hub_score(network)$vector))])
    KC2 <- as.numeric(t(apply(KC, 2, sdd)))
    
    ST  <-
      as.data.frame(strength(network)[order(names(strength(network)))]) ######################## Summing up the edge weights of the adjacent edges for each vertex.
    ST1 <-
      as.numeric(strength(network)[order(names(strength(network)))])
    ST2 <- as.numeric(t(apply(ST, 2, sdd)))
    
    
    write.table(
      # undirected output string
      #"ID,Degree,Betweenness,Closeness,Alpha_Centrality,Eigen_centrality,KNN (Average nearest neighbor degree),Harmonic_centrality,Kleinberg's hub centrality,Strength (Summing up the edge weights of the adjacent edges for each vertex)",
      
      #directed output string
      "ID,Degree,Betweenness,Alpha_Centrality,Eigen_centrality,KNN (Average nearest neighbor degree),Harmonic_centrality,Kleinberg's hub centrality,Strength (Summing up the edge weights of the adjacent edges for each vertex)",
      file = str_c(i, ".csv"),
      sep = ",",
      append = T,
      col.names = FALSE,
      row.names = FALSE,
      quote = FALSE
      
    )
    sortedNames <- V(network)$name[order(V(network)$name)]
    
    write.table(
      # undirected raw
      #paste0(sortedNames,",",DC1,",",BC1,",",CC1,",",AC1,",",EC1,",",KNN1,",",HC1,",",KC1,",",ST1),
      
      # undirected standardized
      #paste0(sortedNames,",",DC2,",",BC2,"",CC2,",,",AC2,",",EC2,",",KNN2,",",HC2,",",KC2,",",ST2),
      
      #directed raw
      #paste0(sortedNames,",",DC1,",",BC1,",",AC1,",",EC1,",",KNN1,",",HC1,",",KC1,",",ST1),
      
      #directed standardized
      paste0(
        sortedNames,
        ",",
        DC2,
        ",",
        BC2,
        ",",
        AC2,
        ",",
        EC2,
        ",",
        KNN2,
        ",",
        HC2,
        ",",
        KC2,
        ",",
        ST2
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
