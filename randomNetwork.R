library(igraph)

# read the excel file for each network's structure
food_webs <- readxl::read_excel("foodWeb_structures.xlsx")

# name the nodes of each random network after order of alphabets
number_to_letters <- function(num) {
  if (num <= 26) {
    return(LETTERS[num])
  } else {
    return(paste0(LETTERS[(num - 1) %/% 26 + 1], LETTERS[(num - 1) %% 26 + 1]))
  }
}

# function: generate random networks keeping the properties
generate_save_and_analyze_random_networks <- function(food_web_id, num_nodes, connectance) {
  # if the folder "networks" doesn't exist, then create one
  if (!dir.exists("networks")) {
    dir.create("networks")
  }
  
  for (i in 1:1000) {
    success <- FALSE
    while (!success) {
      # create a graph with names
      node_names <- sapply(1:num_nodes, number_to_letters)
      g <- make_empty_graph(n = num_nodes, directed = FALSE)
      V(g)$name <- node_names
      
      # create edge by connectance randomly
      for (source in 1:(num_nodes - 1)) {
        for (target in (source + 1):num_nodes) {
          if (runif(1) < connectance) {
            g <- add_edges(g, c(node_names[source], node_names[target]))
          }
        }
      }
      
      # remove duplicate edges
      g <- simplify(g)
      
      tryCatch({
        # compute centralities
        deg_centrality <- degree(g)
        betw_centrality <- betweenness(g)
        eigen_centrality <- eigen_centrality(g)$vector
        alpha_centrality <- alpha_centrality(g)
        
        # save centralies into dateframe
        centrality_data <- data.frame(
          node = node_names,
          degree = deg_centrality,
          betweenness = betw_centrality,
          eigenvector = eigen_centrality,
          alpha = alpha_centrality
        )
        
        # save centralities as a csv file
        network_name <- paste0(food_web_id, "_random_", i)
        file_name <- paste0("networks/centrality/", network_name, "_centrality.csv")
        write.csv(centrality_data, file_name, row.names = FALSE)
        print(str_c(food_web_id,"_",i))
        
        # save randomly-generated edges
        network_file_name <- paste0("networks/", network_name, ".txt")
        write.table(get.edgelist(g), file = network_file_name, row.names = FALSE, col.names = FALSE, quote = FALSE)
        
        success <- TRUE
      }, error = function(e) {
        # if error, then re-do
        success <- FALSE
      })
    }
  }
}

# for each foodweb, create random networks
lapply(1:nrow(food_webs), function(idx) {
  row <- food_webs[idx, ]
  generate_save_and_analyze_random_networks(row$FoodWebID, row$Node, row$connectance)
})
