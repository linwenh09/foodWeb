library(stringr)
library(fundiversity)

for (i in 1:92) {
  for (j in 1:1000) {
    tryCatch({
      abun <- read.csv(
        str_c("networks/abun/FW", i, "_abun.csv"),
        row.names = 1,
        stringsAsFactors = TRUE
      )
      abun <- as.matrix(abun)
      trait <- read.csv(
        str_c("networks/centrality/FW", i, "_random_", j, "_centrality.csv"),
        row.names = 1
      )
      trait <- as.matrix(trait)
      trait <- trait[, c(1, 2, 3, 4)]  # choose which centrality
      
      # compute functional diversity
      raoValue <- fd_raoq(trait)
      write.table(
        str_c(raoValue$Q,"\tFW",i,"\tRandom_",j),
        file = "fun_RaoQ.txt",
        append = TRUE,
        col.names = FALSE,
        row.names = FALSE,
        quote = FALSE
      )
      fdis <- fd_fdis(trait)
      write.table(
        str_c(fdis$FDis,"\tFW",i,"\tRandom_",j),
        file = "fun_fdis.txt",
        append = TRUE,
        col.names = FALSE,
        row.names = FALSE,
        quote = FALSE
      )
      fdiv <- fd_fdiv(trait, abun)
      write.table(
        str_c(fdiv$FDiv,"\tFW",i,"\tRandom_",j),
        file = "fun_fdiv.txt",
        append = TRUE,
        col.names = FALSE,
        row.names = FALSE,
        quote = FALSE
      )
      feve <- fd_feve(trait)
      write.table(
        str_c(feve$FEve,"\tFW",i,"\tRandom_",j),
        file = "fun_feve.txt",
        append = TRUE,
        col.names = FALSE,
        row.names = FALSE,
        quote = FALSE
      )
      fric <- fd_fric(trait)
      write.table(
        str_c(fric$FRic,"\tFW",i,"\tRandom_",j),
        file = "fun_fric.txt",
        append = TRUE,
        col.names = FALSE,
        row.names = FALSE,
        quote = FALSE
      )
    }, error = function(e) {
      # when error happens
      message <- paste("Error in network FW", i, "random", j, ": ", e$message)
      print(message)
      # skipping the current network, next
    })
  }
}
