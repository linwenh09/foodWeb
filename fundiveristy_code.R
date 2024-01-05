setwd("C://Users//Bryce//OneDrive//Documents//myLife//NTU_PHD//92FoodWeb//FoodWebDataset//new//undirected//standardized_0620")
library(stringr)
library(fundiversity)


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
)) {
  print(i)
  abun  <- read.csv(str_c("FW", i, "_abun.csv"),
                    row.names = 1,
                    stringsAsFactors = TRUE)
  abun <- as.matrix(abun)
  trait <- read.csv(
    str_c(i, ".csv"),  row.names = 1
  )
  #trait <- trait[,c(3)]
  trait <- as.matrix(trait)
  #trait <- trait[,c(1,2,4,5)]  ####### 挑特選的 indices ########
  trait <- trait[,c(1,2,4,8)]  ####### 挑特選的 indices ########
  
  
  
  raoValue <- fd_raoq(trait)
  write.table(
    raoValue$Q,
    #file = "fun_RaoQ_4indices.txt",
    file = "fun_RaoQ_DC_BC_AC_KC.txt",
    append = T,
    col.names = FALSE,
    row.names = FALSE,
    quote = FALSE
  )
  fdis <- fd_fdis(trait)
  write.table(
    fdis$FDis,
    #file = "fun_fdis_4indices.txt",
    file = "fun_fdis_DC_BC_AC_KC.txt",
    append = T,
    col.names = FALSE,
    row.names = FALSE,
    quote = FALSE
  )
  fdiv <- fd_fdiv(trait,abun)
  write.table(
    fdiv$FDiv,
    #file = "fun_fdiv_4indices.txt",
    file = "fun_fdiv_DC_BC_AC_KC.txt",
    append = T,
    col.names = FALSE,
    row.names = FALSE,
    quote = FALSE
  )
  feve <- fd_feve(trait)
  write.table(
    feve$FEve,
    #file = "fun_feve_4indices.txt",
    file = "fun_feve_DC_BC_AC_KC.txt",
    append = T,
    col.names = FALSE,
    row.names = FALSE,
    quote = FALSE
  )
  fric <- fd_fric(trait)
  write.table(
    fric$FRic,
    #file = "fun_fric_4indices.txt",
    file = "fun_fric_DC_BC_AC_KC.txt",
    append = T,
    col.names = FALSE,
    row.names = FALSE,
    quote = FALSE
  )
}