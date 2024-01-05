setwd(
  "C:/Users/Bryce/OneDrive/Documents/NTU_PHD/92FoodWeb/FoodWebDataset/new/undirected/standardized/"
)
library(stringr)
library(corrr)
library(ggplot2)
library(ggdendro)

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
  topoIndex <-
    read.csv(str_c(i, ".csv"),
             row.names = 1)
  topoIndex <- topoIndex[,-c(9)]
  cc <-
    cor(topoIndex, use = "pairwise.complete.obs", method = "kendall")
  
  
  
  png(str_c(i, ".png"), width = 960, height = 720)
  #plot(hc)
  hc <- hclust(dist(cc))
  plot(
    ggdendrogram(
      hc,
      rotate = T,
      theme_dendro = T,
      labels = T
    ) + ggtitle(str_c("FW", i))
    + labs(x = "Speices", y = NULL)
    + theme(
      axis.title.x = element_text(size = 20),
      axis.text.x  = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.y  = element_text(size = 20),
      plot.title = element_text(size = 20),
    )
  )
  dev.off()
}
