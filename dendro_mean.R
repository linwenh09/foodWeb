setwd(
  "C:/Users/Bryce/OneDrive/Documents/NTU_PHD/92FoodWeb/FoodWebDataset/new/undirected/standardized/"
)
library(stringr)
library(corrr)
library(ggplot2)
library(ggdendro)

print(1)
topoIndex <-
  read.csv(("1.csv"),row.names = 1)
topoIndex <- topoIndex[,-c(9)]
cc1 <-
  cor(topoIndex, use = "pairwise.complete.obs", method = "kendall")

mat_list <- list()
mat_list[[1]] <- cc1
cc_m <- matrix(0, nrow=8, ncol=8)

for (i in c(
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
  #cc1 <- cc + cc1
  
  mat_list[[i]] <- cc
  
}

#cc_m <- cc1 / 92
for(i in 1:8){
  for(j in 1:8){
    # Use sapply to obtain elements from the same position in each matrix, then calculate the median
    cc_m[i, j] <- median(sapply(mat_list, function(x) x[i, j]))
  }
}

rownames(cc_m) <- rownames(cc1)
colnames(cc_m) <- colnames(cc1)


png("Median.png", width = 960, height = 720)
#plot(hc)
hc <- hclust(dist(cc_m))
plot(
  ggdendrogram(
    hc,
    rotate = T,
    theme_dendro = T,
    labels = T
  ) + ggtitle("")
  + labs(x = "", y = NULL)
  + theme(
    axis.title.x = element_text(size = 20),
    axis.text.x  = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y  = element_text(size = 20),
    plot.title = element_text(size = 20),
  )
)
dev.off()
