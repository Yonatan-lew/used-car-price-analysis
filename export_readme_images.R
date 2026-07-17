# ============================================================
# EXPORT PLOTS FOR GITHUB README — Used Car Price project
# Run this AFTER running your full car_price_analysis.R
# (objects cars, dati, cor_ma, km_k2, km_k3, Cars_sd must exist)
# It creates an "images" folder with the 4 PNGs the README uses.
# ============================================================

library(ggplot2)
library(cowplot)
library(factoextra)
library(corrplot)

dir.create("images", showWarnings = FALSE)

# 1. Price distribution
p <- ggplot(cars, aes(x = selling_price)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of Selling Price", x = "selling price", y = "")
ggsave("images/price_distribution.png", p, width = 9, height = 5, dpi = 150)

# 2. Correlation matrix
png("images/correlation_matrix.png", width = 1200, height = 1000, res = 150)
corrplot(cor_ma,
         method = "color", type = "upper", order = "hclust", addrect = 3,
         col = colorRampPalette(c("red", "white", "blue"))(200),
         tl.col = "black", tl.cex = 0.9, number.cex = 0.8, addCoef.col = "black")
dev.off()

# 3. K-Means clusters (K=2 vs K=3)
p <- plot_grid(
  fviz_cluster(km_k2, data = Cars_sd, geom = "point") + ggtitle("K-Means K=2"),
  fviz_cluster(km_k3, data = Cars_sd, geom = "point") + ggtitle("K-Means K=3")
)
ggsave("images/kmeans_clusters.png", p, width = 12, height = 5, dpi = 150)

# 4. Price by cluster
p <- ggplot(cars, aes(x = Cluster, y = log(selling_price), fill = Cluster)) +
  geom_boxplot(alpha = 0.7) + theme_minimal() +
  labs(title = "log(Selling Price) by Cluster", x = "Cluster", y = "log price")
ggsave("images/price_by_cluster.png", p, width = 8, height = 5, dpi = 150)

cat("Done! Check the images folder.\n")
