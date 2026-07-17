# ==========================================================
# USED CAR PRICE ANALYSIS
# Techniques: Linear Regression + Cluster Analysis (PCA, K-Means, HAC)
# ==========================================================

# ==========================================================
# SETUP AND LIBRARIES
# ==========================================================
rm(list = ls())                                  # clear environment
while (!is.null(dev.list())) dev.off()           # close all plot windows
set.seed(1234)                                   # reproducibility

library(ggplot2)
library(dplyr)
library(tidyverse)
library(magrittr)
library(cluster)     # Gower and PAM
library(cowplot)     # grid visualization
library(gridExtra)   # advanced grids
library(NbClust)     # optimal K
library(factoextra)  # PCA / cluster visualization
library(FactoMineR)  # PCA calculation
library(corrplot)
library(dendextend)  # dendrograms
library(car)         # VIF
library(lmtest)      # BP and DW tests

# Optional extras (skipped automatically if not installed):
#   install.packages(c("naniar","clValid"))
has_naniar  <- requireNamespace("naniar",  quietly = TRUE)
has_clvalid <- requireNamespace("clValid", quietly = TRUE)

# ==========================================================
# DATA LOADING AND PREPARATION
# ==========================================================
# Set the working directory to the folder that contains this script and cars.csv.
# (adjust the path if you move the project)
# Run this script from the project folder (data goes in a "data" subfolder)
cars_raw <- read.csv("data/cars.csv", sep = ",", stringsAsFactors = FALSE)

str(cars_raw)
print(paste(">> Raw dimensions:", nrow(cars_raw), "x", ncol(cars_raw)))

# ---- Cleaning: the units columns are stored as text ("23.4 kmpl", "1248 CC",
#      "74 bhp") so we strip the units and convert them to numbers. ----
clean_num <- function(x) as.numeric(gsub("[^0-9.]", "", x))

cars <- cars_raw %>%
  mutate(
    brand        = word(name, 1),            # first word of the model name = brand
    mileage      = clean_num(mileage),       # kmpl
    engine       = clean_num(engine),        # CC
    max_power    = clean_num(max_power),      # bhp
    car_age      = 2020 - year               # dataset was collected around 2020
  ) %>%
  # torque is too inconsistent to parse reliably -> dropped; name replaced by brand
  select(-name, -torque, -year)

# Test Drive Cars are a tiny, atypical group -> remove (5 rows)
cars <- cars %>% filter(owner != "Test Drive Car")

# ---- Missing values ----
if (has_naniar) print(naniar::gg_miss_var(cars) + labs(title = "Missing values per variable"))
print(colSums(is.na(cars)))

# rows with NA are a small fraction -> drop them for complete-case analysis
cars <- na.omit(cars)

# convert categoricals to factors
cat_vars <- c("fuel", "seller_type", "transmission", "owner", "brand")
cars[cat_vars] <- lapply(cars[cat_vars], as.factor)

# keep only brands with enough observations so factor levels are stable
big_brands <- names(which(table(cars$brand) >= 30))
cars <- cars %>% filter(brand %in% big_brands) %>% droplevels()

print(paste(">> Clean dimensions:", nrow(cars), "x", ncol(cars)))
str(cars)
attach(cars)

n   <- nrow(cars)
rel <- 100 / n   # relative percentage factor

# ==========================================================
# EXPLORATORY / DESCRIPTIVE ANALYSIS
# ==========================================================

# selling price (target)
summary(selling_price)
ggplot(cars, aes(x = selling_price)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of Selling Price", x = "selling price", y = "")
boxplot(selling_price, main = "Selling Price", col = "steelblue")

# log price is much closer to symmetric -> we will model log(price)
ggplot(cars, aes(x = log(selling_price))) +
  geom_histogram(bins = 40, fill = "darkgreen", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of log(Selling Price)", x = "log selling price", y = "")

# car age
summary(car_age)
boxplot(car_age, main = "Car Age (years)", col = "lightblue")

# km driven
summary(km_driven)
boxplot(km_driven, main = "Km Driven", col = "plum")

# fuel type (pie chart, as in the templates)
fuel_df <- cars %>%
  count(fuel) %>%
  mutate(percent = n / sum(n) * 100,
         label   = paste0(round(percent, 1), "%"))

ggplot(fuel_df, aes(x = "", y = percent, fill = fuel)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_label(aes(label = label), color = "white",
             position = position_stack(vjust = 0.5), show.legend = FALSE) +
  theme_void() +
  labs(title = "Fuel Type Distribution")

# transmission
table(transmission)
ggplot(cars, aes(x = transmission)) +
  geom_bar(width = 0.5, fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Transmission", x = "transmission", y = "")

# owner
table(owner)
ggplot(cars, aes(x = owner)) +
  geom_bar(width = 0.6, fill = "lightblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Number of Previous Owners", x = "", y = "")

# seller type
table(seller_type)

# price by transmission and fuel
ggplot(cars, aes(x = transmission, y = log(selling_price), fill = transmission)) +
  geom_boxplot(alpha = 0.7) + theme_minimal() +
  labs(title = "log(Price) by Transmission", y = "log selling price")

ggplot(cars, aes(x = fuel, y = log(selling_price), fill = fuel)) +
  geom_boxplot(alpha = 0.7) + theme_minimal() +
  labs(title = "log(Price) by Fuel Type", y = "log selling price")

# ---- correlation among numeric variables ----
dati <- cars[, c("selling_price", "km_driven", "mileage",
                 "engine", "max_power", "seats", "car_age")]
cor_ma <- cor(dati)
corrplot(cor_ma,
         method = "color", type = "upper", order = "hclust", addrect = 3,
         col = colorRampPalette(c("red", "white", "blue"))(200),
         tl.col = "black", tl.cex = 0.9, number.cex = 0.8, addCoef.col = "black")

# ==========================================================
# TECHNIQUE 1 — LINEAR REGRESSION
# Target: log(selling_price)
# ==========================================================

cars$log_price <- log(cars$selling_price)

# null and full models
modnull <- lm(log_price ~ 1, data = cars)
summary(modnull)

modfull <- lm(log_price ~ car_age + km_driven + mileage + engine + max_power +
                seats + fuel + seller_type + transmission + owner + brand,
              data = cars)
summary(modfull)

# ---- forward selection (as in the Mental_Health template) ----
m0 <- lm(log_price ~ 1, data = cars)
add1(m0,
     scope = ~ car_age + km_driven + mileage + engine + max_power +
       seats + fuel + seller_type + transmission + owner + brand,
     data = cars, test = "F")

# build up the model with the strongest predictors
m1 <- lm(log_price ~ max_power, data = cars)
summary(m1)

m2 <- lm(log_price ~ max_power + car_age, data = cars)
summary(m2)

m3 <- lm(log_price ~ max_power + car_age + transmission, data = cars)
summary(m3)

# automatic stepwise selection (both directions) on the full model for comparison
m_step <- step(modfull, direction = "both", trace = 0)
summary(m_step)

# model comparison table
print(data.frame(
  Model  = c("null", "m1", "m2", "m3", "full", "stepwise"),
  Adj_R2 = round(c(summary(modnull)$adj.r.squared, summary(m1)$adj.r.squared,
                   summary(m2)$adj.r.squared,      summary(m3)$adj.r.squared,
                   summary(modfull)$adj.r.squared, summary(m_step)$adj.r.squared), 3),
  AIC    = round(c(AIC(modnull), AIC(m1), AIC(m2), AIC(m3),
                   AIC(modfull), AIC(m_step)), 1)
))

# ---- residual diagnostics on the chosen model ----
final_model <- m_step
e <- residuals(final_model)
shapiro.test(e[sample(length(e), min(5000, length(e)))])  # normality (Shapiro caps at 5000)
plot(fitted(final_model), e, pch = 20, col = rgb(0, 0, 0, 0.3),
     xlab = "Fitted", ylab = "Residuals", main = "Residuals vs Fitted")
abline(h = 0, col = "red")
qqnorm(e); qqline(e, col = "red")
car::vif(final_model)   # multicollinearity
dwtest(final_model)     # autocorrelation
bptest(final_model)     # Breusch-Pagan heteroscedasticity

# fitted vs actual (log scale)
plot(fitted(final_model), cars$log_price,
     xlab = "Fitted (log scale)", ylab = "Actual (log scale)",
     main = "Fitted vs Actual", pch = 20, col = rgb(0, 0, 0, 0.3))
abline(0, 1, col = "red", lwd = 2)

# ==========================================================
# TECHNIQUE 2 — CLUSTER ANALYSIS (PCA, K-Means, HAC)
# Segment cars by their numeric characteristics.
# ==========================================================

# cluster on the numeric features EXCLUDING the price (so price doesn't drive
# the segments — we instead see how price differs across the segments afterwards)
dati_pred <- dati[, setdiff(names(dati), "selling_price")]
Cars_sd   <- scale(dati_pred)

# ---- PCA ----
res.pca <- PCA(dati_pred, graph = FALSE)
summary(res.pca)
eig_val <- get_eigenvalue(res.pca)
print(round(eig_val, 3))

# Kaiser rule: keep components with eigenvalue > 1
k_keep <- sum(eig_val[, 1] > 1)
cat(">> Kaiser rule: keep", k_keep, "PCs |",
    round(eig_val[k_keep, 3], 2), "% variance\n")

fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 60), title = "PCA: Explained Variance")
grid.arrange(
  fviz_contrib(res.pca, choice = "var", axes = 1, top = 6, title = "Contrib. to PC1"),
  fviz_contrib(res.pca, choice = "var", axes = 2, top = 6, title = "Contrib. to PC2"),
  ncol = 2
)
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("blue", "orange", "red"),
             repel = TRUE, title = "Correlation Circle (PC1-PC2)")

# ---- optimal number of clusters ----
fviz_nbclust(Cars_sd, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) + labs(subtitle = "Elbow Method")
fviz_nbclust(Cars_sd, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette Method")

# NbClust / gap stat are slow on the full data -> run on a subsample
set.seed(42)
sub_idx  <- sample(nrow(Cars_sd), 800)
Cars_sub <- Cars_sd[sub_idx, ]
res.nbclust <- NbClust(Cars_sub, distance = "euclidean",
                       min.nc = 2, max.nc = 6, method = "kmeans", index = "all")

# ---- hierarchical clustering (on the subsample, for readable dendrograms) ----
dist_E    <- dist(Cars_sub)
hclust_co <- hclust(dist_E, method = "complete")
hclust_wa <- hclust(dist_E, method = "ward.D2")

plot(hclust_co, main = "Complete Linkage", xlab = "", sub = "", labels = FALSE)
plot(hclust_wa, main = "Ward's Method",    xlab = "", sub = "", labels = FALSE)
rect.hclust(hclust_wa, k = 2, border = "red")
rect.hclust(hclust_wa, k = 3, border = "blue")

dend_w <- as.dendrogram(hclust_wa)
plot(color_branches(dend_w, k = 3, groupLabels = TRUE), main = "Ward Dendrogram (K=3)")

# ---- final K-Means (on the full data) ----
set.seed(123)
km_k2 <- kmeans(Cars_sd, centers = 2, nstart = 30)
km_k3 <- kmeans(Cars_sd, centers = 3, nstart = 30)

plot_grid(
  fviz_cluster(km_k2, data = Cars_sd, geom = "point") + ggtitle("K-Means K=2"),
  fviz_cluster(km_k3, data = Cars_sd, geom = "point") + ggtitle("K-Means K=3")
)

# proportion of total variance explained by cluster separation
cat(">> K-Means BSS/TSS  K=2:", round(km_k2$betweenss / km_k2$totss, 3),
    " | K=3:", round(km_k3$betweenss / km_k3$totss, 3), "\n")

# ---- profile the clusters ----
cars$Cluster <- as.factor(km_k3$cluster)

cluster_profile <- aggregate(dati, list(Cluster = km_k3$cluster), mean)
cluster_profile[, -1] <- round(cluster_profile[, -1], 1)
print(cluster_profile)              # how each segment looks (incl. average price)
print(table(km_k3$cluster))

ggplot(cars, aes(x = Cluster, y = log(selling_price), fill = Cluster)) +
  geom_boxplot(alpha = 0.7) + theme_minimal() +
  labs(title = "log(Selling Price) by Cluster", x = "Cluster", y = "log price")

# ---- validation (internal) on a subsample ----
if (has_clvalid) {
  intern <- clValid::clValid(Cars_sub, nClust = 2:3,
                             clMethods = c("hierarchical", "kmeans"),
                             validation = "internal")
  summary(intern)
}

# ==========================================================
# END
# ==========================================================
