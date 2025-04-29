################################
## Load required packages
################################

library(readxl)
library(dplyr)
library(stringr)
library(prospectr)
library(factoextra)
library(ggplot2)
library(stats)
library(purrr)
library(rcartocolor)
library(caret)
library(cluster)
library(ggdendro)
library(ape)
library (writexl)
library(pheatmap)

#######################################
## Load Data
#######################################

raw_data <- read_excel("C:/~~/~~/raw_data.xlsx")

## average of replicas and group by base ID
raw_data$ID <- gsub("_R1|_R2", "", raw_data$ID)

#Create Origin column
raw_data$Origin <- str_sub(raw_data$ID, 1, 2)

## Convert spectral columns to numeric
raw_data[, !(names(raw_data) %in% c("ID", "Origin"))] <- apply(raw_data[, !(names(raw_data) %in% c("ID", "Origin"))], 2, as.numeric)

raw_avg <- raw_data %>%
  group_by(ID, Origin) %>%
  summarise(across(where(is.numeric), mean), .groups = "drop")

# Save labels
raw_labels <- raw_avg$ID
raw_matrix <- raw_avg[, -1]


##########################################
## Hierarchical Analysis of Cluster (HCA)
##########################################
## PREPROCESSING (centring and scaling)
preprocessor <- preProcess(raw_matrix, method = c("center", "scale"))
data_transformed <- predict(preprocessor, raw_matrix)


### OPTIMAL CLUSTERING METHOD

linkage_methods <- c("average", "single", "complete", "ward")

agglomerative_coefficients <- function(method) {
  agnes(data_transformed, method = method)$ac
}

names(linkage_methods) <- linkage_methods

linkage_scores <- map_dbl(linkage_methods, agglomerative_coefficients)
names(linkage_scores) <- linkage_methods

print(linkage_scores)

best_method <- names(which.max(linkage_scores))
cat("\n✅  Best linkage method:", best_method, "\n")


## Hierarchical Cluster Analysis
distance_matrix <- dist(data_transformed, method = "euclidean")
hclust_method <- ifelse(best_method == "ward", "ward.D2", best_method)
hc_best <- hclust(distance_matrix, method = best_method)
hc_best$labels <- raw_labels

## Optimal number of cluster (Silhouette)

silhouette_plot <- fviz_nbclust(
  data_transformed,
  FUN = hcut,
  method = "silhouette",
  diss = dist(data_transformed)
) +
  labs(title = "Silhouette Method for Optimal Clusters") +
  theme_minimal()

ggsave("figures/silhouette_plot.png", silhouette_plot, width = 10, height = 6, dpi = 300)
print(silhouette_plot)


## Dendrogram 

ordered_labels <- hc_best$labels[hc_best$order]
origin <- substr(ordered_labels, 1, 2)

origin_color <- c("ES" = "#F72108", "IT" = "#F79500", "MR" = "#A717E8", "PT" = "#1BAB18")
label_colors <- origin_color[origin]

dendrogram <- fviz_dend(
  hc_best,
  k = 4,
  show_labels = TRUE,
  cex = 0.7,
  lwd = 0.7,
  label_cols = label_colors,
  k_colors = c("#66c2a5", "#fc8d62", "#8da0cb","#E89820"),
  rect = TRUE,
  rect_fill=TRUE,
  main = "Dendrogram",
  ggtheme = theme_minimal()
)

ggsave("figures/hca_plot.png", dendrogram, width = 10, height = 6, dpi = 300)
print(dendrogram)


# Phylogenetic type tree
tree_phylo <- as.phylo(hc_best)
origin_phylo <- substr(tree_phylo$tip.label, 1, 2)
origin_color <- c("ES" = "#F72108", "IT" = "#F79500", "MR" = "#A717E8", "PT" = "#1BAB18")
tip_colors <- origin_color [origin_phylo]

png("figures/phylogenetic_tree.png", width = 8, height = 8, units = "in", res = 300)
par(mar = c(1, 1, 3, 1))
plot(tree_phylo,
     type = "fan",
     tip.color = tip_colors,
     cex = 0.8,
     label.offset = 0.1,
     no.margin = TRUE,
     main = "Phylogenetic dendrogram")
dev.off()


##########################################
## Principal Component Analysis (PCA)
##########################################

## Create scores folder
if (!dir.exists("scores")) dir.create("scores")

spectra_matrix <- raw_avg %>%
  select(-(ID:Origin)) %>%
  as.matrix()

# Delete standard deviation zero
spectra_matrix <- spectra_matrix[, apply(spectra_matrix, 2, function(x) sd(x, na.rm = TRUE) != 0)]


pca <- function(spectra_matrix, meta_data, title, filename_plot, filename_scores) {
  pca_result <- prcomp(spectra_matrix, center = TRUE, scale. = TRUE)
  
  scores <- as.data.frame(pca_result$x[, 1:2])
  scores$ID <- meta_data$ID
  scores$Origin <- meta_data$Origin
  
  var_exp <- round(100 * summary(pca_result)$importance[2, 1:2], 1)
  
  p <- ggplot(scores, aes(x = PC1, y = PC2, color = Origin)) +
    geom_point(size = 3, alpha = 0.8) +
    stat_ellipse(aes(group = Origin), linetype = "dotted", level = 0.9) +
    labs(
      title = paste0(title, "\nPCA: Variance explained: PC1=", var_exp[1], "%, PC2=", var_exp[2], "%"),
      x = paste0("PC1 (", var_exp[1], "%)"),
      y = paste0("PC2 (", var_exp[2], "%)")
    ) +
    theme_minimal() +
    scale_color_manual(name = "Origin", values = origin_color)
  
  print(p)
  ggsave(file.path("figures", filename_plot), p, width = 8, height = 6, dpi = 300)
  
  write_xlsx(scores, file.path("scores", filename_scores))
  
  return(pca_result)
}

raw_pca <- pca(spectra_matrix, raw_avg, "PCA - RAW", "pca_raw.png", "scores_raw.xlsx")

## Function for calculating mean distance between centroids
centroids_distance_summary <- function(pca_obj, meta_data) {
  scores <- as.data.frame(pca_obj$x[, 1:2])
  scores$ID <- meta_data$ID
  scores$Origin <- meta_data$Origin
  
  centroids <- scores %>%
    group_by(Origin) %>%
    summarise(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop")
  
  dist_matrix <- dist(centroids[, c("PC1", "PC2")])
  
  mean_dist <- mean(as.matrix(dist_matrix)[upper.tri(dist_matrix)])
  
  return(data.frame(Mean_Centroid_Distance = mean_dist))
}


centroids_distance_matrix <- function(pca_obj, meta_data) {
  scores <- as.data.frame(pca_obj$x[, 1:2])
  scores$ID <- meta_data$ID
  scores$Origin <- meta_data$Origin
  
  centroids <- scores %>%
    group_by(Origin) %>%
    summarise(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop")
  
  dist_matrix <- dist(centroids[, c("PC1", "PC2")])
  dist_matrix_df <- as.data.frame(as.matrix(dist_matrix))
  
  rownames(dist_matrix_df) <- centroids$Origin
  colnames(dist_matrix_df) <- centroids$Origin
  
  return(dist_matrix_df)
}

#Mean Distance
distance_summary <- centroids_distance_summary(raw_pca, raw_avg)
print(distance_summary)
write_xlsx(distance_summary, "scores/centroids_mean_distance.xlsx")

#Individual Distances
distance_matrix <- centroids_distance_matrix(raw_pca, raw_avg)
print(distance_matrix)
write_xlsx(distance_matrix, "scores/centroids_distance_matrix.xlsx")

#Heatmap with individual distances
pheatmap(distance_matrix,
         cluster_rows = FALSE, 
         cluster_cols = FALSE,
         display_numbers = TRUE,
         main = "Distances between Centroids")


############################################################
##  Automatically save loaded packages and their versions
############################################################

loaded_pkgs <- sessionInfo()$otherPkgs

pkg_versions <- sapply(loaded_pkgs, function(pkg) {
  paste0(pkg$Package, "==", pkg$Version)
})

req_file <- "requirements.txt"

if (file.exists(req_file)) {
  existing_lines <- readLines(req_file)
  existing_pkgs <- sub("(.*)==.*", "\\1", existing_lines)
} else {
  existing_pkgs <- character(0)
}

new_pkgs <- pkg_versions[!(names(pkg_versions) %in% existing_pkgs)]


if (length(new_pkgs) > 0) {
  cat(new_pkgs, file = req_file, sep = "\n", append = TRUE)
  message("New packaged added to requirementss.txt")
} else {
  message("No new packages to add.")
}

cat(paste0(new_pkgs, "\n"), file = req_file, sep = "", append = TRUE)