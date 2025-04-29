################################
## Load required packages
################################
library(readxl)
library(dplyr)
library(stringr)
library(caret)
library(prospectr)
library(ggplot2)
library(randomForest)
library(writexl)
library(tidyr)

if (!dir.exists("figures")) dir.create("figures")
if (!dir.exists("scores")) dir.create("scores")

## Save confusion matrix as image
confusion_matrix <- function(real, pred, name) {
  conf_matrix <- table (Predicted = pred, Real = real)
  conf_pct <- prop.table(conf_matrix, margin = 2) * 100
  conf_df <- as.data.frame(conf_pct)
  colnames(conf_df) <- c("Predicted", "Real", "Percentage")
  
  p <- ggplot(conf_df, aes(x = Real, y = Predicted, fill = Percentage)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.1f%%", Percentage)), color = "black", size = 5) +
    scale_fill_gradient(low = "white", high = "darkgreen") +
    labs(title = paste("Confusion Matrix -", name), x = "Real Class", y = "Predicted") +
    theme_minimal()
  
  ggsave(paste0("figures/confusion_rf_", gsub(" ", "_", tolower(name)), ".png"), 
         p, width = 6, height = 5, dpi = 300)
}

# Save scores to Excel and Kappa to CSV
scores <- function(real, pred, name, train_real = NULL, train_pred = NULL) {
  conf <- confusionMatrix(pred, real)
  scores_df <- as.data.frame(conf$byClass)
  scores_df$Class <- rownames(scores_df)
  scores_df <- scores_df[, c("Class", setdiff(names(scores_df), "Class"))]
  write_xlsx(scores_df, paste0("scores/scores_rf_", gsub(" ", "_", tolower(name)), ".xlsx"))
  kappa_val <- data.frame(Set = "Test", Kappa = conf$overall["Kappa"])
  if (!is.null(train_real) && !is.null(train_pred)) {
    kappa_train <- confusionMatrix(train_pred, train_real)$overall["Kappa"]
    kappa_val <- rbind(kappa_val, data.frame(Set = "Train", Kappa = kappa_train))
  }
  
  write_xlsx(kappa_val, paste0("scores/kappa_rf_", gsub(" ", "_", tolower(name)), ".xlsx"))
}


#Train and Evaluation RF

pipeline_rf_test <- function(X_train, Y_train, X_test, Y_test, name_model) {
  cat("\n--------------------------------------------------\n")
  cat(paste("📊 Executing RF for:", name_model, "\n"))
  cat("--------------------------------------------------\n")
  
  df_train <- as.data.frame(X_train)
  df_train$Class <- Y_train
  df_test <- as.data.frame(X_test)
  colnames(df_train) <- make.names(colnames(df_train))
  colnames(df_test) <- make.names(colnames(df_test))
  
  model_rf <- randomForest(Class ~ ., data = df_train, importance = TRUE, ntree = 100, mtry = 5)
  pred_rf <- predict(model_rf, newdata = df_test)
  pred_train <- predict(model_rf, newdata = df_train)
  
  acc_test <- mean(pred_rf == Y_test)
  acc_train <- mean(pred_train == Y_train)
  
  confusion_matrix(Y_test, pred_rf, name_model)
  scores (Y_test, pred_rf, name_model, Y_train, pred_train)
  
  cat(sprintf("✅ Accuracy TRAIN (%s): %.2f%%\n", name_model, acc_train * 100))
  cat(sprintf("✅ Accuracy TEST  (%s): %.2f%%\n", name_model, acc_test * 100))
  
  # Drift times importance
  importance_df <- as.data.frame(importance(model_rf))
  importance_df$Drift_time <- as.numeric(gsub("X", "", rownames(importance_df)))
  importance_df <- importance_df[order(-importance_df$MeanDecreaseGini), ]
  write_xlsx(importance_df, paste0("scores/importance_rf_", gsub(" ", "_", tolower(name_model)), ".xlsx"))
  
  top_n <- 30
  g <- ggplot(importance_df[1:top_n, ], aes(x = reorder(Drift_time, MeanDecreaseGini), y = MeanDecreaseGini)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = paste("Top Wavelenghts -", name_model),
         x = "Wavelenghts (nm)", y = "Importance (Gini)") +
    theme_minimal()
  
  ggsave(paste0("scores/importance_rf_", gsub(" ", "_", tolower(name_model)), ".png"), g, width = 7, height = 5, dpi = 300)
  
  return(data.frame(model = name_model, Accuracy_Train = acc_train * 100, Accuracy_Test = acc_test * 100))
}

#######################################
## Load Data
#######################################

raw_data <- read_excel("C:/~~/~~/raw_data_superv.xlsx")

numeric_cols <- sapply(raw_data, is.numeric)
X_raw <- as.matrix(raw_data[, numeric_cols])
Y <- as.factor(raw_data$GR)

set.seed(123)
split_index <- createDataPartition(Y, p = 0.7, list = FALSE)
Y_train <- Y[split_index]
Y_test <- Y[-split_index]

##############################################
## Execute RF for the different pretreatments
##############################################

results <- list()

# 1) Raw data
X_train <- X_raw[split_index, ]; X_test <- X_raw[-split_index, ]
results[["Raw"]] <- pipeline_rf_test(X_train, Y_train, X_test, Y_test, "Raw")

# 2) Normalization to maximum (per row)

X_norm <- X_raw / apply(X_raw, 1, max)
X_train <- X_norm[split_index, ]; X_test <- X_norm[-split_index, ]
results[["Normalization to maximum"]] <- pipeline_rf_test(X_train, Y_train, X_test, Y_test, "Normalization to maximum")

# 3) Savitzky-Golay
X_sg <- savitzkyGolay(X_raw, m = 0, p = 2, w = 11)
X_train <- X_sg[split_index, ]; X_test <- X_sg[-split_index, ]
results[["Savitzky-Golay"]] <- pipeline_rf_test(X_train, Y_train, X_test, Y_test, "Savitzky-Golay")

# 4) First derivative with Savitzky-Golay
preprocess_first_derivative <- function(X) {
  savitzkyGolay(X, m = 1, p = 2, w = 11)
}
X_fd <- preprocess_first_derivative(as.matrix(X_raw))
X_train <- X_fd[split_index, ]; X_test <- X_fd[-split_index, ]
results[["First derivative"]] <- pipeline_rf_test(X_train, Y_train, X_test, Y_test, "First derivative")


# 5) Second derivative with Savitzky-Golay
preprocess_second_derivative <- function(X) {
  savitzkyGolay(X, m = 2, p = 2, w = 11)
}
X_sd <- preprocess_second_derivative(as.matrix(X_raw))
X_train <- X_sd[split_index, ]; X_test <- X_sd[-split_index, ]
results[["Second derivative"]] <- pipeline_rf_test(X_train, Y_train, X_test, Y_test,"Second derivative")


# 6) SNV (Standard Normal Variate)
preprocess_snv <- function(X) {
  standardNormalVariate(X)
}
X_snv <- preprocess_snv(as.matrix(X_raw))
X_train <- X_snv[split_index, ]; X_test <- X_snv[-split_index, ]
results[["SNV"]] <- pipeline_rf_test(X_train, Y_train, X_test, Y_test, "SNV")

# 7) MSC
X_msc <- msc(X_raw)
X_train <- X_msc[split_index, ]; X_test <- X_msc[-split_index, ]
results[["MSC"]] <- pipeline_rf_test(X_train, Y_train, X_test, Y_test, "MSC")


results_df <- do.call(rbind, results)
print(results_df)
write_xlsx(results_df, "scores/accuracy_summary_rf_train_test.xlsx")

summary <- bind_rows(results)
summary_long <- summary %>% pivot_longer(cols = starts_with("Accuracy"), names_to = "Type", values_to = "Value")
plot_acc <- ggplot(summary_long, aes(x = reorder(model, value), y = value, fill = Tipo)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Accuracy per pretreatment (Train vs Test)", x = "Pretreatment", y = "Accuracy (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("Accuracy_Train" = "#1f77b4", "Accuracy_Test" = "#ff7f0e"))

print(plot_acc)
ggsave("figures/accuracy_rf_train_vs_test_.png", plot_acc, width = 8, height = 5, dpi = 300)


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
