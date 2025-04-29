################################
## Load required packages
################################
library(caret)      
library(randomForest) 
library(ggplot2)  
library(writexl)
library(prospectr)
library(readxl)

if (!dir.exists("metrics")) dir.create("metrics")
if (!dir.exists("figures")) dir.create("figures")


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

# Save metrics to Excel and Kappa to CSV
metrics <- function(real, pred, name, train_real = NULL, train_pred = NULL) {
  conf <- confusionMatrix(pred, real)
  metrics_df <- as.data.frame(conf$byClass)
  metrics_df$Class <- rownames(metrics_df)
  metrics_df <- metrics_df[, c("Class", setdiff(names(metrics_df), "Class"))]
  write_xlsx(metrics_df, paste0("metrics/metrics_rf_", gsub(" ", "_", tolower(name)), ".xlsx"))
  kappa_val <- data.frame(Set = "Test", Kappa = conf$overall["Kappa"])
    if (!is.null(train_real) && !is.null(train_pred)) {
    kappa_train <- confusionMatrix(train_pred, train_real)$overall["Kappa"]
    kappa_val <- rbind(kappa_val, data.frame(Set = "Train", Kappa = kappa_train))
  }
  
  write_xlsx(kappa_val, paste0("metrics/kappa_rf_", gsub(" ", "_", tolower(name)), ".xlsx"))
}


pipeline_rf_cv <- function(X, Y, name_model) {
  cat("\n--------------------------------------------------\n")
  cat(paste("📊 Executing RF 5-fold CV for:", name_model, "\n"))
  cat("--------------------------------------------------\n")
  
  X_df <- as.data.frame(X)
  colnames(X_df) <- make.names(colnames(X_df))
  
  set.seed(123)  
  folds <- createFolds(Y, k = 5, returnTrain = FALSE)
  
  pred_cv <- factor(rep(NA, length(Y)), levels = levels(Y))
  
  ## 1) Loop over each fold
  for (i in seq_along(folds)) {
    test_idx  <- folds[[i]]
    train_idx <- setdiff(seq_along(Y), test_idx)
    
    # Create training data.frame
    df_train <- X_df[train_idx, ]
    df_train$Class <- Y[train_idx]
    
    # Create test data.frame
    df_test <- X_df[test_idx, ]
    
    # Train Random Forest
    model_rf <- randomForest(Class ~ ., data = df_train,
                              importance = TRUE, ntree = 100, mtry = 5)
    
    # Predict in the test fold
    pred_fold <- predict(model_rf, newdata = df_test)
    
    # Store predictions in corresponding position
    pred_cv[test_idx] <- pred_fold
  }
  
  # 2) Global metrics on the whole dataset (accumulated test)
  conf <- confusionMatrix(pred_cv, Y)
  acc_cv <- mean(pred_cv == Y)
  
  # Save confusion matrix and metrics in files
  confusion_matrix (Y, pred_cv, name_model)
  metrics(Y, pred_cv, name_model)
  
  cat(sprintf("✅ Accuracy 5-fold CV (%s): %.2f%%\n", name_model, acc_cv * 100))
  
  # 3) Train final model with all data (for variable importance)
  df_full <- X_df
  df_full$Class <- Y
  final_model <- randomForest(Class ~ ., data = df_full,
                              importance = TRUE, ntree = 100, mtry = 5)
  
  # 4) Importance of variables with the final model
  importance_df <- as.data.frame(importance(final_model))
  importance_df$Variable <- rownames(importance_df)
  rownames(importance_df) <- NULL
  importance_df <- importance_df[order(-importance_df$MeanDecreaseGini), ]
  write_xlsx(importance_df, paste0("metrics/importance_rf_", gsub(" ", "_", tolower(name_model)), ".xlsx"))
  
  # Plot top 30 most important variables
  top_n <- min(30, nrow(importance_df))
  g <- ggplot(importance_df[1:top_n, ], aes(x = reorder(Variable, MeanDecreaseGini), 
                                            y = MeanDecreaseGini)) +
    geom_col() +
    coord_flip() +
    labs(title = paste("Top Wavelenghts", name_model),
         x = "Wavelenghts (nm)", y = "Importance (Gini)") +
    theme_minimal()
  
  ggsave(paste0("figures/importance_rf_", gsub(" ", "_", tolower(name_model)), ".png"), 
         g, width = 7, height = 5, dpi = 300)
  
  return(data.frame(model = name_model, Accuracy_5foldCV = acc_cv * 100))
}


#######################################
## Load Data
#######################################

raw_data <- read_excel("C:/~~/~~/raw_data_superv.xlsx")

Y <- as.factor(raw_data$GR)
X_raw <- raw_data[, sapply(raw_data, is.numeric)]

##############################################
## Execute RF for the different pretreatments
##############################################

results <- list()

# 1) Raw data
results[["Raw"]] <- pipeline_rf_cv(X_raw, Y, "Raw")

# 2) Normalization to maximum (per row)
X_norm <- X_raw / apply(X_raw, 1, max)
results[["Normalization to maximum"]] <- pipeline_rf_cv(X_norm, Y, "Normalization to maximum")

# 3) Savitzky-Golay
X_sg <- savitzkyGolay(as.matrix(X_raw), m = 0, p = 2, w = 11)
X_sg <- as.data.frame(X_sg)
results[["Savitzky-Golay"]] <- pipeline_rf_cv(X_sg, Y, "Savitzky-Golay")

# 4) First derivative with Savitzky-Golay
preprocess_first_derivative <- function(X) {
  savitzkyGolay(X, m = 1, p = 2, w = 11)
}
X_fd <- preprocess_first_derivative(as.matrix(X_raw))
results[["First derivative"]] <- pipeline_rf_cv(X_fd, Y, "First derivative")


# 5) Second derivative with Savitzky-Golay
preprocess_second_derivative <- function(X) {
  savitzkyGolay(X, m = 2, p = 2, w = 11)
}
X_sd <- preprocess_second_derivative(as.matrix(X_raw))
results[["Second derivative"]] <- pipeline_rf_cv(X_sd, Y, "Second derivative")


# 6) SNV (Standard Normal Variate)
preprocess_snv <- function(X) {
  standardNormalVariate(X)
}
X_snv <- preprocess_snv(as.matrix(X_raw))
results[["SNV"]] <- pipeline_rf_cv(X_snv, Y, "SNV")

# 7) MSC
X_msc <- msc(as.matrix(X_raw))
X_msc <- as.data.frame(X_msc)
results[["MSC"]] <- pipeline_rf_cv(X_msc, Y, "MSC")

##############################################
## Execute RF for the different pretreatments
##############################################

results_df <- do.call(rbind, results)
print(results_df)
write_xlsx(results_df, "scores/accuracy_summary_rf_cv.xlsx")

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

