################################
## Load required packages
################################
library(readxl)
library(randomForest)
library(prospectr)

################################
## Crear carpeta “App”
################################
if (!dir.exists("App")) dir.create("App")

################################
## Función de preprocesado
################################
preprocess_first_derivative <- function(X) {
  savitzkyGolay(X, m = 1, p = 2, w = 11)
}

################################
## Cargar y preparar datos
################################
raw_data <- read_excel("Documents/GitHub/Oil_Origin_NIRS_ML/data/NIRS_raw_data_superv.xlsx")
numeric_cols <- sapply(raw_data, is.numeric)
X_raw <- as.matrix(raw_data[, numeric_cols])
Y <- as.factor(raw_data$GR)

set.seed(123)
split_index <- caret::createDataPartition(Y, p = 0.7, list = FALSE)
X_train <- X_raw[split_index, , drop = FALSE]
X_test  <- X_raw[-split_index, , drop = FALSE]
Y_train <- Y[split_index]
Y_test  <- Y[-split_index]

################################
## Preprocesar primera derivada
################################
X_fd_train <- preprocess_first_derivative(X_train)
X_fd_test  <- preprocess_first_derivative(X_test)

# Convertir a data.frame para randomForest y limpiar nombres
df_train <- as.data.frame(X_fd_train)
df_train$Class <- Y_train
colnames(df_train) <- make.names(colnames(df_train))

df_test <- as.data.frame(X_fd_test)
colnames(df_test) <- make.names(colnames(df_test))

################################
## Entrenar el modelo RF
################################
set.seed(123)
model_rf_sg1 <- randomForest(
  Class ~ .,
  data = df_train,
  importance = TRUE,
  ntree = 100,
  mtry = 5
)

################################
## Guardar el modelo entrenado
################################
saveRDS(
  model_rf_sg1,
  file = "Documents/GitHub/Oil_Origin_NIRS_ML/App/model_rf_savitzkyGolay_1st_derivative.rds"
)

cat("✅ Modelo RF (Savitzky–Golay 1ª derivada) guardado en Documents/GitHub/Oil_Origin_NIRS_ML/App/model_rf_savitzkyGolay_1st_derivative.rds\n")
