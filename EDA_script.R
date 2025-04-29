################################
## Load required packages
################################

library(readxl) 
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(writexl)

#######################################
## Load Data
#######################################

raw_data <- read_excel("C:/~~/~~/raw_data.xlsx")

## Select numeric data (intensities)
numeric_data <- raw_data %>% select(where(is.numeric)) 

#######################################
## Search for empty data (NA)
#######################################

## Use of a heatmap for detecting NA data
heatmap_data <- numeric_data %>%
  mutate(Row = row_number()) %>% 
  pivot_longer(cols = -Row, names_to = "Drift time", values_to = "Value") %>%
  mutate(Missing = factor(ifelse(is.na(Value), "Missing", "Present")))

formatted_labels <- function(labels) {
  sprintf("%.2f", as.numeric(labels))
}

heatmap_plot <- ggplot(heatmap_data, aes(x = Variable, y = Row, fill = Missing)) +
  geom_tile() +
  scale_fill_viridis_d(name = "Status", option = "cividis", direction = -1) +
  labs(
    title = "Heatmap of Present and Missing Values",
    x = "Drift time [RIP relative] (ms)",
    y = "Row"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10)
  ) +
  scale_x_discrete(
    breaks = levels(factor(heatmap_data$Variable))[seq(1, ncol(numeric_data), by = 75)],
    labels = formatted_labels
  )

print(heatmap_plot)


## Save the heatmap

output_dir <- ("C:/~~/~~/figures")
if (!dir.exists(output_dir)) dir.create(output_dir)

ggsave(file.path(output_dir, "Missing_values_plot.png"), plot = heatmap_plot, width = 10, height = 6, dpi = 300)


#####################################################
## Calculation of Z-scores and detection of outliers
#####################################################

z_scores <- scale(numeric_data)

average_abs_z <- apply(z_scores, 1, function(x) mean(abs(x), na.rm = TRUE))

z_threshold <- 3  #Considering 97.5% of variance deviation

numeric_data_with_outliers <- data.frame(
  Observation = 1:nrow(numeric_data),
  Average_Abs_Z = average_abs_z,
  Is_Outlier = average_abs_z > z_threshold
)

#######################################
## Outliers visualization
#######################################

## Create a scatter plot of Average Absolute Z-Scores
zscore_plot <- ggplot(numeric_data_with_outliers, aes(x = Observation, y = Average_Abs_Z)) +
  geom_point(aes(color = Is_Outlier), size = 2) +
  geom_hline(yintercept = z_threshold, linetype = "dashed", color = "red") +
  scale_color_viridis_d(
    option = "viridis",
    begin = 0.2, end = 0.8, # Adjust the range for better contrast
    name = "Status",
    labels = c("Inlier", "Outlier")
  ) +
  labs(
    title = "Outliers in Observations (Z-Scores)",
    x = "Observation",
    y = "Average Absolute Z-Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10)
  )

## Save the plot
print(zscore_plot)

output_dir <- "figures"
if (!dir.exists(output_dir)) dir.create(output_dir)

ggsave(file.path(output_dir, "Zscore_outliers_plot.png"), plot = zscore_plot, width = 10, height = 6, dpi = 300)

#######################################
## Save the outliers summary
#######################################

outliers_summary <- numeric_data_with_outliers %>%
  filter(Is_Outlier) %>%
  select(Observation, Average_Abs_Z)

print("Summary of Outlier Observations:")
print(outliers_summary)

#######################################
# Modify the dataset (if is necessary)
#######################################

clean_data <- raw_data %>%
  slice(-outliers_summary$Observation)


clean_data_path <- "C:/~~/~~/Raw_data_clean.xlsx"
write_xlsx(clean_data, clean_data_path)


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

