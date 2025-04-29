# Geographical Classification of Olive Oil Samples Using Vis-NIRS Spectroscopy and Machine Learning Techniques

# 📌 Overview
This repository provides the code and methodologies developed for the classification of olive oil samples based on their geographical origin, considering four distinct regions (Spain, Morocco, Portugal, and Italy). The dataset was generated using visible-near infrared spectroscopy (Vis-NIRS), with spectral acquisition conditions carefully optimised beforehand.
The aim of this study was to build and evaluate machine learning models capable of discriminating olive oil samples according to their origin. The implementation of such predictive models in quality control processes could significantly enhance the authentication and traceability of olive oils in the food industry.



# 📂 Project Structure

The repository is structured as follows:

├── figures/                                # Generated figures from data analysis

├── scores/                                 # Generated scores from data analysis

├── scripts/                                # Contains scripts for data analysis

│   ├── EDA_script.R                        # Exploratory Data Analysis (EDA)
 
│   ├── Spectra_visualization_script.R      # Ion Mobility Sum Spectra ploting 

│   ├── Unsupervised_algorithms_script.R    # Unsupervised Machine Learning (HCA and PCA)

│   ├── RF_CV_script.R                      # Supervised Machine Learning Random Forest with 5-fold CV for different pretreatments 

│   ├── RF_Train_Test_script.R              # Supervised Machine Learning Random Forest with Train-Test for different pretreatments 


├── requirements.txt                     # Required R packages

├── README.md                            # Project documentation

├── LICENSE                              # License file


# 🔄 Workflow
The data analysis workflow follows these main steps:

- Exploratory Data Analysis (EDA): Detection of missing values and outliers.

- Spectra Visualization based on origin of olive oil simples.

- Unsuervised techniques: Exploratory assessment of the dataset using Hierarchical Clustering Analysis (HCA) and Principal Component Analysis (PCA)

- Supervised Machine Learning Random Forest 5-fold Cross-Validation using different pre-treatments: raw data, maximum normalisation, Savitzky-Golay filter, first derivative with Savitzky-Golay filter, second derivative with Savitzky-Golay filter, Standard Normal Variation, Multiplicative Scatter Correction pretreatments.

- Supervised Machine Learning Random Forest with Train (70% of data) and Test (30% of data) using different pre-treatments: raw data, maximum normalisation, Savitzky-Golay filter, first derivative with Savitzky-Golay filter, second derivative with Savitzky-Golay filter, Standard Normal Variation, Multiplicative Scatter Correction pretreatments.


# 🖥️ Software and Dependencies
The analysis is conducted in R (v4.4.0). The required R packages are specified in requirements.txt.

# 📜 License
This project is licensed under the GNU GENERAL PUBLIC License. See LICENSE for deta
