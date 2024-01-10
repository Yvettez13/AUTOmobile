# Automobile Dataset Analysis in R

## Introduction

This project focuses on analyzing an automobile dataset using R. It encompasses various stages including data preprocessing, exploratory data analysis (EDA), feature engineering, and model training using Linear Discriminant Analysis (LDA) and Random Forest. The primary objective is to derive meaningful insights from the automobile data and predict market segments and symboling ratings.

## Project Structure

- `data/`: Directory containing the automobile dataset used for the analysis.
- `scripts/`: R scripts for executing data preprocessing, EDA, feature engineering, and model training.
- `results/`: Contains outputs from the analysis, including statistical summaries, visualizations, and model performance metrics.
- `README.md`: Documentation file providing detailed information about the project, setup instructions, and usage guidelines.

## Key Libraries Used

- `readr`: For efficiently reading CSV data files.
- `dplyr`: Utilized for data manipulation and transformation.
- `ggplot2`: For creating histograms and scatter plots in the EDA process.
- `MASS`, `klaR`, and `caret`: Libraries used for implementing Linear Discriminant Analysis.
- `randomForest`: For building and evaluating Random Forest models.

## Getting Started

### Prerequisites

Ensure R is installed on your system along with the following packages:
- `readr`
- `dplyr`
- `ggplot2`
- `MASS`
- `klaR`
- `caret`
- `randomForest`

### Installation

Clone the repository and install the required R packages. Run the following commands in your R console:

```R
install.packages(c("readr", "dplyr", "ggplot2", "MASS", "klaR", "caret", "randomForest"))
