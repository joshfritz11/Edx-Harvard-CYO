##########################################################
# Create training and test sets
##########################################################

if (!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# Dry Bean Dataset
# https://archive-beta.ics.uci.edu/dataset/602/dry+bean+dataset
# https://archive.ics.uci.edu/ml/machine-learning-databases/00602/DryBeanDataset.zip

options(timeout = 120)

dl <- "DryBeanDataset.zip"
if (!file.exists(dl))
    download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00602/DryBeanDataset.zip", dl)

Dry_Bean_Dataset_file <- "DryBeanDataset/Dry_Bean_Dataset.xlsx"
if (!file.exists(Dry_Bean_Dataset_file))
    unzip(dl, Dry_Bean_Dataset_file)

Dry_Bean_Dataset <- readxl::read_xlsx(Dry_Bean_Dataset_file,
          col_names = TRUE,
          trim_ws = TRUE)

# Test set will be 10% of the data.
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y = Dry_Bean_Dataset$Class, times = 1, p = 0.1, list = FALSE)
train_set <- Dry_Bean_Dataset[-test_index, ]
test_set <- Dry_Bean_Dataset[test_index, ]