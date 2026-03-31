#### 01_building_data_file.R --------------------------------------------------------
#### Clean-up -----------------------------------------------------------------
rm(list = ls())
library(haven)
library(dplyr)
#### Install packages --------------------------------------------------------
# Remove possible lock file
unlink("C:/Users/tkoende/AppData/Local/R/win-library/4.3/00LOCK-voteswitchR")

# Install required Shiny version
remotes::install_version("shiny", version = "1.7.2", upgrade = "never")

# Install voteswitchR
remotes::install_github("denis-cohen/voteswitchR", upgrade = "never")


#### Launch ShinyApp for data processing --------------------------------------
# Call function
voteswitchR::build_data_file()

#### Check micro-data file ---------------------------------------------------
# Load data
load("C:/Users/koend/OneDrive - UvA/Bureaublad/UvA/R_Project/VoteSwitching/VoteSwitching/data/raw/data_file.RData")
df_microdata <- data_file

# Check structure
str(df_microdata)
View(df_microdata$data)