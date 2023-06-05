
setwd("C:/Users/frost/Documents/_UNI/_MA/3_Semester/Spatial_Econometrics/AA_Project_SpatEcon")

# Packages ####
# load the required packages:

library("needs")
needs(
  "tidyverse",
  "lubridate",
  "sf",
  "arrow",
  "stringr",
  "ggthemes",
  "viridis",
  "ggtext",
  "spatstat",
  "sp",
  "gt",
  "maptools",
  "Hmisc",
  "lme4",
  "cowplot",
  "stargazer",
  "plm"
)

# source code ####

source("Code/load_data.R") # load the raw data, if required

source("Code/quadcount_cor.R")
source("Code/Clustering_Analysis.R")

# from now on only year 2006!

if(!file.exists("Data/Clean/data_final.Rds") &
   !file.exists("Data/Clean/data_final_long.Rds")){
  source("Code/make_data.R") # generate random points and compute arrests 
                             # and other SQFs around all points.
                             # also determine borrow the points are in!
} else {
  data_final <- readRDS("Data/Clean/data_final.Rds")
  data_final_long <- readRDS("Data/Clean/data_final_long.Rds")
}

source("Code/Regressions.R") # load regression results
