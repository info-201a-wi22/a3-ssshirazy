# Assignment Overview ----------------------------------------------------------
# Assignment 3: Data Visualization (incarceration) (INFO-201, Winter 2022)

# Set up working directory
setwd("C:/Users/Samira Shirazy/Desktop/Assessments/a3-ssshirazy")
getwd()

# Load the necessary packages 
library("dplyr")
library("stringr")
library("tidyverse")

# Load the *incarceration trends* data into a variable. `incarceration_data`
file_name <- "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
incarceration_data <- read.csv(file_name, 
                               header = TRUE, stringsAsFactors = FALSE)

View(incarceration_data)
print(colnames(incarceration_data))

# 5 relevant values of interest:
  # 1: In 1970 vs 2018, which state had the smallest `jail_rated_capacity` and
    # which one had the largest?
  # 2: Which year had the highest percent of Black prisoners?
  # 3: 
  # 4: 
  # 5: 
