library(tidyverse)

# read data and analysis ------------------------------------------------------

# clean data
data_file <- "inputs/clean_data_child.xlsx"

data_nms <- names(readxl::read_excel(path = data_file, sheet = "UGA2109_Cross-Sectoral Child...", n_max = 200))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_clean_data <- readxl::read_excel(path = data_file, sheet = "UGA2109_Cross-Sectoral Child...", col_types = c_types)
df_clean_data_harm_mentioned <- readxl::read_excel(path = data_file, sheet = "harm_mentioned")

# read analysis output 
df_analysis_output <- readr::read_csv("inputs/full_analysis_lf_child.csv")

# tool
df_survey <- readxl::read_excel("inputs/Child_Protection_Assessment_Child_Tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/Child_Protection_Assessment_Child_Tool.xlsx", sheet = "choices")
