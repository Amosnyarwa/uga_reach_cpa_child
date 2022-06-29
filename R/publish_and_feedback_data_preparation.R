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

# prepare data for publishing ---------------------------------------------

# main
child_indicators_to_remove <- c("child_experienced_sexual_violence",
                                "gender_experienced_sexual_violence",
                                "age_experienced_sexual_violence",
                                "place_where_sexual_violence_mostly_occurs",
                                "place_where_sexual_violence_mostly_occurs_other",
                                "perpetrators_of_sexual_violence",
                                "perpetrators_of_sexual_violence_other",
                                "covid_impact_on_sexual_violence",
                                "sexual_violence_help_seeking",
                                "child_experienced_violence",
                                "gender_children_experienced_violence",
                                "age_group_experienced_violence",
                                "type_violence_inflicted_on_child",
                                "type_violence_inflicted_on_child_other",
                                "place_where_child_violence_occured",
                                "place_where_child_violence_occured_other",
                                "perpetrator_of_child_violence",
                                "perpetrator_of_child_violence_other",
                                "causes_of_child_violence",
                                "causes_of_child_violence_other")

df_prepared_data_main <- df_clean_data %>% 
  select(-c("interview_feedback":"call_back_note"), -starts_with(child_indicators_to_remove))

df_prepared_data_harm_mentioned <- df_clean_data_harm_mentioned %>% 
  select(-c("start":"responent_sex"))

# writing output to excel

list_of_prepared_datasets <- list("UGA2109_Cross-Sectoral Child..." = df_prepared_data_main,
                                  "harm_mentioned" = df_prepared_data_harm_mentioned)

openxlsx::write.xlsx(x = list_of_prepared_datasets,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_UGA2109 - Cross-sectoral child protection assessment_child_data.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "NA")

