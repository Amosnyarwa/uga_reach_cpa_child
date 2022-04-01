# checks for data collection

library(tidyverse)
library(lubridate)
library(glue)

source("R/support_functions.R")

# read data 
df_tool_data <- readxl::read_excel("inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Child_Data.xlsx") %>% 
  mutate(i.check.uuid = `_uuid`,
         i.check.start_date = as_date(start),
         i.check.enumerator_id = enumerator_id,
         i.check.district_name = district_name,
         i.check.point_number = point_number,
         start = as_datetime(start),
         end = as_datetime(end)) %>% 
  filter(assent_child == "yes", respondent_age > 11, respondent_age < 18, i.check.start_date > as_date("2022-01-30"), 
         !str_detect(string = point_number, pattern = fixed('test', ignore_case = TRUE))
  ) %>% 
  select(-c(starts_with("...15")), -c(starts_with("places_where_child_feels_most_at_risk"),
                                      starts_with("how_protection_risks_influence_behaviour"),
                                      starts_with("child_role_with_chronic_or_disability"),
                                      "end_note",	"consent",	"hoh",	"introduction_to_caregiver",
                                      "location",	"sex",	"age",	"introduction_to_child",	"arrival",
                                      "edu_completed",	"hh_size",	"numfamily",	"children_for_mdd",
                                      "num_children_for_mdd",	"children_school_aged",	"num_children_school_aged",
                                      "demo_check",	"repeat_intro_one"))

# repeats
harm_mentioned = readxl::read_excel(path = "inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Child_Data.xlsx", sheet = "harm_mentioned") 

child_age_info = readxl::read_excel(path = "inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Child_Data.xlsx", sheet = "child_age_info")

# join repeats to the main dataset
df_tool_data_harm_mentioned <- df_tool_data %>% 
  right_join(harm_mentioned, by = c("_uuid" = "_submission__uuid") ) %>% 
  filter(is.na(`_uuid`)) 

df_tool_data_child_age_info <- df_tool_data %>% 
  right_join(child_age_info, by = c("_uuid" = "_submission__uuid") ) %>% 
  filter(is.na(`_uuid`)) 

# tool
df_survey <- readxl::read_excel("inputs/Child_Protection_Assessment_Child_Tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/Child_Protection_Assessment_Child_Tool.xlsx", sheet = "choices")

df_sample_data <- sf::st_read("inputs/cpa_child_settlement_host_samples.gpkg", quiet = TRUE)

# output holder -----------------------------------------------------------

logic_output <- list()

# Time checks -------------------------------------------------------------

# Time interval for the survey
min_time_of_survey <- 25
max_time_of_survey <- 120

df_c_survey_time <-  check_survey_time(input_tool_data = df_tool_data, 
                                       input_min_time = min_time_of_survey, 
                                       input_max_time = max_time_of_survey)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_survey_time")

# check the time between surveys
min_time_btn_surveys <- 5

df_c_time_btn_survey <- check_time_interval_btn_surveys(input_tool_data = df_tool_data,
                                                        input_min_time = min_time_btn_surveys)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_time_btn_survey")

# Logical checks ----------------------------------------------------------


# spatial checks ----------------------------------------------------------

sample_pt_nos <- df_sample_data %>% 
  mutate(unique_pt_number = paste0(status, "_", Name)) %>% 
  pull(unique_pt_number) %>% 
  unique()

# duplicate point numbers
df_c_duplicate_pt_nos <- check_duplicate_pt_numbers(input_tool_data = df_tool_data %>% filter(district_name != "kampala"),
                                                    input_sample_pt_nos_list = sample_pt_nos)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_duplicate_pt_nos")

# pt id does not exist in sample
df_c_pt_not_in_sample <- check_pt_number_not_in_samples(input_tool_data = df_tool_data %>% filter(district_name != "kampala"), 
                                                        input_sample_pt_nos_list = sample_pt_nos)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_pt_not_in_sample")

# check for exceeded threshold distance

threshold_dist <- 150

df_c_greater_thresh_distance <- check_threshold_distance(input_sample_data = df_sample_data,
                                                         input_tool_data = df_tool_data %>% filter(district_name != "kampala"),
                                                         input_threshold_dist = threshold_dist)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_greater_thresh_distance")

# combined logical checks ----------------------------------------------------------

df_logic_checks <- bind_rows(logic_output)

# others checks

df_others_data <- extract_other_data(input_tool_data = df_tool_data, 
                                     input_survey = df_survey, 
                                     input_choices = df_choices)

df_others_data_repeats <- extract_other_data_repeats(input_repeat_data = df_tool_data_harm_mentioned, 
                                                     input_survey = df_survey, 
                                                     input_choices = df_choices, 
                                                     input_sheet_name = "harm_mentioned",
                                                     input_repeat_cols = c("how_protection_risks_influence_behaviour", 
                                                                           "places_where_child_feels_most_at_risk"))

# combine logic and others checks
df_combined_checks <- bind_rows(df_logic_checks, df_others_data, df_others_data_repeats)

# output the resulting data frame
write_csv(x = df_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), "_combined_checks_child.csv"), na = "")
