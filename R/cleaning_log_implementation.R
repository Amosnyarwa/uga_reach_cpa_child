library(tidyverse)
library(lubridate)
library(glue)

source("R/support_functions.R")

# read data ---------------------------------------------------------------

# tool data
cols_from_main_dataset <- c("start",   "end", "today", "instruction_note", "consent_caregiver", "assent_caregiver", "consent_child", 
                            "assent_child", "district_name", "enumerator_id", "point_number", "status", "refugee_settlement", "refugee_settlement_zone", 
                            "sub_county_div", "nationality", "nationality_other", "respondent_age", "responent_sex",    "_id",   "uuid",  "index")

# sheets
data_cols_to_remove <- c("_parent_table_name",	"_submission__id",	"_submission__submission_time",	"_submission__validation_status", 
                         "_submission__notes",	"_submission__status",	"_submission__submitted_by",	"_submission__tags")

data_nms_harm <- names(readxl::read_excel(path = "inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Child_Data.xlsx", sheet = "harm_mentioned", n_max = 100))
c_types_harm <- ifelse(str_detect(string = data_nms_harm, pattern = "_other$"), "text", "guess")

harm_mentioned = readxl::read_excel(path = "inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Child_Data.xlsx", 
                                    sheet = "harm_mentioned", col_types = c_types_harm) %>% 
  select(-all_of(data_cols_to_remove)) %>% 
  mutate(across(.cols = everything(), .fns = ~ifelse(str_detect(string = ., pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA", .)))

# child_age_info = readxl::read_excel(path = "inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Child_Data.xlsx", sheet = "child_age_info") %>% 
#   select(-all_of(data_cols_to_remove)) %>% 
#   mutate(across(.cols = everything(), .fns = ~ifelse(str_detect(string = ., pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA", .)))

# main dataset
data_nms <- names(readxl::read_excel(path = "inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Child_Data.xlsx", sheet = "UGA2109_Cross-Sectoral Child...", n_max = 100))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_raw_data <- readxl::read_excel(path = "inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Child_Data.xlsx", sheet = "UGA2109_Cross-Sectoral Child...", col_types = c_types) %>%
  filter(assent_child == "yes", respondent_age > 11, respondent_age < 18, as_date(as_datetime(start)) > as_date("2022-01-30"), 
         !str_detect(string = point_number, pattern = fixed('test', ignore_case = TRUE))
  ) %>% 
  rename(`if_selected_ngo_or_un_agency/medecins_sans_frontieres` = `if_selected_ngo_or_un_agency/médecins_sans_frontières`,
         `causes_of_stress_among_caregivers/childrens_safety` = `causes_of_stress_among_caregivers/children’s_safety`,
         `action_child_takes_when_told_to_do_harsh_work/i_tell_the_person_i_wont_do_it` = `action_child_takes_when_told_to_do_harsh_work/i_tell_the_person_i_won't_do_it`
  ) %>% 
  mutate(if_selected_ngo_or_un_agency = str_replace(string = if_selected_ngo_or_un_agency, pattern = "médecins_sans_frontières", replacement = "medecins_sans_frontieres"),
         causes_of_stress_among_caregivers = str_replace(string = causes_of_stress_among_caregivers, pattern = "children’s_safety", replacement = "childrens_safety"),
         action_child_takes_when_told_to_do_harsh_work = str_replace(string = action_child_takes_when_told_to_do_harsh_work, pattern = "i_tell_the_person_i_won't_do_it", replacement = "i_tell_the_person_i_wont_do_it")
  ) %>% 
  mutate(refugee_settlement = ifelse(district_name == "kampala" & status == "refugee", district_name, refugee_settlement),
         refugee_settlement_zone = ifelse(district_name == "kampala" & status == "refugee", sub_county_div, refugee_settlement_zone)
  ) %>% 
  select(-c(starts_with("...15")), -c(starts_with("places_where_child_feels_most_at_risk"),
                                      starts_with("how_protection_risks_influence_behaviour"),
                                      starts_with("child_role_with_chronic_or_disability"),
                                      "end_note",	"consent",	"hoh",	"introduction_to_caregiver",
                                      "location",	"sex",	"age",	"introduction_to_child",	"arrival",
                                      "edu_completed",	"hh_size",	"numfamily",	"children_for_mdd",
                                      "num_children_for_mdd",	"children_school_aged",	"num_children_school_aged",
                                      "demo_check",	"repeat_intro_one")) %>% 
  mutate(across(.cols = everything(), .fns = ~ifelse(str_detect(string = ., pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA", .))) %>% 
  mutate(start = as_datetime(start), end = as_datetime(end), today = as_date(as_datetime(today)), date_arrival = as_date(as_datetime(date_arrival)))

# join repeats to the main dataset
df_raw_data_harm_mentioned <- df_raw_data %>% 
  select(-`_index`) %>% 
  inner_join(harm_mentioned, by = c("_uuid" = "_submission__uuid") )

# df_raw_data_child_age_info <- df_raw_data %>% 
#   select(-`_index`) %>% 
#   inner_join(child_age_info, by = c("_uuid" = "_submission__uuid") ) 

# cleaning log
df_cleaning_log <- read_csv("inputs/combined_checks_child.csv") %>% 
  mutate(adjust_log = ifelse(is.na(adjust_log), "apply_suggested_change", adjust_log),
         value = ifelse(is.na(value) & comment == "implement_logical_change", "blank", value),
         value = ifelse(is.na(value) & issue_id %in% c("logic_c_outlier"), "blank", value),
         value = ifelse(is.na(value) & type == "remove_survey", "blank", value)) %>%
  filter(adjust_log != "delete_log", !is.na(value), !is.na(uuid)) %>% 
  mutate(value = ifelse(value == "blank" & comment == "implement_logical_change", NA, value),
         relevant = NA) %>% 
  select(uuid, type, name, value, issue_id, sheet, index, relevant, issue)
# survey tool
df_survey <- readxl::read_excel("inputs/Child_Protection_Assessment_Child_Tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/Child_Protection_Assessment_Child_Tool.xlsx", sheet = "choices") %>% 
  mutate( name = str_replace(string = name, pattern = "médecins_sans_frontières", replacement = "medecins_sans_frontieres"),
          name = str_replace(string = name, pattern = "children’s_safety", replacement = "childrens_safety"),
          name = str_replace(string = name, pattern = "i_tell_the_person_i_won't_do_it", replacement = "i_tell_the_person_i_wont_do_it") )


# handle datasets ---------------------------------------------------------

# main dataset
df_cleaned_data <- implement_cleaning_support(input_df_raw_data = df_raw_data, 
                                              input_df_survey = df_survey, 
                                              input_df_choices = df_choices, 
                                              input_df_cleaning_log = df_cleaning_log %>% filter(name %in% colnames(df_raw_data))) %>% 
  select(-c("child_bearing_status", "number_children"))

df_cleaned_data_update_child_labour <- df_cleaned_data %>% 
  mutate(int.child_domestic_labour = case_when(respondent_age <= 14 & child_perform_domestic_chores == "yes" & hrs_child_perfoms_domestic_chores >= 21 ~ "yes",
                                               respondent_age <= 14 & child_perform_domestic_chores == "yes" & hrs_child_perfoms_domestic_chores < 21 ~ "no",
                                               respondent_age >= 15 & child_perform_domestic_chores == "yes" ~ "no",
                                               TRUE ~ "NA" ),
         int.child_econ_labour = case_when(respondent_age <= 14 & child_perform_econ_labour == "yes" & hrs_child_perfoms_econ_labour < 14 ~ "no",
                                           respondent_age <= 14 & child_perform_econ_labour == "yes" & hrs_child_perfoms_econ_labour >= 14 ~ "yes",
                                           respondent_age >= 15 & child_perform_econ_labour == "yes" & hrs_child_perfoms_econ_labour < 43 ~ "no",
                                           respondent_age >= 15 & child_perform_econ_labour == "yes" & hrs_child_perfoms_econ_labour >= 43 ~ "yes",
                                           TRUE ~ "NA" ),
         child_labour = case_when(int.child_domestic_labour == "yes" | int.child_econ_labour == "yes" ~ "yes",
                                      int.child_domestic_labour == "no" & int.child_econ_labour == "no" ~ "no",
                                      int.child_domestic_labour == "no" & int.child_econ_labour == "NA" ~ "no",
                                      int.child_domestic_labour == "NA" & int.child_econ_labour == "no" ~ "no",
                                      TRUE ~ "NA" )
  ) %>% 
  relocate(child_labour, .after = "hrs_child_perfoms_econ_labour") %>% 
  select(-c("hrs_child_perfoms_domestic_chores", "hrs_child_perfoms_econ_labour"))

write_csv(df_cleaned_data_update_child_labour, file = paste0("outputs/", butteR::date_file_prefix(), "_clean_data_child.csv"))

# harm_mentioned
df_cleaning_log_harm_mentioned <- df_cleaning_log %>% 
  filter(uuid %in% df_raw_data_harm_mentioned$`_uuid`, name %in% colnames(df_raw_data_harm_mentioned))

df_cleaned_harm_mentioned_data <- implement_cleaning_support(input_df_raw_data = df_raw_data_harm_mentioned, 
                                                             input_df_survey = df_survey, 
                                                             input_df_choices = df_choices, 
                                                             input_df_cleaning_log = df_cleaning_log_harm_mentioned) %>% 
  select(cols_from_main_dataset, any_of(colnames(harm_mentioned)))

write_csv(df_cleaned_harm_mentioned_data, file = paste0("outputs/", butteR::date_file_prefix(), "_clean_harm_mentioned_data_child.csv"))

# # child_age_info
# df_cleaning_log_child_age_info <- df_cleaning_log %>% 
#   filter(uuid %in% df_raw_data_child_age_info$`_uuid`, name %in% colnames(df_raw_data_child_age_info))
# 
# df_cleaned_child_age_info_data <- implement_cleaning_support(input_df_raw_data = df_raw_data_child_age_info, 
#                                                              input_df_survey = df_survey, 
#                                                              input_df_choices = df_choices, 
#                                                              input_df_cleaning_log = df_cleaning_log_child_age_info) %>% # cleaning log has no data for cleaning in this sheet
#   select(cols_from_main_dataset, any_of(colnames(child_age_info)))
# 
# write_csv(df_cleaned_child_age_info_data, file = paste0("outputs/", butteR::date_file_prefix(), "_clean_child_age_info_data_child.csv"))

list_of_clean_datasets <- list("UGA2109_Cross-Sectoral Child..." = df_cleaned_data,
                               "harm_mentioned" = df_cleaned_harm_mentioned_data
)

openxlsx::write.xlsx(x = list_of_clean_datasets,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_clean_data_child.xlsx"), 
                     overwrite = TRUE)
