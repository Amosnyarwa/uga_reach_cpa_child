library(tidyverse)

# read data and analysis ------------------------------------------------------

# clean data
data_file <- "inputs/clean_data_child.xlsx"

data_nms <- names(readxl::read_excel(path = data_file, sheet = "UGA2109_Cross-Sectoral Child...", n_max = 200))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_clean_data <- readxl::read_excel(path = data_file, sheet = "UGA2109_Cross-Sectoral Child...", col_types = c_types)
df_clean_data_harm_mentioned <- readxl::read_excel(path = data_file, sheet = "harm_mentioned")
df_clean_data_with_weights <- readxl::read_excel(path = "inputs/clean_data_child_with_weights.xlsx")

# read analysis output 
df_analysis_output <- readr::read_csv("inputs/full_analysis_lf_child.csv")

# tool
df_survey <- readxl::read_excel("inputs/Child_Protection_Assessment_Child_Tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/Child_Protection_Assessment_Child_Tool.xlsx", sheet = "choices")

# feedback ----------------------------------------------------------------

df_feedback_data <- df_clean_data %>% 
  filter(interview_feedback %in% c("yes")) %>% 
  select(interview_feedback:call_back_note)

openxlsx::write.xlsx(x = df_feedback_data,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_ACTED_feedback_child.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "NA")

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

audit_cols_to_remove <- c("deviceid", "audit", "audit_URL", "instance_name")

df_prepared_data_main <- df_clean_data %>% 
  select(-c("interview_feedback":"call_back_note"), -starts_with(child_indicators_to_remove), -any_of(audit_cols_to_remove)) %>% 
  left_join(df_clean_data_with_weights)

df_prepared_data_harm_mentioned <- df_clean_data_harm_mentioned %>% 
  select(-c("start":"responent_sex"), -any_of(audit_cols_to_remove))

# writing output to excel

list_of_prepared_datasets <- list("UGA2109_Cross-Sectoral Child..." = df_prepared_data_main,
                                  "harm_mentioned" = df_prepared_data_harm_mentioned)

openxlsx::write.xlsx(x = list_of_prepared_datasets,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_UGA2109 - Cross-sectoral child protection assessment_child_data.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "NA")

# prepare analysis for publishing -----------------------------------------

df_prepare_survey <- df_survey %>% 
  select(type, name, label) %>% 
  separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" ) %>% 
  filter(select_type %in% c("integer", "date", "text", "calculate", "select_one", "select_multiple"))

df_prepare_analysis_output <- df_analysis_output %>% 
  filter(!variable %in% child_indicators_to_remove) %>% 
  mutate(variable = ifelse(is.na(variable), variable_val, variable),
         variable = ifelse(variable == "i.education_level", "hoh_education", variable),
         int.variable = ifelse(str_detect(string = variable, pattern = "^i\\."), str_replace(string = variable, pattern = "^i\\.", replacement = ""), variable)) %>% 
  left_join(df_prepare_survey, by = c("int.variable" = "name")) %>% 
  relocate(label, .after = variable) %>% 
  mutate(label = ifelse(is.na(label), variable, label),
         `mean/pct` = ifelse(select_type %in% c("integer"), `mean/pct`, `mean/pct`*100),
         `mean/pct` = round(`mean/pct`, digits = 2)) %>%
  select(`Question`= label, `choices/options` = variable_val, `Results(mean/percentage)` = `mean/pct`, population, subset_1_name, subset_1_val)


list_of_output <- list("child" = df_prepare_analysis_output)

openxlsx::write.xlsx(x = list_of_output,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_UGA2109 - Cross-sectoral child protection assessment_child_analysis.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "NA")
