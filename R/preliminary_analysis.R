library(tidyverse)
library(srvyr)
library(janitor)
library(glue)
library(lubridate)

# source some scripts
source("R/composite_indicators.R")
source("R/make_weights.R")
source("R/support_functions.R")

# load data ---------------------------------------------------------------

df_cleaned_with_kampala <- read_csv("inputs/clean_data_child.csv")
df_cleaned <- df_cleaned_with_kampala %>% 
  filter(district_name != "kampala")
df_harm_mentioned_with_kampala <-  read_csv("inputs/clean_harm_mentioned_data_child.csv")
df_harm_mentioned <- df_harm_mentioned_with_kampala %>% 
  filter(district_name != "kampala")
# df_child_age_info <-  read_csv("inputs/clean_child_age_info_data_child.csv")

dap <- read_csv("inputs/r_dap_child.csv") %>% 
  janitor::clean_names()

start <- Sys.time() 

# load in individual level population data sets
df_ref_pop_with_kampala <- read_csv("inputs/refugee_population_child.csv")
df_ref_pop <- df_ref_pop_with_kampala %>% 
  filter(strata != "kampala")
df_host_pop <- read_csv("inputs/host_population_child.csv")

# main: prepare data and create survey ------------------------------------------------

df_with_composites <- create_composite_indicators_cpa_child(input_df = df_cleaned) %>% 
  mutate(strata = case_when(status == "refugee" ~ paste0(i.refugee_settlement, "_refugee"),
                            status == "host_community" ~ paste0(i.region,"_host"),
                            TRUE ~ status
  ))

# split data into host and refugee

df_ref <- df_with_composites %>% 
  filter(status == "refugee")

df_host <- df_with_composites %>% 
  filter(status == "host_community")

# create weights

# refugee weights
ref_weight_table <- make_refugee_weight_table(input_df_ref = df_ref, 
                                              input_refugee_pop = df_ref_pop)
df_ref_with_weights <- df_ref %>% 
  left_join(ref_weight_table, by = "strata")

# host weights
host_weight_table <- make_host_weight_table(input_df_host = df_host, 
                                            input_host_pop = df_host_pop)
df_host_with_weights <- df_host %>% 
  left_join(host_weight_table, by = "strata")

# output data with weights and composite indicators for publishing

df_combined_data_with_weights <- bind_rows(df_ref_with_weights, df_host_with_weights) %>% 
  select(c("uuid", "weights"), starts_with("i.") )

openxlsx::write.xlsx(x = df_combined_data_with_weights,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_clean_data_child_with_weights.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "NA")

openxlsx::write.xlsx(x = df_combined_data_with_weights,
                     file = paste0("inputs/clean_data_child_with_weights.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "NA")

# set up design objects

ref_svy <- as_survey(.data = df_ref_with_weights, strata = strata, weights = weights )
host_svy <- as_survey(.data = df_host_with_weights, strata = strata, weights = weights )

df_main_analysis <- analysis_support_after_survey_creation(input_ref_svy = ref_svy,
                                                           input_host_svy = host_svy,
                                                           input_dap = dap %>% filter(!variable %in% c("ages_for_minor",
                                                                                                       "how_protection_risks_influence_behaviour",
                                                                                                       "places_where_child_feels_most_at_risk")))

# harm_mentioned: prepare data and create survey ------------------------------------------------

df_with_composites_harm_mentioned <- create_composite_indicators_cpa_child_repeats(input_df = df_harm_mentioned)

# split data into host and refugee
df_ref_harm_mentioned <- df_with_composites_harm_mentioned %>% 
  filter(status == "refugee")

df_host_harm_mentioned <- df_with_composites_harm_mentioned %>% 
  filter(status == "host_community")

# set up design objects

ref_svy_harm_mentioned <- as_survey(.data = df_ref_harm_mentioned)
host_svy_harm_mentioned <- as_survey(.data = df_host_harm_mentioned)

df_harm_mentioned_analysis <- analysis_support_after_survey_creation(input_ref_svy = ref_svy_harm_mentioned,
                                                                     input_host_svy = host_svy_harm_mentioned,
                                                                     input_dap = dap %>% filter(variable %in% c("how_protection_risks_influence_behaviour",
                                                                                                                "places_where_child_feels_most_at_risk")))

# # child_age_info: prepare data and create survey ------------------------------------------------
# 
# df_with_composites_child_age_info <- create_composite_indicators_cpa_child_repeats(input_df = df_child_age_info)
# 
# # split data into host and refugee
# df_ref_child_age_info <- df_with_composites_child_age_info %>% 
#   filter(status == "refugee")
# 
# df_host_child_age_info <- df_with_composites_child_age_info %>% 
#   filter(status == "host_community")
# 
# # set up design objects
# 
# ref_svy_child_age_info <- as_survey(.data = df_ref_child_age_info )
# host_svy_child_age_info <- as_survey(.data = df_host_child_age_info )
# 
# 
# df_child_age_info_analysis <- analysis_support_after_survey_creation(input_ref_svy = ref_svy_child_age_info,
#                                                input_host_svy = host_svy_child_age_info,
#                                                input_dap = dap %>% filter(variable %in% c("ages_for_minor")))


#  analysis including kampala ---------------------------------------------

df_main_analysis_with_kampala <- analysis_support_mofification_kampala(input_df_cleaned = df_cleaned_with_kampala,
                                                                       input_dap = dap %>% filter(!variable %in% c("ages_for_minor",
                                                                                                                   "how_protection_risks_influence_behaviour",
                                                                                                                   "places_where_child_feels_most_at_risk")), 
                                                                       input_dataset = "main_dataset")

df_harm_mentioned_analysis_with_kampala <- analysis_support_mofification_kampala(input_df_cleaned = df_harm_mentioned_with_kampala,
                                                                                 input_dap = dap %>% filter(variable %in% c("how_protection_risks_influence_behaviour",
                                                                                                                            "places_where_child_feels_most_at_risk")), 
                                                                                 input_dataset = "repeats_dataset")
# merge analysis ----------------------------------------------------------

full_analysis_long <- bind_rows(df_main_analysis, df_harm_mentioned_analysis, df_main_analysis_with_kampala, df_harm_mentioned_analysis_with_kampala)

end <- Sys.time()

print(paste("Time taken to run the script: ", end - start))

full_analysis_long %>%
  write_csv(paste0("outputs/", butteR::date_file_prefix(), "_full_analysis_lf_child.csv"), na="")
