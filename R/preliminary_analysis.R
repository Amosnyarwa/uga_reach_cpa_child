library(tidyverse)
library(srvyr)
library(janitor)
library(glue)

# source some scripts
source("R/composite_indicators.R")
source("R/make_weights.R")
source("R/support_functions.R")

# load data ---------------------------------------------------------------

df_cleaned <- read_csv("inputs/clean_data_child.csv")
df_harm_mentioned = read_csv("inputs/clean_harm_mentioned_data_child.csv")
df_child_age_info = read_csv("inputs/clean_child_age_info_data_child.csv")

dap <- read_csv("inputs/r_dap_child.csv") %>% 
  janitor::clean_names()

start<- Sys.time() 

# load in individual level population data sets
df_ref_pop <- read_csv("inputs/refugee_population_child.csv")
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

# set up design objects

ref_svy <- as_survey(.data = df_ref_with_weights, strata = strata, weights = weights )
host_svy <- as_survey(.data = df_host_with_weights, strata = strata, weights = weights )

df_main_analysis <- analysis_support_after_survey_creation(input_ref_svy = ,
                                                           input_host_svy = ,
                                     input_dap = dap %>% filter(!variable %in% c("ages_for_minor",
                                                                                 "how_protection_risks_influence_behaviour",
                                                                                 "places_where_child_feels_most_at_risk")))
df_harm_mentioned_analysis <- analysis_support_after_survey_creation(input_ref_svy = df_harm_mentioned,
                                               input_host_svy = df_ref_pop,
                                               input_dap = dap %>% filter(variable %in% c("how_protection_risks_influence_behaviour",
                                                                                          "places_where_child_feels_most_at_risk")))
df_child_age_info_analysis <- analysis_support_after_survey_creation(input_ref_svy = df_child_age_info,
                                               input_host_svy = df_ref_pop,
                                               input_dap = dap %>% filter(variable %in% c("ages_for_minor")))

# merge analysis ----------------------------------------------------------

full_analysis_long <- bind_rows(df_main_analysis, df_harm_mentioned_analysis, df_child_age_info_analysis)

end <- Sys.time()

print(paste("Time taken to run the script: ", end - start))

full_analysis_long %>%
  write_csv(paste0("outputs/", butteR::date_file_prefix(), "_full_analysis_lf_child.csv"), na="")
