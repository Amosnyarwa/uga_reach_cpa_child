# function for creating composite indicators

create_composite_indicators_cpa_child <- function(input_df) {
  input_df %>% 
    mutate(
      i.refugee_settlement = case_when(district_name == "adjumani" & status == "refugee" ~ "adjumani", 
                                       district_name == "kampala" & status == "refugee" ~ "kampala",
                                       refugee_settlement == "rhino" ~ "rhino_camp",
                                       TRUE ~ refugee_settlement),
      i.region = case_when(district_name %in% c("kampala") ~ "central",
                           district_name %in% c("isingiro", "kamwenge", "kikuube", "kyegegwa") ~ "south_west",
                           TRUE ~ "west_nile"),
      i.location_type = case_when(district_name %in% c("kampala") ~ "urban",
                                  TRUE ~ "rural"),
      i.respondent_age = case_when(respondent_age < 15 ~ "age_12_14",
                                   TRUE ~ "age_15_17"),
      i.education_level = case_when(hoh_education %in% c("no_formal_education") ~ "none",
                                    hoh_education %in% c("completed_primary", "incomplete_primary", "incomplete_secondary") ~ "low",
                                    hoh_education %in% c("completed_secondary", "incomplete_university", "incomplete_prof_degree", 
                                                               "incomplete_voc_training", "completed_voc_training", "incomplete_tertiary") ~ "middle",
                                    hoh_education %in% c("completed_tertiary", "completed_university", "completed_prof_degree") ~ "higher",
                                    hoh_education %in% c("other") ~ "other",
                                    TRUE ~ hoh_education
                                    )
    )
}
create_composite_indicators_cpa_child_repeats <- function(input_df) {
  input_df %>% 
    mutate(
      i.refugee_settlement = case_when(district_name == "adjumani" & status == "refugee" ~ "adjumani", 
                                       district_name == "kampala" & status == "refugee" ~ "kampala",
                                       refugee_settlement == "rhino" ~ "rhino_camp",
                                       TRUE ~ refugee_settlement),
      i.region = case_when(district_name %in% c("kampala") ~ "central",
                           district_name %in% c("isingiro", "kamwenge", "kikuube", "kyegegwa") ~ "south_west",
                           TRUE ~ "west_nile"),
      i.location_type = case_when(district_name %in% c("kampala") ~ "urban",
                                  TRUE ~ "rural"),
      i.respondent_age = case_when(respondent_age < 15 ~ "age_12_14",
                                   TRUE ~ "age_15_17")
    )
}