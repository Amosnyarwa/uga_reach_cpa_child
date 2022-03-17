
source("R/support_functions.R")

# output holder -----------------------------------------------------------

logic_seperate_output <- list()

# okay_parents_arrange_child_marriage_not_agree_1 -------------------------
df_c_logic_okay_parents_arrange_child_marriage_not_agree <- df_tool_data %>% 
  filter(okay_parents_arrange_child_marriage %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree") &
           (okay_parents_arrange_child_marriage_for_money %in% c("agree", "strongly_agree") | 
              okay_parents_arrange_child_marriage_for_her_safety %in% c("agree", "strongly_agree"))) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_parents_arrange_child_marriage",
         i.check.current_value = okay_parents_arrange_child_marriage,
         i.check.value = "",
         i.check.issue_id = "okay_parents_arrange_child_marriage_not_agree_1",
         i.check.issue = glue("okay_parents_arrange_child_marriage_for_money: {okay_parents_arrange_child_marriage_for_money}, okay_parents_arrange_child_marriage_for_her_safety: {okay_parents_arrange_child_marriage_for_her_safety}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_c_logic_okay_parents_arrange_child_marriage_not_agree")

# okay_parents_arrange_child_marriage_agree_2 -----------------------------
df_c_logic_okay_parents_arrange_child_marriage_agree <- df_tool_data %>% 
  filter(okay_parents_arrange_child_marriage %in% c("agree", "strongly_agree") &
           (okay_parents_arrange_child_marriage_for_money %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree") | 
              okay_parents_arrange_child_marriage_for_her_safety %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree"))) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_parents_arrange_child_marriage",
         i.check.current_value = okay_parents_arrange_child_marriage,
         i.check.value = "",
         i.check.issue_id = "okay_parents_arrange_child_marriage_agree_2",
         i.check.issue = glue("okay_parents_arrange_child_marriage_for_money: {okay_parents_arrange_child_marriage_for_money}, okay_parents_arrange_child_marriage_for_her_safety: {okay_parents_arrange_child_marriage_for_her_safety}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_c_logic_okay_parents_arrange_child_marriage_agree")

# okay_girl_less_18_years_get_married_not_agree_3 -------------------------
df_c_logic_okay_girl_get_married_stop_school_once_married <- df_tool_data %>% 
  filter(okay_girl_less_18_years_get_married %in% c("disagree", "strongly_disagree") & 
           okay_girl_stay_home_and_stop_school_once_married %in% c("agree", "strongly_agree", "neither_agree_nor_agree")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_girl_less_18_years_get_married",
         i.check.current_value = okay_girl_less_18_years_get_married,
         i.check.value = "",
         i.check.issue_id = "okay_girl_less_18_years_get_married_not_agree_3",
         i.check.issue = glue("okay_girl_stay_home_and_stop_school_once_married: {okay_girl_stay_home_and_stop_school_once_married}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_c_logic_okay_girl_get_married_stop_school_once_married")

# okay_girl_less_18_years_get_married_not_agree_4 -------------------------
df_c_logic_okay_girl_get_married_reach_puberty <- df_tool_data %>% 
  filter(okay_girl_less_18_years_get_married %in% c("disagree", "strongly_disagree") & 
           okay_girl_get_married_once_reach_puberty %in% c("agree", "strongly_agree", "neither_agree_nor_agree")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_girl_less_18_years_get_married",
         i.check.current_value = okay_girl_less_18_years_get_married,
         i.check.value = "",
         i.check.issue_id = "okay_girl_less_18_years_get_married_not_agree_4",
         i.check.issue = glue("okay_girl_get_married_once_reach_puberty: {okay_girl_get_married_once_reach_puberty}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_c_logic_okay_girl_get_married_reach_puberty")

# okay_girl_less_18_years_get_married_agree_5 -----------------------------
df_c_logic_okay_girl_get_married_agree_reach_puberty <- df_tool_data %>% 
  filter(okay_girl_less_18_years_get_married %in% c("agree", "strongly_agree") & 
           okay_girl_get_married_once_reach_puberty %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_girl_less_18_years_get_married",
         i.check.current_value = okay_girl_less_18_years_get_married,
         i.check.value = "",
         i.check.issue_id = "okay_girl_less_18_years_get_married_agree_5",
         i.check.issue = glue("okay_girl_get_married_once_reach_puberty: {okay_girl_get_married_once_reach_puberty}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_c_logic_okay_girl_get_married_agree_reach_puberty")

# okay_father_mother_to_hit_his_child_agree_but_disagree_reasons_6 --------
# correction: (okay_parents_hit_child_to_discipline:okay_parents_hit_child_to_displine)
df_c_logic_hit_child_agree_but_disagree_reasons <- df_tool_data %>%
  filter((okay_father_to_hit_his_child %in% c("agree", "strongly_agrees") |
            okay_mother_to_hit_her_child %in% c("agree", "strongly_agrees")) &
           (okay_parents_hit_child_to_displine %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree") &
              okay_parents_hit_child_to_set_example %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree"))) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "okay_father_to_hit_his_child",
         i.check.current_value = okay_father_to_hit_his_child,
         i.check.value = "",
         i.check.issue_id = "okay_father_mother_to_hit_his_child_agree_but_disagree_reasons_6",
         i.check.issue = glue("okay_mother_to_hit_her_child : {okay_mother_to_hit_her_child}, okay_parents_hit_child_to_displine : {okay_parents_hit_child_to_displine}, okay_parents_hit_child_to_set_example : {okay_parents_hit_child_to_set_example}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept",
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>%
  dplyr::select(starts_with("i.check")) %>%
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_c_logic_hit_child_agree_but_disagree_reasons")

# okay_father_mother_to_hit_his_child_disagree_but_agree_reasons_7 --------
# correction: (okay_parents_hit_child_to_discipline:okay_parents_hit_child_to_displine)
df_c_logic_hit_child_disagree_but_agree_reasons <- df_tool_data %>%
  filter((okay_father_to_hit_his_child %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree") |
            okay_mother_to_hit_her_child %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree")) &
           (okay_parents_hit_child_to_displine %in% c("agree", "strongly_agrees") |
              okay_parents_hit_child_to_set_example %in% c("agree", "strongly_agrees"))) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "okay_father_to_hit_his_child",
         i.check.current_value = okay_father_to_hit_his_child,
         i.check.value = "",
         i.check.issue_id = "okay_father_mother_to_hit_his_child_disagree_but_agree_reasons_7",
         i.check.issue = glue("okay_mother_to_hit_her_child : {okay_mother_to_hit_her_child}, okay_parents_hit_child_to_displine : {okay_parents_hit_child_to_displine}, okay_parents_hit_child_to_set_example : {okay_parents_hit_child_to_set_example}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept",
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>%
  dplyr::select(starts_with("i.check")) %>%
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_c_logic_hit_child_disagree_but_agree_reasons")

# parents_responsible_to_provide_child_8 ----------------------------------
df_c_logic_parents_responsible_to_provide_child_contradict <- df_tool_data %>% 
  filter(parents_responsible_to_provide_child_enough_food %in% c("strongly_disagree", "disagree"),
         parents_responsible_to_provide_all_child_needs %in% c("strongly_agree", "agree")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "parents_responsible_to_provide_child_enough_food",
         i.check.current_value = parents_responsible_to_provide_child_enough_food,
         i.check.value = "no_answer",
         i.check.issue_id = "parents_responsible_to_provide_child_8",
         i.check.issue = glue("parents_responsible_to_provide_child_enough_food: {parents_responsible_to_provide_child_enough_food}, parents_responsible_to_provide_all_child_needs: {parents_responsible_to_provide_all_child_needs}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "implement_logical_change", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  slice(rep(1:n(), each = 2)) %>% 
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.district_name, 
           i.check.point_number, i.check.type,  i.check.name,  i.check.current_value) %>% 
  mutate(rank = row_number(),
         i.check.name = ifelse(rank == 2, "parents_responsible_to_provide_all_child_needs", i.check.name),
         i.check.current_value = ifelse(rank == 2, parents_responsible_to_provide_all_child_needs, i.check.current_value)) %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_c_logic_parents_responsible_to_provide_child_contradict")

# frequency_children_get_involved_in_harsh_work_9 -------------------------
df_c_logic_children_get_involved_in_harsh_work_mismatch <- df_tool_data %>% 
  filter(!is.na(child_labour_economic_types), 
         child_labour_economic_types != "other", 
         str_detect(string = str_c(c("bonded_labour_or_slavery", "construction", 
                                     "charcoal_burning", "handling_of_heavy_loads", "mining", 
                                     "sand_mining", "producing_and_or_trafficking_or_selling_drugs", 
                                     "sale_or_trafficking_of_children_for_labour_purposes", "sexual_exploitation", 
                                     "stone_quarrying", "working_with_armed_groups"), collapse = " "),
                    pattern = str_replace_all(string = child_labour_economic_types, pattern = " ", replacement = "|")),
         frequency_children_get_involved_in_harsh_work == "never") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_children_get_involved_in_harsh_work",
         i.check.current_value = frequency_children_get_involved_in_harsh_work,
         i.check.value = "no_answer",
         i.check.issue_id = "frequency_children_get_involved_in_harsh_work_9",
         i.check.issue = glue("child_labour_economic_types: {child_labour_economic_types}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "implement_logical_change", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_c_logic_children_get_involved_in_harsh_work_mismatch")

# child_labour_economic_types_10 ------------------------------------------
df_c_logic_child_labour_economic_types <- df_tool_data %>% 
  filter(!is.na(child_labour_economic_types), 
         !str_detect(string = str_c(c("bonded_labour_or_slavery", "construction", "charcoal_burning", 
                                      "handling_of_heavy_loads", "mining", "sand_mining", 
                                      "producing_and_or_trafficking_or_selling_drugs", 
                                      "sale_or_trafficking_of_children_for_labour_purposes", "sexual_exploitation", 
                                      "stone_quarrying", "working_with_armed_groups"), collapse = " "),
                     pattern = str_replace_all(string = child_labour_economic_types, pattern = " ", replacement = "|")),
         str_detect(string = str_c(c("bonded_labour", "slavery", "construction", 
                                     "charcoal_burning", "handling_of_heavy_loads", 
                                     "sand_mining", "producing_and_or_trafficking_or_selling_drugs", 
                                     "sale_or_trafficking_of_children", "sexual_exploitation", 
                                     "stone_quarrying", "working_with_armed_groups"), collapse = " "),
                    pattern = str_replace_all(string = specific_types_of_harsh_labour_child_involved_since_covid, pattern = " ", replacement = "|"))) %>% 
  mutate(i.check.type = "add_option",
         i.check.name = "child_labour_economic_types",
         i.check.current_value = child_labour_economic_types,
         i.check.value = "",
         i.check.issue_id = "child_labour_economic_types_10",
         i.check.issue = glue("specific_types_of_harsh_labour_child_involved_since_covid: {specific_types_of_harsh_labour_child_involved_since_covid}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "implement_logical_change", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  separate(col = "specific_types_of_harsh_labour_child_involved_since_covid", into = c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6"), sep = " " , remove = FALSE) %>%
  pivot_longer(cols = "x_1" : "x_6", names_to = "split_var", values_to = "current_value_created") %>% 
  filter(!is.na(current_value_created), !str_detect(string = child_labour_economic_types, pattern = current_value_created) ) %>% 
  mutate(i.check.value = current_value_created) %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = "")) 

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_c_logic_child_labour_economic_types")

# child_labour_economic_types_harsh_work_11 ------------------------------------------
df_c_logic_child_labour_economic_types_harsh_work <- df_tool_data %>% 
  filter(!is.na(child_labour_economic_types)) %>% 
  filter( 
    !str_detect(string = str_c(c("bonded_labour_or_slavery", "construction", "charcoal_burning", 
                                 "handling_of_heavy_loads", "mining", "sand_mining", 
                                 "producing_and_or_trafficking_or_selling_drugs", 
                                 "sale_or_trafficking_of_children_for_labour_purposes", "sexual_exploitation", 
                                 "stone_quarrying", "working_with_armed_groups"), collapse = " "),
                pattern = str_replace_all(string = child_labour_economic_types, pattern = " ", replacement = "|")),
    str_detect(string = action_child_takes_when_told_to_do_harsh_work, pattern = "i_just_do_the_work")
  ) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "child_labour_economic_types",
         i.check.current_value = child_labour_economic_types,
         i.check.value = "",
         i.check.issue_id = "child_labour_economic_types_harsh_work_11",
         i.check.issue = glue("action_child_takes_when_told_to_do_harsh_work: {action_child_takes_when_told_to_do_harsh_work}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_c_logic_child_labour_economic_types_harsh_work")

# child_labour_protection_services_sought_12 ------------------------------------------
df_c_logic_child_labour_protection_services_sought <- df_tool_data %>% 
  filter(str_detect(string = str_c(c("i_report_it_to_ngo_staff", "i_report_it_to_parasocial_worker", 
                                     "i_report_it_to_the_child_protection_committees", "i_report_it_to_the_police", 
                                     "i_report_it_to_the_rwc"), collapse = " "),
                    pattern = str_replace_all(string = action_child_takes_when_told_to_do_harsh_work, pattern = " ", replacement = "|")),
         child_labour_protection_services_sought == "no") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "child_labour_protection_services_sought",
         i.check.current_value = child_labour_protection_services_sought,
         i.check.value = "",
         i.check.issue_id = "child_labour_protection_services_sought_12",
         i.check.issue = glue("action_child_takes_when_told_to_do_harsh_work: {action_child_takes_when_told_to_do_harsh_work}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = "")) 

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_c_logic_child_labour_protection_services_sought")

# protection_services_for_child_16 ----------------------------------------
df_c_logic_protection_services_for_child_a <- df_tool_data %>% 
  filter(protection_services_for_child_violence == "no", protection_services_for_child_physical_harm == "yes"
  ) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "protection_services_for_child_violence",
         i.check.current_value = protection_services_for_child_violence,
         i.check.value = "",
         i.check.issue_id = "protection_services_for_child_16",
         i.check.issue = glue("protection_services_for_child_violence: {protection_services_for_child_violence}, protection_services_for_child_physical_harm: {protection_services_for_child_physical_harm}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_c_logic_protection_services_for_child_a")

# protection_services_for_child_17 ----------------------------------------
df_c_logic_protection_services_for_child <- df_tool_data %>% 
  filter(protection_services_for_child_violence == "yes", protection_services_for_child_physical_harm == "no"
  ) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "protection_services_for_child_violence",
         i.check.current_value = protection_services_for_child_violence,
         i.check.value = "",
         i.check.issue_id = "protection_services_for_child_17",
         i.check.issue = glue("protection_services_for_child_violence: {protection_services_for_child_violence}, protection_services_for_child_physical_harm: {protection_services_for_child_physical_harm}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_c_logic_protection_services_for_child")

# frequency_children_get_involved_in_harsh_work_32 ------------------------
df_c_logic_children_get_involved_in_harsh_work_protection_risks <- df_tool_data %>% 
  filter(str_detect(string = str_c(c("child_labour"), collapse = " "),
                    pattern = str_replace_all(string = child_protection_risks_concerned_about, pattern = " ", replacement = "|")),
         frequency_children_get_involved_in_harsh_work == "never") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_children_get_involved_in_harsh_work",
         i.check.current_value = frequency_children_get_involved_in_harsh_work,
         i.check.value = "no_answer",
         i.check.issue_id = "frequency_children_get_involved_in_harsh_work_32",
         i.check.issue = glue("child_protection_risks_concerned_about: {child_protection_risks_concerned_about}, frequency_children_get_involved_in_harsh_work: {frequency_children_get_involved_in_harsh_work}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "implement_logical_change", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_c_logic_children_get_involved_in_harsh_work_protection_risks")

# # frequency_children_get_involved_in_harsh_work_mismatch_33 ------------------------
# df_c_logic_children_get_involved_in_harsh_work_mismatch_33 <- df_tool_data %>%
#   filter(!str_detect(string = str_c(c("child_labour"), collapse = " "),
#                      pattern = str_replace_all(string = child_protection_risks_concerned_about, pattern = " ", replacement = "|")),
#          frequency_children_get_involved_in_harsh_work != "never") %>%
#   mutate(i.check.type = "change_response",
#          i.check.name = "frequency_children_get_involved_in_harsh_work",
#          i.check.current_value = frequency_children_get_involved_in_harsh_work,
#          i.check.value = "",
#          i.check.issue_id = "frequency_children_get_involved_in_harsh_work_mismatch_33",
#          i.check.issue = glue("child_protection_risks_concerned_about: {child_protection_risks_concerned_about}"),
#          i.check.other_text = "",
#          i.check.checked_by = "",
#          i.check.checked_date = as_date(today()),
#          i.check.comment = "",
#          i.check.reviewed = "",
#          i.check.adjust_log = "",
#          i.check.uuid_cl = "",
#          i.check.so_sm_choices = "") %>%
#   dplyr::select(starts_with("i.check")) %>%
#   rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
# 
# add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_c_logic_children_get_involved_in_harsh_work_mismatch_33")

# child_protection_risks_concerned_about_sexual_violence_34 ------------------------
# correction: (child_experience_sexual_violence:child_experienced_sexual_violence)
df_c_logic_child_protection_risks_concerned_about_sexual_violence <- df_tool_data %>% 
  filter(!str_detect(string = str_c(c("sexual_violence_and_exploitation"), collapse = " "),
                     pattern = str_replace_all(string = child_protection_risks_concerned_about, pattern = " ", replacement = "|")),
         str_detect(string = str_c(c("yes_I_experienced_this", "yes_only_my_siblings", "yes_both_me_and_my_siblings"), collapse = " "),
                    pattern = str_replace_all(string = child_experienced_sexual_violence, pattern = " ", replacement = "|"))) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "child_protection_risks_concerned_about",
         i.check.current_value = child_protection_risks_concerned_about,
         i.check.value = "",
         i.check.issue_id = "child_protection_risks_concerned_about_sexual_violence_34",
         i.check.issue = glue("child_experienced_sexual_violence: {child_experienced_sexual_violence}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_c_logic_child_protection_risks_concerned_about_sexual_violence")

# child_protection_risks_concerned_about_separated_from_parents_35 ------------------------
df_c_logic_child_protection_risks_concerned_about_separated_from_parents <- df_tool_data %>% 
  filter(!str_detect(string = str_c(c("separation_from_family_members"), collapse = " "),
                     pattern = str_replace_all(string = child_protection_risks_concerned_about, pattern = " ", replacement = "|")),
         str_detect(string = str_c(c("yes_I_experienced_this", "yes_only_my_siblings", "yes_both_me_and_my_siblings"), collapse = " "),
                    pattern = str_replace_all(string = child_ever_separated_from_parents, pattern = " ", replacement = "|"))) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "child_protection_risks_concerned_about",
         i.check.current_value = child_protection_risks_concerned_about,
         i.check.value = "",
         i.check.issue_id = "child_protection_risks_concerned_about_separated_from_parents_35",
         i.check.issue = glue("child_ever_separated_from_parents: {child_ever_separated_from_parents}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_c_logic_child_protection_risks_concerned_about_separated_from_parents")

# child_protection_risks_concerned_about_violence_36 ------------------------
df_c_logic_child_protection_risks_concerned_about_violence <- df_tool_data %>% 
  filter(!str_detect(string = str_c(c("physical_violence"), collapse = " "),
                     pattern = str_replace_all(string = child_protection_risks_concerned_about, pattern = " ", replacement = "|")),
         str_count(string = child_protection_risks_concerned_about, pattern = boundary("word")) < 3,
         str_detect(string = str_c(c("yes_I_experienced_this", "yes_only_my_siblings", "yes_both_me_and_my_siblings"), collapse = " "),
                    pattern = str_replace_all(string = child_experienced_violence, pattern = " ", replacement = "|"))) %>% 
  mutate(i.check.type = "add_option",
         i.check.name = "child_protection_risks_concerned_about",
         i.check.current_value = child_protection_risks_concerned_about,
         i.check.value = "physical_violence",
         i.check.issue_id = "child_protection_risks_concerned_about_violence_36",
         i.check.issue = glue("child_experienced_violence: {child_experienced_violence}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "implement_logical_change", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_c_logic_child_protection_risks_concerned_about_violence")

# okay_parents_arrange_child_marriage_37 ----------------------------------
df_c_logic_okay_parents_arrange_child_marriage_mismatch <- df_tool_data %>% 
  filter(
    okay_parents_arrange_child_marriage %in% c("agree", "strongly_agree"),
    okay_girl_less_18_years_get_married %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree")
  ) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_parents_arrange_child_marriage",
         i.check.current_value = okay_parents_arrange_child_marriage,
         i.check.value = "",
         i.check.issue_id = "okay_parents_arrange_child_marriage_37",
         i.check.issue = glue("okay_parents_arrange_child_marriage: {okay_parents_arrange_child_marriage}, okay_girl_less_18_years_get_married: {okay_girl_less_18_years_get_married}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_c_logic_okay_parents_arrange_child_marriage_mismatch")

# okay_parents_arrange_child_marriage_38 ----------------------------------
df_c_logic_okay_parents_arrange_child_marriage_mismatch_38 <- df_tool_data %>% 
  filter(
    okay_parents_arrange_child_marriage %in% c("agree", "strongly_agree"),
    okay_girl_stay_home_and_stop_school_once_become_mother %in% c("strongly_disagree", "disagree")
  ) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_parents_arrange_child_marriage",
         i.check.current_value = okay_parents_arrange_child_marriage,
         i.check.value = "",
         i.check.issue_id = "okay_parents_arrange_child_marriage_38",
         i.check.issue = glue("okay_parents_arrange_child_marriage: {okay_parents_arrange_child_marriage}, okay_girl_stay_home_and_stop_school_once_become_mother: {okay_girl_stay_home_and_stop_school_once_become_mother}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_seperate_output", input_df_name = "df_c_logic_okay_parents_arrange_child_marriage_mismatch_38")

# combined seperate logical checks ----------------------------------------------------------

df_logic_seperate_checks <- bind_rows(logic_seperate_output) %>% 
  mutate(reviewed = ifelse(comment =="implement_logical_change", "1", reviewed))

# output the resulting data frame
write_csv(x = df_logic_seperate_checks, file = paste0("outputs/", butteR::date_file_prefix(), "_logical_checks_child.csv"), na = "NA")

df_logic_seperate_checks %>% 
  filter(comment =="implement_logical_change") %>% 
  write_csv(file = paste0("outputs/", butteR::date_file_prefix(), "_considered_logical_checks_child.csv"), na = "NA")
