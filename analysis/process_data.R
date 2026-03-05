library(here)
library(tidyverse)
library(jsonlite)

processed_data_directory <- here("..","data","processed_data")
file_name <- "nameability"

#read experiment data
exp_data <- read_csv(here(processed_data_directory,paste0(file_name,"-alldata.csv")))

exp_data <- exp_data %>%
  #clean up participant ids
  rename(participant_id = participant) %>%
  mutate(
    participant_id = case_when(
      participant_id == "9252" ~ "parrot",
      participant_id == "A18534325" ~ "moose",
      TRUE ~ trimws(tolower(participant_id))
    )
  )
#double check that participant ids are unique
counts_by_random_id <- exp_data %>%
  group_by(random_id,participant_id) %>%
  count()
#output to track participants
write_csv(counts_by_random_id,here(processed_data_directory,paste0(file_name,"-participant-list.csv")))

coalesce_by_column <- function(df) {
  # Coalesce all columns in a grouped tibble
  # This returns the first non-NA value for each column within the group
  return(coalesce(df[1], df[2]))
}

#extract final questionnaire responses
questionnaire_responses <- exp_data %>% 
  filter(trial_index>2) %>%
  filter(trial_type == "survey-text") %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json) %>%
  select(random_id,participant_id, Language:Extra_info) %>%
  group_by(random_id,participant_id) %>%
  summarise(across(everything(), coalesce_by_column))

#join into exp_data
exp_data <- exp_data %>%
  left_join(questionnaire_responses)

#filter dataset
processed_data <- exp_data %>%
  filter(trial_type == "categorize-image")

#filter participant ids
filter_ids <- c("test1","test1000")

#identify participants from the experiment group
group_members <- c("koala","llama","tiger","fox","lynx")

processed_data <- processed_data %>%
  filter(!(participant_id %in% filter_ids)) %>%
  #flag for group participants
  mutate(participant_is_group_member = case_when(
    participant_id %in% group_members ~ TRUE,
    TRUE ~ FALSE
  
  )) %>%
  #remove unneeded columns
  select(-c(success,plugin_version,timeout:failed_video)) %>%
  #add trial_number
  group_by(participant_id) %>%
  mutate(trial_counter = row_number()) %>%
  group_by(participant_id,random_id) %>%
  #trial number should only increment if the previous trial was correct
  mutate(
    # Check if the PREVIOUS row was correct
    is_previous_correct = case_when(
      trial_counter == 1 ~ TRUE, #by definition first response has previous correct
      TRUE ~ lag(correct, default = FALSE)
  )) %>%
  mutate(
    trial_number =cumsum(is_previous_correct)
  ) %>%
    #add new column "repeat" every time trial_number is the same as the previous row
    mutate(repeated_trial = case_when(
      trial_number == lag(trial_number) ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    ungroup() %>%
  relocate(trial_number,.after=trial_index) %>%
  relocate(trial_counter,.after=trial_index) %>%
  relocate(repeated_trial,.after=trial_number) 
  
#store processed and prepped data
write_csv(processed_data,here(processed_data_directory,paste0(file_name,"-processed-data.csv")))
