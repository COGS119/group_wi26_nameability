library(here)
library(tidyverse)
library(jsonlite)
library(lme4)

processed_data_directory <- here("..","data","processed_data")
file_name <- "nameability"

processed_data <- read_csv(here(processed_data_directory,paste0(file_name,"-processed-data.csv"))) 

#summarize across subjects, by trial
avg_subj_trial <- processed_data %>%
  filter(!repeated_trial) %>%
  group_by(nameability,trial_number) %>%
  summarize(
    N=n(),
    mean_acc = mean(correct),
    sd = sd(correct),
    sem = sd / sqrt(N)
  )


ggplot(avg_subj_trial,aes(trial_number,mean_acc,color=nameability))+
  geom_errorbar(aes(ymin=mean_acc-sem,ymax=mean_acc+sem),width=0)+
  geom_point()+
  geom_line()

#create blocks
processed_data <- processed_data %>%
  mutate(block = ceiling(trial_number/8))

#summarize subjects within block
avg_subj_block <- processed_data %>%
  filter(!repeated_trial) %>%
  group_by(participant_id,nameability,block) %>%
  summarize(
    N=n(),
    mean_acc = mean(correct)
  )

#avg across blocks
overall_by_block <- avg_subj_block %>%
  group_by(nameability,block) %>%
  summarize(
    N=n(),
    avg_accuracy = mean(mean_acc),
    sd = sd(mean_acc),
    sem = sd / sqrt(N)
  )


ggplot(overall_by_block,aes(block,avg_accuracy,color=nameability))+
  geom_errorbar(aes(ymin=avg_accuracy-sem,ymax=avg_accuracy+sem),width=0)+
  geom_point()+
  geom_line()
  
#model
processed_data <- processed_data %>%
  mutate(
    nameability_c = ifelse(nameability=="high",0.5,-0.5),
    trial_number_c = trial_number - 24.5
  )
m <- glmer(correct ~ nameability_c*trial_number_c + (1|participant_id), data=filter(processed_data,!repeated_trial), family=binomial)
summary(m)
