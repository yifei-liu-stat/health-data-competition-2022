rm(list = ls())
library(tidyverse)


medicare2019 <- read_csv("./data/Medicare_Hospital_Spending_Per_Patient-Hospital.csv")
#head(medicare2019)
medicare2018 <- read_csv("./data/Medicare Hospital Spending Per Patient - Hospital.csv")

medicare2018 %>% 
  mutate(Score = as.numeric(ifelse(Score == "Not Available", NA, Score)))  %>% 
  full_join(medicare2019 %>% mutate(Score = as.numeric(ifelse(Score == "Not Available", NA, Score))) %>%
              rename(Score19 = Score), by = "Facility ID" ) %>%
              mutate(med_spend_score  = rowMeans(select(.,contains("Score")), na.rm = T)) %>% select("Facility ID", "med_spend_score") -> med_spend_score


main_dat <- read_csv("./data/Analysis dataset.csv")
main_dat %>% select(-1) %>%
  mutate(`Facility ID` = as.character(`Facility ID`)) %>% left_join(med_spend_score, by = "Facility ID") -> output 


write_csv(output, "./data/final_analysis_data.csv")