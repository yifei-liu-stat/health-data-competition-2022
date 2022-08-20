library(tidyverse)

## Read Data

setwd("/home/liu00980/Documents/8801project/HCAHPS")
dat <- readr::read_csv("./HCAHPS-Hospital.csv", 
                col_types = cols(`ZIP Code` = col_character(),
                                  `Patient Survey Star Rating Footnote` = col_character(),
                                  `HCAHPS Answer Percent Footnote` = col_character(),
                                  `Number of Completed Surveys Footnote` = col_character(),
                                  `Survey Response Rate Percent Footnote` = col_character()) )


## Drop Footnote columns (See more in Footnote_Crosswalk.csv)
dat <- dat %>% dplyr::select(-contains("Footnote"))

## Change 'Not Applicable' to NA
dat %>% 
  dplyr::mutate(across(`Patient Survey Star Rating`:`HCAHPS Linear Mean Value`, 
                ~replace(., . == "Not Applicable", NA))) -> dat

dat %>%
  dplyr::mutate(values = pmax(`Patient Survey Star Rating`, `HCAHPS Answer Percent`, `HCAHPS Linear Mean Value`, na.rm = T)) %>%
  dplyr::select(-(`HCAHPS Question`:`HCAHPS Linear Mean Value`))-> dat

## Check 'Not Available' across all string columns, change those to NA(actual missing)
dat %>%
  dplyr::mutate(across(where(is.character), ~replace(., . == "Not Available", NA))) -> dat

## Spread data
dat %>% 
  tidyr::spread(key = `HCAHPS Measure ID`, value = values) -> wide_dat

## save
write_csv(wide_dat, 'data_processed/wide_HCAHPS.csv', col_names = T) # save to a csv file
save(wide_dat, file = "data_processed/wide_HCAHPS.RData") # save to a .RData file


## some explorations

# 10 linear scores for each hospital, probably will be used as our predictors
wide_dat %>%
  dplyr::filter(`Facility ID` == 100001) %>%
  dplyr::select(contains("LINEAR_SCORE"))
