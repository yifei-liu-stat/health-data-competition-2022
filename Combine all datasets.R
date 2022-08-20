library(tidyverse)
library(dplyr)


# Spread complication dataset
dat <- readr::read_csv("./Complications-19.csv", 
                       col_types = cols(
                        `ZIP Code` = col_character(),
                        `Facility ID` = col_character()
                       ) )


## Drop Footnote columns (See more in Footnote_Crosswalk.csv)
dat <- dat %>% dplyr::select(-contains("Footnote"))
## Drop CI estimates
dat <- dat %>% dplyr::select(-contains("Estimate"))


## Change 'Not Applicable' to NA
dat %>% 
  dplyr::mutate(across(`Denominator`:`Score`, 
                       ~replace(., . == "Not Applicable", NA))) -> dat

## Check 'Not Available' across all string columns, change those to NA(actual missing)
dat %>%
  dplyr::mutate(across(where(is.character), ~replace(., . == "Not Available", NA))) -> dat

## Spread data
### I do not know how to do this
### After removing the irrelevant columns, the result will be right

dat %>%
  dplyr::select(-c(`Measure Name`, `Compared to National`, `Denominator`, `Start Date`, `End Date`)) -> dat

dat %>% 
  tidyr::spread(key = `Measure ID`, value = Score) -> wide_dat_complication


dim(wide_dat_complication)
length(unique(wide_dat_complication$`Facility ID`))

## Save
write_csv(wide_dat_complication, '../data_processed/wide_complication.csv', col_names = T) # save to a csv file
save(wide_dat_complication, file = "../data_processed/wide_complication.RData") # save to a .RData file


## Repeat for Infections-19, Timely-19, Readmit-19

# Spread Infection dataset
dat <- readr::read_csv("./Infections-19.csv", 
                       col_types = cols(
                        `ZIP Code` = col_character(),
                        `Facility ID` = col_character()
                       ) )



## Drop Footnote columns (See more in Footnote_Crosswalk.csv)
dat <- dat %>% dplyr::select(-contains("Footnote"))
## Drop irrelevant columns (columns that are not consistent within a hospital)
dat <- dat %>% dplyr::select(-c(`Measure Name`, `Compared to National`, `Start Date`, `End Date`))


## Check 'Not Available' across all string columns, change those to NA (actual missing)
dat %>%
  dplyr::mutate(across(where(is.character), ~replace(., . %in% c("Not Applicable", "Not Available"), NA))) -> dat

## Spread data
dat %>% 
  tidyr::spread(key = `Measure ID`, value = Score) -> wide_dat_infection


dim(wide_dat_infection)
length(unique(wide_dat_infection$`Facility ID`))

## Save
write_csv(wide_dat_infection, '../data_processed/wide_infection.csv', col_names = T) # save to a csv file
save(wide_dat_infection, file = "../data_processed/wide_infection.RData") # save to a .RData file



# Spread Infection dataset
dat <- readr::read_csv("./Readmit-19.csv", 
                       col_types = cols(
                        `ZIP Code` = col_character(),
                        `Facility ID` = col_character()
                       ) )



## Drop Footnote columns (See more in Footnote_Crosswalk.csv)
dat <- dat %>% dplyr::select(-contains("Footnote"))
## Drop CI columns (See more in Footnote_Crosswalk.csv)
dat <- dat %>% dplyr::select(-contains("Estimate"))
## Drop Date columns (See more in Footnote_Crosswalk.csv)
dat <- dat %>% dplyr::select(-contains("Date"))
## Drop irrelevant columns (columns that are not consistent within a hospital)
dat <- dat %>% dplyr::select(-c(`Measure Name`, `Compared to National`, `Denominator`, `Number of Patients`, `Number of Patients Returned`))


## Check 'Not Available' across all string columns, change those to NA (actual missing)
dat %>%
  dplyr::mutate(across(where(is.character), ~replace(., . %in% c("Not Applicable", "Not Available"), NA))) -> dat

## Spread data
dat %>% 
  tidyr::spread(key = `Measure ID`, value = Score) -> wide_dat_readmit


dim(wide_dat_readmit)
length(unique(wide_dat_readmit$`Facility ID`))


## Save
write_csv(wide_dat_readmit, '../data_processed/wide_readmit.csv', col_names = T) # save to a csv file
save(wide_dat_readmit, file = "../data_processed/wide_readmit.RData") # save to a .RData file



# Spread Timely dataset
dat <- readr::read_csv("./Timely-19.csv", 
                       col_types = cols(
                        `ZIP Code` = col_character(),
                        `Facility ID` = col_character()
                       ) )

# 10021F will be rendered as 10021 if we don't specify `Facility ID` = col_character()

## Drop Footnote columns (See more in Footnote_Crosswalk.csv)
dat <- dat %>% dplyr::select(-contains("Footnote"))
## Drop Date columns (See more in Footnote_Crosswalk.csv)
dat <- dat %>% dplyr::select(-contains("Date"))
## Drop irrelevant columns (columns that are not consistent within a hospital)
dat <- dat %>% dplyr::select(-c(`Condition`, `Measure Name`, `Sample`))


# Seems that only the facility ID is the true identifier
dat %>%
  summarise(across(`Facility ID`:`Phone Number`, ~ length(unique(.x))))
  
  
# Drop fake identifiers (# unique values > # facilities)
dat <- dat %>% dplyr::select(-c(`Facility Name`, `Address`, `Phone Number`))
# Drop County Name (lower case, upper case, spaces)
dat <- dat %>% dplyr::select(-c(`County Name`))
# Drop City, State, and Zip code (some facilities moved to another state)
dat <- dat %>% dplyr::select(-c(`City`, `State`, `ZIP Code`))


## Check 'Not Available' across all string columns, change those to NA (actual missing)
dat %>%
  dplyr::mutate(across(where(is.character), ~replace(., . %in% c("Not Applicable", "Not Available"), NA))) -> dat


## Spread data
dat %>% 
  tidyr::spread(key = `Measure ID`, value = Score) -> wide_dat_timely


dim(wide_dat_timely)
length(unique(wide_dat_timely$`Facility ID`))


## Save
write_csv(wide_dat_timely, '../data_processed/wide_timely.csv', col_names = T) # save to a csv file
save(wide_dat_timely, file = "../data_processed/wide_timely.RData") # save to a .RData file



################################### Final combine

setwd("C:/Users/aejoh/Documents/GitHub/Health-data-competition-2022/data_processed")

load("wide_HCAHPS.RData")
load("wide_complication.RData")
load("wide_readmit.RData")
load("wide_infection.RData")
load("wide_timely.RData")

## Fix idS in wide HCAHPS to match others
head(wide_dat$`Facility ID`)
wide_dat$`Facility ID`[substr(wide_dat$`Facility ID`,1,1)=="0"] <- substr(wide_dat$`Facility ID`[substr(wide_dat$`Facility ID`,1,1)=="0"],2,6)
head(wide_dat$`Facility ID`)

names(wide_dat_complication)
## Keep track of which variables belong to which domain
var.domains <- list(Complications=names(wide_dat_complication)[9:27])

## Left join
analysis_data <- left_join(wide_dat,wide_dat_complication[,c(1,9:27)],by="Facility ID")

## Repeat for readmit, infection, timely care

names(wide_dat_readmit)
var.domains$Readmit <- names(wide_dat_readmit[9:22])

analysis_data <- left_join(analysis_data,wide_dat_readmit[,c(1,9:22)],by="Facility ID")


names(wide_dat_infection)
var.domains$Infection <- names(wide_dat_infection)[9:44]

analysis_data <- left_join(analysis_data,wide_dat_infection[,c(1,9:44)],by="Facility ID")


names(wide_dat_timely)
var.domains$Timely <- names(wide_dat_timely)[2:18]

analysis_data <- left_join(analysis_data,wide_dat_timely[,c(1,2:18)],by="Facility ID")

var.domains

save(var.domains,file="List of variables by domain.RData")
save(analysis_data,file="Analysis dataset.RData")
write.csv(analysis_data,file="Analysis dataset.csv")
