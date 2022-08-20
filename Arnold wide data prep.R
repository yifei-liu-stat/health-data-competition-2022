library(tidyverse)
## Read Data
setwd("C:/Users/aejoh/Documents/GitHub/Health-data-competition-2022/Arnold data prep")
dat <- readr::read_csv("./HCAHPS-17.csv", 
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
hcahps17 <- wide_dat

dat <- readr::read_csv("./HCAHPS-18.csv", 
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
hcahps18 <- wide_dat

dat <- readr::read_csv("./HCAHPS-19.csv", 
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

hcahps19 <- wide_dat



########################################################################################

# Some indicators not included in 2017, which are they?

hcahps18 <- as.data.frame(hcahps18)
hcahps19 <- as.data.frame(hcahps19)

badcols <- which((names(hcahps18) %in% names(hcahps17))==FALSE)
names(hcahps18)[badcols]

# I want to keep these, just look at 2 years then. Ho-hum.

# 16 fewer hospitals in 19 compared to 18

badrows <- which((hcahps19$`Facility ID`%in%hcahps18$`Facility ID`)==FALSE)
not18 <- hcahps19$`Facility ID`[badrows]
badrows <- which((hcahps18$`Facility ID`%in%hcahps19$`Facility ID`)==FALSE)
not19 <- hcahps18$`Facility ID`[badrows]
badids <- c(not18,not19) # 154 cases where the hospital drops in or out
commonids <- unique(c(hcahps18$`Facility ID`,hcahps19$`Facility ID`))
commonids <- commonrows[(commonrows%in%badids)==FALSE]

# Create 2 year HCAHPS which weighted-averages the factors

hcahps18$`Number of Completed Surveys` <- as.numeric(hcahps18$`Number of Completed Surveys`)
hcahps19$`Number of Completed Surveys` <- as.numeric(hcahps19$`Number of Completed Surveys`)

hcahps18$`Survey Response Rate Percent` <- as.numeric(hcahps18$`Survey Response Rate Percent`)
hcahps19$`Survey Response Rate Percent` <- as.numeric(hcahps19$`Survey Response Rate Percent`)

for (j in 13:ncol(hcahps18)){
  hcahps18[,j] <- as.numeric(hcahps18[,j])
  hcahps19[,j] <- as.numeric(hcahps19[,j])
}

hcahps_2yr <- rbind(hcahps19,hcahps18[hcahps18$`Facility ID`%in%not19,])
hcahps_2yr <- hcahps_2yr[hcahps_2yr$`Facility ID`%in%commonids,]

na_sum <- function(x,y,wt=c(1,1)){
  # Compute a weighted sum but 0 out NAs in the summand or weights
  w1 <- ifelse(is.na(wt[1]),0,wt[1])
  w2 <- ifelse(is.na(wt[2]),0,wt[2])
  x <- ifelse(is.na(as.numeric(x)),0,as.numeric(x))
  y <- ifelse(is.na(as.numeric(y)),0,as.numeric(y))
  w1*x+w2*y
}

na_avg <- function(x,y,wt=c(1,1)){
  na_sum(x=x,y=y,wt=wt)/na_sum(x=x/x,y=y/y,wt=wt)
}


# This loop takes about 10-15 minutes, run at your own risk
for (i in 1:nrow(hcahps_2yr)){
  id <- hcahps_2yr$`Facility ID`[i]
  row18 <- hcahps18[hcahps18$`Facility ID`==id,]
  row19 <- hcahps19[hcahps19$`Facility ID`==id,]
  
  # Simple sum of complete surveys
  w18 <- row18$`Number of Completed Surveys`
  w19 <- row19$`Number of Completed Surveys`
  hcahps_2yr$`Number of Completed Surveys`[i] <- na_sum(w18,w19)
  
  # calculate pct
  n18 <- round(w18/(row18$`Survey Response Rate Percent`/100)) # surveys sent out
  n19 <- round(w19/(row19$`Survey Response Rate Percent`/100))
  
  hcahps_2yr$`Survey Response Rate Percent`[i] <- hcahps_2yr$`Number of Completed Surveys`[i] / na_sum(n18,n19)
  
  # for all indicators
  for (j in 13:ncol(hcahps_2yr)){
    # weighted average of non-missing fellas
    hcahps_2yr[i,j] <- round(na_avg(row18[j],row19[j],wt=c(w18,w19)))
  }
}

nrow(na.omit(hcahps_2yr)) # 3471 complete cases

save(hcahps_2yr,file="HCAHPS-1819.RData")
write.csv(hcahps_2yr,file="HCAHPS-1819.csv")


###########################################################################


