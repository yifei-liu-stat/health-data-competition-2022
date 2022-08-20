library(naniar)
library(mice)

setwd("/home/liu00980/Documents/8801project/HCAHPS")

load("data_processed/Analysis dataset.RData") # analysis_data
load("data_processed/List of variables by domain.RData") # var.domains


names.predictors <- names(dplyr::select(analysis_data, contains("LINEAR_SCORE")))
names.responses <- unlist(var.domains)
names(names.responses) <- NULL
names.facilityid <- analysis_data$`Facility ID`

dat <- dplyr::select(analysis_data, one_of(c(names.predictors, names.responses)))

# further get rid of CI, numerator, denominator columns from infection dataset
dat <- dplyr::select(dat, -contains(c("CILOWER", "CIUPPER", "ELIGCASES", "NUMERATOR")))

# deal with factor EDV (treated as discrete numeric variable)
EDV.mapping <- function(EDV) {
    if (is.na(EDV)) return(NA)
    else {
        if (EDV == "low") return("1")
        if (EDV == "medium") return("2")
        if (EDV == "high") return("3")
        if (EDV == "very high") return("4")   
    }
}
EDV.mapping <- Vectorize(EDV.mapping, "EDV", USE.NAMES = FALSE)
# EDV.mapping(c("low", "medium", "high", "very high", NA))
dat <- dplyr::mutate(dat, EDV = EDV.mapping(EDV))

# all to numeric (whether it's continuous or discrete [such as EDV])
dat <- dplyr::mutate_all(dat, as.numeric)

dat <- as.data.frame(dat)
rownames(dat) <- names.facilityid



set.seed(1234)

## Missing value exploration for complications
dat_complication <- dat[, c(names.predictors, var.domains[[1]])]
vis_miss(dat_complication) # almost half of the entries are missing

# Delete rows and columns with more than 50% missing values
somerows <- which(rowMeans(is.na(dat_complication)) <= 0.5)
somecolumns <- which(colMeans(is.na(dat_complication)) <= 0.5)
dat_complication <- dat_complication[somerows, somecolumns]
vis_miss(dat_complication) # much better now

# impute dataset
dat_complication_imputed <- complete(mice(dat_complication, method = "norm.predict", m = 1))
save(dat_complication_imputed, file = "data_processed/dat_complication_imputed.RData")


## Missing value exploration for readmit
dat_readmit <- dat[, c(names.predictors, var.domains[[2]])]
vis_miss(dat_readmit) # almost half of the entries are missing

# Delete rows and columns with more than 50% missing values
somerows <- which(rowMeans(is.na(dat_readmit)) <= 0.5)
somecolumns <- which(colMeans(is.na(dat_readmit)) <= 0.5)
dat_readmit <- dat_readmit[somerows, somecolumns]
vis_miss(dat_readmit) # much better now

# impute dataset
dat_readmit_imputed <- complete(mice(dat_readmit, method = "norm.predict", m = 1))
save(dat_readmit_imputed, file = "data_processed/dat_readmit_imputed.RData")


## Missing value exploration for infections
# note that columns _DOPC (number of device days) have really large scale
names.infections <- names(dat)[names(dat) %in% var.domains[[3]]]
dat_infection <- dat[, c(names.predictors, names.infections)]
vis_miss(dat_infection) # almost half of the entries are missing

# Delete rows and columns with more than 50% missing values
somerows <- which(rowMeans(is.na(dat_infection)) <= 0.5)
somecolumns <- which(colMeans(is.na(dat_infection)) <= 0.5)
dat_infection <- dat_infection[somerows, somecolumns]
vis_miss(dat_infection) # much better now

# impute dataset
dat_infection_imputed <- complete(mice(dat_infection, method = "norm.predict", m = 1))
save(dat_infection_imputed, file = "data_processed/dat_infection_imputed.RData")


## Missing value exploration for timely
dat_timely <- dat[, c(names.predictors, var.domains[[4]])]
vis_miss(dat_timely) # almost half of the entries are missing

# Delete rows and columns with more than 50% missing values
somerows <- which(rowMeans(is.na(dat_timely)) <= 0.5)
somecolumns <- which(colMeans(is.na(dat_timely)) <= 0.5)
dat_timely <- dat_timely[somerows, somecolumns]
vis_miss(dat_timely) # much better now

# impute dataset
myblocks <- list(
    norm = setdiff(names(dat_timely), "EDV"),
    pmm = "EDV"
)
dat_timely_imputed <- complete(mice(dat_timely, method = c("norm.predict", "pmm"), blocks = myblocks, m = 1))
save(dat_timely_imputed, file = "data_processed/dat_timely_imputed.RData")




## deal with missing values for the whole dataset
temp <- dat
vis_miss(temp)

# Delete rows and columns with more than 50% missing values
somerows <- which(rowMeans(is.na(temp)) <= 0.5)
somecolumns <- which(colMeans(is.na(temp)) <= 0.5)
temp <- temp[somerows, somecolumns]
vis_miss(temp) # much better now

# impute dataset
myblocks <- list(
    norm = setdiff(names(temp), "EDV"),
    pmm = "EDV"
)
temp_imputed <- complete(mice(temp, method = c("norm.predict", "pmm"), blocks = myblocks, m = 1))
save(temp_imputed, file = "data_processed/data_allresponses_imputed.RData")
