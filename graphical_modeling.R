library(qgraph)
library(GGMncv)
library(tidyverse)




loadRData <- function(fileName){
#loads an RData file, and returns it
    load(fileName)
    get(ls()[ls() != "fileName"])
}


setwd("/home/liu00980/Documents/8801project/HCAHPS")


load("data_processed/dat_complication_imputed.RData")
load("data_processed/dat_infection_imputed.RData")
load("data_processed/dat_readmit_imputed.RData")
load("data_processed/dat_timely_imputed.RData")
load("data_processed/dat_allresponses_imputed.RData") # v2
load("data_processed/data_allresponses_imputed.RData") # v1
load("data_processed/List of variables by domain.RData") # var.domains
names(var.domains) <- c("complication", "readmit", "infection", "timely")

# a utils function
index.domain <- function(dat, domain = "experience") {
    if(domain == "experience")
        index <- which(endsWith(names(dat), "_LINEAR_SCORE"))
    else
        index <- which(names(dat) %in% var.domains[[domain]])
    return(index)
}


## graphical modeling with individual imputed datasets

response <- "complication"
# response <- "infection"
# response <- "readmit"
# response <- "timely"

filepath <- paste("data_processed/dat_", response, "_imputed.RData", sep = "")
temp_dat <- na.omit(loadRData(filepath))

groups <- list()
groups[["experience"]] <- index.domain(temp_dat, "experience")
groups[[response]] <- index.domain(temp_dat, response)

# partial correlation graph obtained from GLASSO
fit <- EBICglasso(cor(temp_dat), nrow(temp_dat))

fit_temp1 <- ggmncv(cor(temp_dat), n = nrow(temp_dat), penalty = "lasso", ic = "ebic")
fdr_temp2 <- inference(fit_temp1, method = "fdr", alpha = 0.05)
fit <- fit * fdr_temp2$adj

# partial correlation obtained from graphical lasso

qgraph(
    fit,
    cut = 0.2,
    groups = groups,
    title = "Partial Correlation Graph from GLASSO",
    repulsion = 0.7
    )

# # save figures
# qgraph(
#     fit,
#     cut = 0.2,
#     groups = groups,
#     title = "Partial Correlation Graph from GLASSO",
#     repulsion = 0.7,
#     filetype = "jpg",
#     filename = paste("figures/", tools::toTitleCase(response), "-GLASSO", sep = "")
#     )








## graphical modeling using the altogether imputed dataset (v1)

dat_all <- temp_imputed
dat_all %>%
    dplyr::select(-contains(c("DOPC", "EDV"))) %>% as.data.frame() -> dat_all
dim(dat_all)
cormatrix <- cor(dat_all)

# # approach 1
# fit_all <- EBICglasso(cormatrix, nrow(dat_all))
# fit_temp1 <- ggmncv(cor(dat_all), n = nrow(dat_all), penalty = "lasso", ic = "ebic")
# fdr_temp2 <- inference(fit_temp1, method = "fdr", alpha = 0.05)
# fit_all <- fit_all * fdr_temp2$adj

# approach 2
fit_temp1 <- ggmncv(cor(dat_all), n = nrow(dat_all), penalty = "lasso", ic = "ebic")
pcor_debias <- desparsify(fit_temp1)$P
# pcor_debias <- ppcor::pcor(dat_all)$estimate
# pcor_debias <- cor(dat_all)
fdr_temp2 <- inference(fit_temp1, method = "fdr", alpha = 0.05)
pcor_debias <- pcor_debias * fdr_temp2$adj

groups <- list()
for (response in c("experience", names(var.domains)))
    groups[[response]] <- index.domain(dat_all, response)

# some exploration
rownames(pcor_debias) <- names(dat_all)
colnames(pcor_debias) <- names(dat_all)

sum(pcor_debias != 0) / 2 / (ncol(pcor_debias) * (ncol(pcor_debias) - 1) / 2)

temp <- pcor_debias


temp <- pcor_debias[groups$experience, c(groups$complication, groups$readmit, groups$infection, groups$timely)]
sum(temp != 0)
length(temp)
mean(temp != 0)

index.pair <- which(temp != 0, arr.ind = T)
sig.pair <- data.frame(
    measure1 = names(dat_all)[groups$experience][index.pair[, 1]],
    measure2 = names(dat_all)[c(groups$complication, groups$readmit, groups$infection, groups$timely)][index.pair[, 2]],
    pcor = temp[which(temp != 0)]
)
rownames(sig.pair) <- NULL

sig.pair

sig.pair.sortbyE <- sig.pair[order(sig.pair$measure1), ]

sig.pair.sortbyE


# overall graph
qgraph(
    pcor_debias,
    minimum = 0.05,
    cut = 0.2,
    groups = groups,
    layout = "spring",
    labels = 1:ncol(dat_all),
    vsize = 3.5,
    esize = 4,
    repulsion = 0.9,
    # edge.color = "black",
    title = "Partial Correlation Graph from Debiased GLASSO under Level 0.05"
    )


# only care about within-group correlation and experience vs responses
pcor_debias[groups$complication, groups$readmit] <- 0
pcor_debias[groups$complication, groups$infection] <- 0
pcor_debias[groups$complication, groups$timely] <- 0
pcor_debias[groups$readmit, groups$complication] <- 0
pcor_debias[groups$readmit, groups$infection] <- 0
pcor_debias[groups$readmit, groups$timely] <- 0
pcor_debias[groups$infection, groups$complication] <- 0
pcor_debias[groups$infection, groups$readmit] <- 0
pcor_debias[groups$infection, groups$timely] <- 0
pcor_debias[groups$timely, groups$complication] <- 0
pcor_debias[groups$timely, groups$readmit] <- 0
pcor_debias[groups$timely, groups$infection] <- 0

prefixs <- rep(c("E", "C", "R", "I", "T"), sapply(groups, length))
nums <- unlist(sapply(groups, function(x) 1:length(x)))
mylabels <- paste(prefixs, nums, sep = "")

qgraph(
    pcor_debias,
    minimum = 0.05,
    cut = 0.15,
    groups = groups,
    layout = "spring",
    labels = mylabels,
    nodeNames = names(dat_all),
    legend.cex = 0.25,
    vsize = 4.6,
    esize = 6,
    repulsion = 0.9,
    title = "Partial Correlation Graph from Debiased GLASSO under Level 0.05"
    )


# # save figures
# EBICgraph <- qgraph(
#     fit_all, 
#     groups = groups,
#     layout = "circular",
#     labels = 1:ncol(dat_all),
#     vsize = 3,
#     title = "Partial Correlation Graph from GLASSO",
#     filetype = "jpg",
#     filename = "figures/Allresponses-GLASSO"
#     )

centralityPlot(EBIC = EBICgraph)


## study the relationship of interest based on the subgraph
# 1: clean score
# 2:7: staff responsiveness score
# 8: rating score
# 9: quiet score
# 10: recommendation score
# 12:14: mortality rate
# 44: OP_18b


pcor_debias[1:10, 12:14]

names(dat_all)[49:51]


plot(dat_all[, c(15, 19:22)])
plot(dat_all[, c(49, 50, 51)])

pcor_debias[49, 50]
    
    
    
    


## build DAG with PC algorithm

library(pcalg)

suffStat <- list(C = cormatrix, n = nrow(dat_allresponses_imputed))
pc.fit <- pc(suffStat, indepTest = gaussCItest, p = ncol(dat_allresponses_imputed), alpha = 0.05)

qgraph(
    pc.fit,
    groups = groups,
    layout = "circle",
    title = "DAG Obtained from PC Algorithm",
    repulsion = 0.7,
    vsize = 3
)

summary(pc.fit)















    
