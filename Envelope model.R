setwd("C:/Users/aejoh/Documents/GitHub/Health-data-competition-2022")
library(Renvlp)
library(dplyr)
load("data/data_allresponses_imputed.RData")
df <- temp_imputed

#############
# Data prep #
#############


for (j in 1:51){
  print(j)
  print(names(df)[j])
  print(summary(df[,j]))
}

# 26:  EDAC_30_HF right-skewed
# 27: EDAC_30_PN right-skewed
# 35-40: Drop as discussed
# 42: ED_2b right-skewed
# 43: EDV Drop, not really an indicator
# 45: OP_18c right-skewed


for (j in c(26,27,42,45)){
  hist(df[,j],main=names(df)[j])
}

# EDAC vars actually look fine
# Chop off extreme outliers for the others

sum(df$ED_2b>750)
which(df$ED_2b>750)
df <- df[-321,]

which(df$OP_18c>2000)
df <- df[-1829,]

# And log+1 transform
df$ED_2b <- log1p(df$ED_2b)
df$OP_18c <- log1p(df$OP_18c)

# Delete the columns commented on above
df <- df[,-c(35:40,43)]

df$`Facility ID` <- row.names(df)

# Load this to add infection data

load("data/Analysis dataset.RData")
names(analysis_data)
infection <- analysis_data[,c(1,144,150,156,162,168)]
df.infection <- na.omit(left_join(df,infection,by="Facility ID"))


# Add medical spending
spend <- read.csv("data/final_analysis_data.csv")
names(spend)[1] <- "Facility ID"
spend$`Facility ID` <- as.character(spend$`Facility ID`)
df.2 <- left_join(df,spend[,c(1,192)],by="Facility ID")

# Remove Facility ID again
df <- df[,-45]
df.infection <- df.infection[,-45]
df.2 <- df.2[,-45]
df.2 <- na.omit(df.2)

############
# Envelope #
############

X <- as.matrix(df[,1:10])
Y <- as.matrix(df[,-(1:10)])
p <- ncol(X)
r <- ncol(Y)

# Need to get a u; pick by cross-validation

pred.errs <- numeric(r)
for (u in 1:r){
  print(u)
  pred.errs[u] <- cv.env(X,Y,u,10,50)
}
(choose.u <- data.frame("DimEnvelope"=1:34,"CVPredError"=pred.errs))
with(choose.u,plot(CVPredError~DimEnvelope,type="l"))
save(choose.u,file="Envelope/Cross-Validated prediction errors.RData")

# Elbows: 2, 12, 25
## Zoom in without u=1:

with(choose.u,plot(CVPredError[-1]~DimEnvelope[-1],type="l"))

# CV 25, or maybe 14

u.env(X,Y)

# AIC: 25
# BIC: 8
# LRT: 25
## Go with 25



mod1 <- env(X,Y,25)

# Overall F-test
results <- testcoef.env(m=mod1,L=diag(r),R=diag(p),A=matrix(0,nrow=r,ncol=p))
## P-value 0

# Coefficient-specific
tester.f <- function(m){
  # Plug in a model, get p-values for all coefficients (unadjusted)
  L <- matrix(0,nrow=1,ncol=r)
  R <- matrix(0,nrow=p,ncol=1)
  pvals <- matrix(1,nrow=r,ncol=p)
  for (i in 1:r){
    L[i] <- 1
    for (j in 1:p){
      R[j] <- 1
      pvals[i,j] <- testcoef.env(m,L,R,A=matrix(0,nrow=1,ncol=1))$pValue
      R[j] <- 0
    }
    L[i] <- 0
  }
  pvals
}
results <- tester.f(mod1)
rej <- .05/340
sum(results<rej) # 44 significant results at the .05 level, Bonferroni adjusted!

hots <- which(results<rej,arr.ind = TRUE)
xnames <- names(df)[1:10]
ynames <- names(df[-(1:10)])

(out <- data.frame("SurveyVar" = xnames[hots[,2]],
                   "ClinicalVar" = ynames[hots[,1]],
                   "Coefficient" = round(mod1$beta[hots],3),
                   "P-value" = round(results[hots],5)))
save(out,file="Envelope/Final summary base model.RData")


###############
# W/ spending #
###############

X <- as.matrix(df.2[,c(1:10,45)])
Y <- as.matrix(df.2[,-c(1:10,45)])
p <- ncol(X)
r <- ncol(Y)

# Need to get a u; pick by cross-validation

pred.errs.2 <- numeric(r)
for (u in 1:r){
  print(u)
  pred.errs.2[u] <- cv.env(X,Y,u,10,50)
}
(choose.u.2 <- data.frame("DimEnvelope"=1:r,"CVPredError"=pred.errs.2))
with(choose.u.2,plot(CVPredError~DimEnvelope,type="l"))
save(choose.u.2,file="Envelope/Cross-Validated prediction errors, with spending.RData")

# Elbow: 2
## Zoom in without u=1:

with(choose.u.2,plot(CVPredError[-1]~DimEnvelope[-1],type="l"))

# CV 13, 25

u.env(X,Y)

# AIC: 24
# BIC: 5
# LRT: 25
## Go with 25



mod2 <- env(X,Y,25)

# Coefficient tests
results <- tester.f(mod2)
rej <- .05/(ncol(X)*ncol(Y))
sum(results<rej) # 41 results

hots <- which(results<rej,arr.ind = TRUE)
xnames <- names(df.2)[c(1:10,45)]
ynames <- names(df.2[-c(1:10,45)])

(out2 <- data.frame("SurveyVar" = xnames[hots[,2]],
                   "ClinicalVar" = ynames[hots[,1]],
                   "Coefficient" = round(mod2$beta[hots],3),
                   "P-value" = round(results[hots],5)))

save(out2,file="Envelope/Final summary, spending model.RData")


#################
# W/ infections #
#################

X <- as.matrix(df.infection[,1:10])
Y <- as.matrix(df.infection[,-(1:10)])
p <- ncol(X)
r <- ncol(Y)

# Need to get a u; pick by cross-validation

pred.errs.3 <- numeric(r)
for (u in 1:r){
  print(u)
  pred.errs.3[u] <- cv.env(X,Y,u,10,50)
}
(choose.u.3 <- data.frame("DimEnvelope"=1:34,"CVPredError"=pred.errs.3))
with(choose.u.3,plot(CVPredError~DimEnvelope,type="l"))
save(choose.u.3,file="Envelope/Cross-Validated prediction errors, with infections.RData")

# Elbows: 2, 12, 25
## Zoom in without u=1:

with(choose.u.2,plot(CVPredError[-1]~DimEnvelope[-1],type="l"))

# CV 25, or maybe 14

u.env(X,Y)

# AIC: 25
# BIC: 8
# LRT: 25
## Go with 25



mod3 <- env(X,Y,25)

# Overall F-test
results <- testcoef.env(m=mod1,L=diag(r),R=diag(p),A=matrix(0,nrow=r,ncol=p))
## P-value 0

# Coefficient-specific
tester.f <- function(m){
  # Plug in a model, get p-values for all coefficients (unadjusted)
  L <- matrix(0,nrow=1,ncol=r)
  R <- matrix(0,nrow=p,ncol=1)
  pvals <- matrix(1,nrow=r,ncol=p)
  for (i in 1:r){
    L[i] <- 1
    for (j in 1:p){
      R[j] <- 1
      pvals[i,j] <- testcoef.env(m,L,R,A=matrix(0,nrow=1,ncol=1))$pValue
      R[j] <- 0
    }
    L[i] <- 0
  }
  pvals
}
results <- tester.f(mod1)
rej <- .05/(ncol(X)*ncol(Y))
sum(results<rej) # 44 significant results at the .05 level, Bonferroni adjusted!

hots <- which(results<rej,arr.ind = TRUE)
xnames <- names(df.infection)[1:10]
ynames <- names(df.infection[-(1:10)])

(out3 <- data.frame("SurveyVar" = xnames[hots[,2]],
                   "ClinicalVar" = ynames[hots[,1]],
                   "Coefficient" = round(mod1$beta[hots],3),
                   "P-value" = round(results[hots],5)))

save(out3,file="Envelope/Final summary, infections included.RData")