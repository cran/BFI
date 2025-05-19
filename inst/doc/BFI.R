## ----setup, include = FALSE---------------------------------------------------
require(knitr)
opts_chunk$set(
  collapse = F # T for red
)

## ----include = FALSE----------------------------------------------------------
# First set a CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

## ----message=FALSE, results='hide'--------------------------------------------
# Install and load the BFI package from CRAN:
install.packages("BFI")
library(BFI)

## -----------------------------------------------------------------------------
data(package = "BFI")

## -----------------------------------------------------------------------------
# Get the number of rows and columns
dim(trauma)

# To get an idea of the data set, print the first 7 rows
head(trauma, 7)

## -----------------------------------------------------------------------------
# Get some info about the data set from the help file
?trauma

## -----------------------------------------------------------------------------
trauma$age <- scale(trauma$age)
trauma$ISS <- scale(trauma$ISS) 
trauma$GCS <- scale(trauma$GCS) 
trauma$hospital <- as.factor(trauma$hospital)

## -----------------------------------------------------------------------------
length(levels(trauma$hospital))

## -----------------------------------------------------------------------------
# Center 1:
# X1 <- data.frame(sex=trauma$sex[trauma$hospital==1],
#                  age=trauma$age[trauma$hospital==1],
#                  ISS=trauma$ISS[trauma$hospital==1],
#                  GCS=trauma$GCS[trauma$hospital==1])
X1 <- subset(trauma, hospital == 1, select = c(sex, age, ISS, GCS))
Lambda1 <- inv.prior.cov(X1, lambda=0.01, L=3, family="binomial")
fit1 <- MAP.estimation(y=trauma$mortality[trauma$hospital==1], X=X1, family="binomial", Lambda=Lambda1)
summary(fit1)

# Center 2:
# X2 <- data.frame(sex=trauma$sex[trauma$hospital==2],
#                  age=trauma$age[trauma$hospital==2],
#                  ISS=trauma$ISS[trauma$hospital==2],
#                  GCS=trauma$GCS[trauma$hospital==2])
X2 <- subset(trauma, hospital == 2, select = c(sex, age, ISS, GCS))
Lambda2 <- inv.prior.cov(X2, lambda=0.01, L=3, family="binomial")
fit2 <- MAP.estimation(y=trauma$mortality[trauma$hospital==2], X=X2, family="binomial", Lambda=Lambda2)
summary(fit2)

# Center 3:
# X3 <- data.frame(sex=trauma$sex[trauma$hospital==3],
#                  age=trauma$age[trauma$hospital==3],
#                  ISS=trauma$ISS[trauma$hospital==3],
#                  GCS=trauma$GCS[trauma$hospital==3])
X3 <- subset(trauma, hospital == 3, select = c(sex, age, ISS, GCS))
Lambda3 <- inv.prior.cov(X3, lambda=0.01, L=3, family="binomial")
fit3 <- MAP.estimation(y=trauma$mortality[trauma$hospital==3], X=X3, family="binomial", Lambda=Lambda3)
summary(fit3)

## -----------------------------------------------------------------------------
# Example for Center 3:
fit3 <- MAP.estimation(y=trauma$mortality[trauma$hospital==3], X=X3, family="binomial", Lambda=Lambda3, control = list(maxit=500))

## -----------------------------------------------------------------------------
# number of samples in center 1
fit1$n
# number of parameters in center 1
fit1$np

# number of samples in center 2
fit2$n

# number of samples in center 3
fit3$n

## ----eval = FALSE-------------------------------------------------------------
# # Save fit1 as an RDS file
# saveRDS(fit1, file="fit1.rds")
# 
# # Save fit2 as an RDS file
# saveRDS(fit2, file="fit2.rds")
# 
# # Save fit3 as an RDS file
# saveRDS(fit3, file="fit3.rds")

## ----eval = FALSE-------------------------------------------------------------
# # Load the RDS files
# fit1 <- readRDS("fit1.rds") # use the relative path to the file
# fit2 <- readRDS("fit2.rds") # use the relative path to the file
# fit3 <- readRDS("fit3.rds") # use the relative path to the file

## -----------------------------------------------------------------------------
theta_hats <- list(fit1$theta_hat, fit2$theta_hat, fit3$theta_hat)
A_hats     <- list(fit1$A_hat, fit2$A_hat, fit3$A_hat)
Lambda_com <- inv.prior.cov(X1, lambda=0.01, L=3, family="binomial")
Lambdas    <- list(Lambda1, Lambda2, Lambda3, Lambda_com)
BFI_fits   <- bfi(theta_hats, A_hats, Lambdas, family="binomial")
summary(BFI_fits, cur_mat=TRUE)

## -----------------------------------------------------------------------------
# MAP estimates of the combined data:
X_combined  <- data.frame(sex=trauma$sex,
                          age=trauma$age,
                          ISS=trauma$ISS,
                          GCS=trauma$GCS)
Lambda   <- inv.prior.cov(X=X_combined, lambda=0.01, L=3, family="binomial")
fit_comb  <- MAP.estimation(y=trauma$mortality, X=X_combined, family="binomial", Lambda=Lambda) 
summary(fit_comb, cur_mat=TRUE)

## -----------------------------------------------------------------------------
# Squared Errors:
(fit_comb$theta_hat - BFI_fits$theta_hat)^2

