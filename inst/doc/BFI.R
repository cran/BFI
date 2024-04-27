## ----setup, include = FALSE---------------------------------------------------
require(knitr)
opts_chunk$set(
  collapse = F # T for red
)

## ----message=FALSE------------------------------------------------------------
# First install and load the package 'devtools'
#if(!require(devtools)) {install.packages("devtools")}
library(devtools)

# Now install BFI from GitHub
#devtools::install_github("hassanpazira/BFI", force = TRUE)

# load BFI
library(BFI)

## -----------------------------------------------------------------------------
data(package = "BFI")

## -----------------------------------------------------------------------------
# Load 'trauma' in the R workspace
data("trauma")

# Get the number of rows and columns
dim(trauma)

# To get an idea of the dataset, print the first 7 rows
head(trauma, 7)

## -----------------------------------------------------------------------------
(col_name <- colnames(trauma))

## -----------------------------------------------------------------------------
# Get some info about the dataset from the help file
?trauma

## -----------------------------------------------------------------------------

