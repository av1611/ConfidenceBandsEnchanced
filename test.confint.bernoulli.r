# Cleaning out the workspace
rm (list = ls())
# setwd() to the dir where the script have been stored
if (! require("rstudioapi")) install.packages("rstudioapi")
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Loading the function
source("confint.bernoulli.r")
# Testing it
confint.bernoulli(replication.count = 10, 
                  super.replication.count = 10,
                  sample.size = 10, 
                  p = 0.5,
                  alpha = 0.25)

confint.bernoulli(replication.count = 50, 
                  super.replication.count = 50,
                  sample.size = 50, 
                  p = 0.5,
                  alpha = 0.25)
