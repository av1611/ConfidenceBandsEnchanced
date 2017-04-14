# Cleaning out the workspace
rm (list = ls())
# setwd() to the dir where the script have been stored
if (! require("rstudioapi")) install.packages("rstudioapi")
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Loading the function
source("confint.norm.mu.r")
# Testing it
confint.norm.mu(replication.count = 10, 
                   super.replication.count = 15,
                   sample.size = 20, 
                   mu = 0, 
                   sigma = 1,
                   alpha = 0.25)

confint.norm.mu(replication.count = 50, 
                   super.replication.count = 50,
                   sample.size = 50, 
                   mu = 0, 
                   sigma = 1,
                   alpha = 0.25)



