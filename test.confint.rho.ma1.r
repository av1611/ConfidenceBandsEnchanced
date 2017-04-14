# Cleaning out the workspace
rm (list = ls())
# setwd() to the dir where the script have been stored
if (! require("rstudioapi")) install.packages("rstudioapi")
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Loading the function
source("confint.rho.ma1.r")
# Testing it
confint.rho.ma1(replication.count = 10, 
                super.replication.count = 10,
                sample.size = 10,
                theta = .5,
                sigma = 1,
                mu = 0,
                alpha = 0.5)

confint.rho.ma1(replication.count = 10, 
                super.replication.count = 10,
                sample.size = 10,
                theta = .5,
                sigma = 5,
                mu = 0,
                alpha = 0.05)

confint.rho.ma1(replication.count = 50, 
                super.replication.count = 50,
                sample.size = 50, 
                theta = .5,
                sigma = 5,
                mu = 0,
                alpha = 0.5)
