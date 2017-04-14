# Cleaning out the workspace
rm (list = ls())
# setwd() to the dir where the script have been stored
if (! require("rstudioapi")) install.packages("rstudioapi")
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Loading the function
source("confint.norm.sigma.r")
# Testing it
confint.norm.sigma(replication.count = 10, 
                   super.replication.count = 10,
                   sample.size = 10, 
                   mu = 0, 
                   sigma = 1,
                   alpha = 0.25)

confint.norm.sigma(replication.count = 50, 
                   super.replication.count = 50,
                   sample.size = 50, 
                   mu = 0, 
                   sigma = 1,
                   alpha = 0.25)


# Rprof(tmp <- tempfile())
# confint.norm.sigma(replication.count = 500, 
#                    super.replication.count = 500,
#                    sample.size = 500, 
#                    mu = 0, 
#                    sigma = 1,
#                    alpha = 0.25)
# Rprof()
# summaryRprof(tmp)
# 
# require(profr)
# require(ggplot2)
# x = profr(confint.norm.sigma(replication.count = 50, 
#                      super.replication.count = 50,
#                      sample.size = 50, 
#                      mu = 0, 
#                      sigma = 1,
#                      alpha = 0.25))
# ggplot(x)
