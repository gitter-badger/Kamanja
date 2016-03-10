# Rattle is Copyright (c) 2006-2015 Togaware Pty Ltd.

#============================================================
# Rattle timestamp: 2016-03-07 15:10:41 x86_64-apple-darwin13.4.0 

# Rattle version 4.1.0 user 'billbruns'

# This log file captures all Rattle interactions as R commands. 

Export this log to a file using the Export button or the Tools 
# menu to save a log of all your activity. This facilitates repeatability. For example, exporting 
# to a file called 'myrf01.R' will allow you to type in the R Console 
# the command source('myrf01.R') and so repeat all actions automatically. 
# Generally, you will want to edit the file to suit your needs. You can also directly 
# edit this current log in place to record additional information before exporting. 
 
# Saving and loading projects also retains this log.

# We begin by loading the required libraries.

library(rattle)   # To access the weather dataset and utility commands.
library(magrittr) # For the %>% and %<>% operators.

# This log generally records the process of building a model. However, with very 
# little effort the log can be used to score a new dataset. The logical variable 
# 'building' is used to toggle between generating transformations, as when building 
# a model, and simply using the transformations, as when scoring a dataset.

building <- TRUE
scoring  <- ! building


# A pre-defined value is used to reset the random seed so that results are repeatable.

crv$seed <- 42 

#============================================================
# Rattle timestamp: 2016-03-07 15:11:01 x86_64-apple-darwin13.4.0 

# Load the data.

crs$dataset <- read.csv("file:///Users/billbruns/videos/Audit.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

#============================================================
# Rattle timestamp: 2016-03-07 15:11:02 x86_64-apple-darwin13.4.0 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 2000 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 1400 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 300 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 300 observations

# The following variable selections have been noted.

crs$input <- c("Age", "Employment", "Education", "Marital",
     "Occupation", "Income", "Gender", "Deductions",
     "Hours")

crs$numeric <- c("Age", "Income", "Deductions", "Hours")

crs$categoric <- c("Employment", "Education", "Marital", "Occupation",
     "Gender")

crs$target  <- "TARGET_Adjusted"
crs$risk    <- "RISK_Adjustment"
crs$ident   <- "ID"
crs$ignore  <- "IGNORE_Accounts"
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-03-07 15:56:19 x86_64-apple-darwin13.4.0 

# KMeans 

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# The 'reshape' package provides the 'rescaler' function.

library(reshape, quietly=TRUE)

# Generate a kmeans cluster of size 10.

crs$kmeans <- kmeans(sapply(na.omit(crs$dataset[crs$sample, crs$numeric]), rescaler, "range"), 10)

#============================================================
# Rattle timestamp: 2016-03-07 15:56:19 x86_64-apple-darwin13.4.0 

# Report on the cluster characteristics. 

# Cluster sizes:

paste(crs$kmeans$size, collapse=' ')

# Data means:

colMeans(sapply(na.omit(crs$dataset[crs$sample, crs$numeric]), rescaler, "range"))

# Cluster centers:

crs$kmeans$centers

# Within cluster sum of squares:

crs$kmeans$withinss

# Time taken: 0.00 secs

#============================================================
# Rattle timestamp: 2016-03-07 15:57:09 x86_64-apple-darwin13.4.0 

# Export KMeans 

# The 'pmml' package provides the 'pmml' function.

library(pmml, quietly=TRUE)

#============================================================
# Rattle timestamp: 2016-03-07 16:09:03 x86_64-apple-darwin13.4.0 

# Save the project data (variable crs) to file.

save(crs, file="/Users/billbruns/videos/clusterAudit.rattle", compress=TRUE)
