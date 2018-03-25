# Import required libraries
library("data.table", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

# Read data from csv file
training.data<-read.csv("~/Downloads/BlogFeedback/blogData_train.csv",header=FALSE, na.strings=c(""))
# Extract attributes 51 to 60 (both inclusive) and call them basic features
lm.training.data.basic <- subset(training.data, select = c(51:60,281))

# -------------------------- LINEAR REGRESSION --------------------------

# Build a trained model
lm.model.basic <- lm(V281~., data=lm.training.data.basic)
# Performance Report
summary(lm.model.basic)

# ---------------- LINEAR REGRESSION - SELECTED COLUMNS ------------------

# Build a trained model based on selected column
lm.model.basic.sel <- lm(V281~.-V53-V55-V57-V58-V60, data=lm.training.data.basic)
# Performance Report
summary(lm.model.basic.sel)

# -------------------------- TESTING THE MODEL - LINEAR REGRESSION - ENTIRE TEST DATASET ----------------------------

# Install data.table Library. Version: data.table 1.10.4.3
# install.packages('data.table')
# library("data.table", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

# Merge files in Feb folder and create feb.test.data variable
# This can be also done with pattern attribute as follows:
# test.filenames = list.files(pattern = "blogData.test-2012.02")
files <- list.files(path = "~/Downloads/BlogFeedback/Feb",pattern = ".csv")
data.dir <- paste(getwd(),'Downloads/BlogFeedback/Feb',sep = "/")
temp <- lapply(paste(data.dir,files,sep = "/"), read.csv,header=FALSE)
data <- rbindlist( temp )
feb.test.data<-data

# Merge files in Mar folder and create mar.test.data variable
files <- list.files(path = "~/Downloads/BlogFeedback/Mar",pattern = ".csv")
data.dir <- paste(getwd(),'Downloads/BlogFeedback/Mar',sep = "/")
temp <- lapply(paste(data.dir,files,sep = "/"), read.csv,header=FALSE)
data <- rbindlist( temp )
mar.test.data<-data

# Predicting Training Dataset
predict.train.data<-predict(lm.model.basic, lm.training.data.basic, se.fit = TRUE)

# Predicting Training Dataset
predict.train.data.sel<-predict(lm.model.basic.sel, lm.training.data.basic, se.fit = TRUE)

# Predicting Test Dataset
predict.feb.test.data<-predict(lm.model.basic, feb.test.data, se.fit = TRUE)
predict.mar.test.data<-predict(lm.model.basic, mar.test.data, se.fit = TRUE)
predict.feb.test.data.sel<-predict(lm.model.basic.sel, feb.test.data, se.fit = TRUE)
predict.mar.test.data.sel<-predict(lm.model.basic.sel, mar.test.data, se.fit = TRUE)

mse.lm.basic <- mean ((predict.train.data$fit)^2)
# 366.3478
mse.lm.basic.sel <- mean ((predict.train.data.sel$fit)^2)
# 365.7374
mse.lm.basic.feb <- mean ((predict.feb.test.data$fit)^2)
# 289.7674
mse.lm.basic.mar <- mean ((predict.mar.test.data$fit)^2)
# 308.6731
mse.lm.basic.sel.feb <- mean ((predict.feb.test.data.sel$fit)^2)
# 285.9533
mse.lm.basic.sel.mar <- mean ((predict.mar.test.data.sel$fit)^2)
# 300.1138

# -------------------------- TESTING THE MODEL - LINEAR REGRESSION - TWO TEST DATASETS ----------------------------

feb.test.data.06<-read.csv("~/Downloads/BlogFeedback/Feb/blogData_test-2012.02.06.00_00.csv",header=FALSE, na.strings=c(""))
feb.test.data.06 <- subset(feb.test.data.06, select = c(51:60,281))

feb.test.data.25<-read.csv("~/Downloads/BlogFeedback/Feb/blogData_test-2012.02.25.00_00.csv",header=FALSE, na.strings=c(""))
feb.test.data.25 <- subset(feb.test.data.25, select = c(51:60,281))

mar.test.data.08<-read.csv("~/Downloads/BlogFeedback/Mar/blogData_test-2012.03.08.00_00.csv",header=FALSE, na.strings=c(""))
mar.test.data.08 <- subset(mar.test.data.08, select = c(51:60,281))

mar.test.data.23<-read.csv("~/Downloads/BlogFeedback/Mar/blogData_test-2012.03.23.00_00.csv",header=FALSE, na.strings=c(""))
mar.test.data.23 <- subset(mar.test.data.23, select = c(51:60,281))

predict.feb.test.data.06<-predict(lm.model.basic, feb.test.data.06, se.fit = TRUE)
predict.feb.test.data.06.sel<-predict(lm.model.basic.sel, feb.test.data.06, se.fit = TRUE)
predict.feb.test.data.25<-predict(lm.model.basic, feb.test.data.25, se.fit = TRUE)
predict.feb.test.data.25.sel<-predict(lm.model.basic.sel, feb.test.data.25, se.fit = TRUE)

mse.lm.basic.feb.06 <- mean ((predict.feb.test.data.06$fit)^2)
# 170.0224
mse.lm.basic.sel.feb.06 <- mean ((predict.feb.test.data.06.sel$fit)^2)
# 179.3881
mse.lm.basic.feb.25 <- mean ((predict.feb.test.data.25$fit)^2)
# 362.6647
mse.lm.basic.sel.feb.25 <- mean ((predict.feb.test.data.25.sel$fit)^2)
# 357.8765

predict.mar.test.data.08<-predict(lm.model.basic, mar.test.data.08, se.fit = TRUE)
predict.mar.test.data.08.sel<-predict(lm.model.basic.sel, mar.test.data.08, se.fit = TRUE)
predict.mar.test.data.23<-predict(lm.model.basic, mar.test.data.23, se.fit = TRUE)
predict.mar.test.data.23.sel<-predict(lm.model.basic.sel, mar.test.data.23, se.fit = TRUE)

mse.lm.basic.mar.08 <- mean ((predict.mar.test.data.08$fit)^2)
# 427.9151
mse.lm.basic.sel.mar.08 <- mean ((predict.mar.test.data.08.sel$fit)^2)
# 434.834
mse.lm.basic.mar.23 <- mean ((predict.mar.test.data.23$fit)^2)
# 113.1113
mse.lm.basic.sel.mar.23 <- mean ((predict.mar.test.data.23.sel$fit)^2)
# 114.2254
