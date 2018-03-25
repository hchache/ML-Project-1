# Import required libraries
library("data.table", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

# Read data from csv file
training.data<-read.csv("~/Downloads/BlogFeedback/blogData_train.csv",header=FALSE, na.strings=c(""))
# Extract attributes 51 to 60 (both inclusive) and call them basic features
lm.training.data.basic <- subset(training.data, select = c(63:262,281))

# -------------------------- LINEAR REGRESSION --------------------------

# Build a trained model
lm.model.basic <- lm(V281~., data=lm.training.data.basic)
# Performance Report
summary(lm.model.basic)

# ---------------- LINEAR REGRESSION - SELECTED COLUMNS ------------------

# Build a trained model based on selected column
lm.model.basic.sel <- lm(V281~V69+V77+V102+V125+V154+V170+V184+V191+V194+V195+V210+V219+V228+V232+V241+V251, data=lm.training.data.basic)
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
# 83.46968
mse.lm.basic.sel <- mean ((predict.train.data.sel$fit)^2)
# 72.79843
mse.lm.basic.feb <- mean ((predict.feb.test.data$fit)^2)
# 74.60372
mse.lm.basic.mar <- mean ((predict.mar.test.data$fit)^2)
# 73.00842
mse.lm.basic.sel.feb <- mean ((predict.feb.test.data.sel$fit)^2)
# 66.03322
mse.lm.basic.sel.mar <- mean ((predict.mar.test.data.sel$fit)^2)
# 63.05738

# -------------------------- TESTING THE MODEL - LINEAR REGRESSION - TWO TEST DATASETS ----------------------------

feb.test.data.06<-read.csv("~/Downloads/BlogFeedback/Feb/blogData_test-2012.02.06.00_00.csv",header=FALSE, na.strings=c(""))
feb.test.data.06 <- subset(feb.test.data.06, select = c(63:262,281))

feb.test.data.25<-read.csv("~/Downloads/BlogFeedback/Feb/blogData_test-2012.02.25.00_00.csv",header=FALSE, na.strings=c(""))
feb.test.data.25 <- subset(feb.test.data.25, select = c(63:262,281))

mar.test.data.08<-read.csv("~/Downloads/BlogFeedback/Mar/blogData_test-2012.03.08.00_00.csv",header=FALSE, na.strings=c(""))
mar.test.data.08 <- subset(mar.test.data.08, select = c(63:262,281))

mar.test.data.23<-read.csv("~/Downloads/BlogFeedback/Mar/blogData_test-2012.03.23.00_00.csv",header=FALSE, na.strings=c(""))
mar.test.data.23 <- subset(mar.test.data.23, select = c(63:262,281))

predict.feb.test.data.06<-predict(lm.model.basic, feb.test.data.06, se.fit = TRUE)
predict.feb.test.data.06.sel<-predict(lm.model.basic.sel, feb.test.data.06, se.fit = TRUE)
predict.feb.test.data.25<-predict(lm.model.basic, feb.test.data.25, se.fit = TRUE)
predict.feb.test.data.25.sel<-predict(lm.model.basic.sel, feb.test.data.25, se.fit = TRUE)

mse.lm.basic.feb.06 <- mean ((predict.feb.test.data.06$fit)^2)
# 92.15032
mse.lm.basic.sel.feb.06 <- mean ((predict.feb.test.data.06.sel$fit)^2)
# 82.41736
mse.lm.basic.feb.25 <- mean ((predict.feb.test.data.25$fit)^2)
# 86.45197
mse.lm.basic.sel.feb.25 <- mean ((predict.feb.test.data.25.sel$fit)^2)
# 74.99393

predict.mar.test.data.08<-predict(lm.model.basic, mar.test.data.08, se.fit = TRUE)
predict.mar.test.data.08.sel<-predict(lm.model.basic.sel, mar.test.data.08, se.fit = TRUE)
predict.mar.test.data.23<-predict(lm.model.basic, mar.test.data.23, se.fit = TRUE)
predict.mar.test.data.23.sel<-predict(lm.model.basic.sel, mar.test.data.23, se.fit = TRUE)

mse.lm.basic.mar.08 <- mean ((predict.mar.test.data.08$fit)^2)
# 70.79979
mse.lm.basic.sel.mar.08 <- mean ((predict.mar.test.data.08.sel$fit)^2)
# 66.81595
mse.lm.basic.mar.23 <- mean ((predict.mar.test.data.23$fit)^2)
# 58.47147
mse.lm.basic.sel.mar.23 <- mean ((predict.mar.test.data.23.sel$fit)^2)
# 51.79943
