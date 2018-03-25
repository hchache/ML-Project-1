# Import required libraries
library("data.table", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
# Read data from csv file
training.data<-read.csv("~/Downloads/BlogFeedback/blogData_train.csv",header=FALSE, na.strings=c(""))
# Extract attributes 51 to 60 (both inclusive) and call them basic features
lm.training.data.basic <- subset(training.data, select = c(63:262,281))

# Process target variable: Set values greater than 0 as 1
for( i in 1 : nrow(lm.training.data.basic)){ if(lm.training.data.basic$V281[i]>mean(lm.training.data.basic$V281)) {lm.training.data.basic$V281[i]<-1}else{lm.training.data.basic$V281[i]<-0}}
# Build a trained model
logit.model.basic.dm <- glm(V281~.,family=binomial(link='logit'),data = lm.training.data.basic )
# Performance Report
summary(logit.model.basic.dm)

# --------------- LOGISTIC REGRESSION - DATA MANUPULATION - SELECTED COLUMN --------------

# Build a trained model
logit.model.basic.dm.sel <- glm(V281~V69+V77+V102+V125+V154+V170+V184+V191+V194+V195+V210+V219+V228+V232+V241+V251,family=binomial(link='logit'),data = lm.training.data.basic )
# Performance Report
summary(logit.model.basic.dm.sel)

# --------------------------- TESTING THE MODEL ----------------------------

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

# Process target variable: Set values greater than 0 as 1
feb.test.data.dm <- feb.test.data
for( i in 1 : nrow(feb.test.data.dm)){ if(feb.test.data.dm$V281[i]>mean(feb.test.data.dm$V281)) {feb.test.data.dm$V281[i]<-1}else{feb.test.data.dm$V281[i]<-0}}

mar.test.data.dm <- mar.test.data
for( i in 1 : nrow(mar.test.data.dm)){ if(mar.test.data.dm$V281[i]>mean(mar.test.data.dm$V281)) {mar.test.data.dm$V281[i]<-1}else{mar.test.data.dm$V281[i]<-0}}

# Calculating accuracy of predicted result
predict.train.data.lg.dm <- predict(logit.model.basic.dm,lm.training.data.basic, se.fit = TRUE)
predict.train.data.lg.dm$fit[predict.train.data.lg.dm$fit > mean(predict.train.data.lg.dm$fit)] = 1
predict.train.data.lg.dm$fit[predict.train.data.lg.dm$fit <= mean(predict.train.data.lg.dm$fit)] = 0

cm.train.data.basic <- confusionMatrix(predict.train.data.lg.dm$fit, unlist(lm.training.data.basic$V281))

# Predecting Test Data
predict.feb.test.data.lg.dm<-predict(logit.model.basic.dm,feb.test.data.dm, se.fit = TRUE)
predict.feb.test.data.lg.dm$fit[predict.feb.test.data.lg.dm$fit > mean(predict.feb.test.data.lg.dm$fit)] = 1
predict.feb.test.data.lg.dm$fit[predict.feb.test.data.lg.dm$fit <= mean(predict.feb.test.data.lg.dm$fit)] = 0

cm.feb.test.data.lg.dm <- confusionMatrix(predict.feb.test.data.lg.dm$fit, unlist(feb.test.data.dm$V281))


predict.mar.test.data.lg.dm<-predict(logit.model.basic.dm,mar.test.data.dm, se.fit = TRUE)
predict.mar.test.data.lg.dm$fit[predict.mar.test.data.lg.dm$fit > mean(predict.mar.test.data.lg.dm$fit)] = 1
predict.mar.test.data.lg.dm$fit[predict.mar.test.data.lg.dm$fit <= mean(predict.mar.test.data.lg.dm$fit)] = 0

cm.mar.test.data.lg.dm <- confusionMatrix(predict.mar.test.data.lg.dm$fit, unlist(mar.test.data.dm$V281))


# --------------- TESTING THE MODEL - SELECTED COLUMN --------------

# Build a trained model
logit.model.basic.dm.sel <- glm(V281~V69+V77+V102+V125+V154+V170+V184+V191+V194+V195+V210+V219+V228+V232+V241+V251,family=binomial(link='logit'),data = lm.training.data.basic )

# Calculating accuracy of predicted result
predict.train.data.lg.dm.sel <- predict(logit.model.basic.dm.sel,lm.training.data.basic, se.fit = TRUE)
predict.train.data.lg.dm.sel$fit[predict.train.data.lg.dm.sel$fit > mean(predict.train.data.lg.dm.sel$fit)] = 1
predict.train.data.lg.dm.sel$fit[predict.train.data.lg.dm.sel$fit <= mean(predict.train.data.lg.dm.sel$fit)] = 0

cm.train.data.basic.sel <- confusionMatrix(predict.train.data.lg.dm.sel$fit, unlist(lm.training.data.basic$V281))

predict.feb.test.data.lg.dm.sel<-predict(logit.model.basic.dm.sel,feb.test.data.dm, se.fit = TRUE)
predict.feb.test.data.lg.dm.sel$fit[predict.feb.test.data.lg.dm.sel$fit > mean(predict.feb.test.data.lg.dm.sel$fit)] = 1
predict.feb.test.data.lg.dm.sel$fit[predict.feb.test.data.lg.dm.sel$fit <= mean(predict.feb.test.data.lg.dm.sel$fit)] = 0

cm.feb.test.data.lg.dm.sel <- confusionMatrix(predict.feb.test.data.lg.dm.sel$fit, unlist(feb.test.data.dm$V281))


predict.mar.test.data.lg.dm.sel<-predict(logit.model.basic.dm.sel,mar.test.data.dm, se.fit = TRUE)
predict.mar.test.data.lg.dm.sel$fit[predict.mar.test.data.lg.dm.sel$fit > mean(predict.mar.test.data.lg.dm.sel$fit)] = 1
predict.mar.test.data.lg.dm.sel$fit[predict.mar.test.data.lg.dm.sel$fit <= mean(predict.mar.test.data.lg.dm.sel$fit)] = 0

cm.mar.test.data.lg.dm.sel <- confusionMatrix(predict.mar.test.data.lg.dm.sel$fit, unlist(mar.test.data.dm$V281))

# --------------- TESTING THE MODEL - TWO DATASETS --------------

feb.test.data.06<-read.csv("~/Downloads/BlogFeedback/Feb/blogData_test-2012.02.06.00_00.csv",header=FALSE, na.strings=c(""))
feb.test.data.06 <- subset(feb.test.data.06, select = c(63:262,281))

feb.test.data.25<-read.csv("~/Downloads/BlogFeedback/Feb/blogData_test-2012.02.25.00_00.csv",header=FALSE, na.strings=c(""))
feb.test.data.25 <- subset(feb.test.data.25, select = c(63:262,281))

mar.test.data.08<-read.csv("~/Downloads/BlogFeedback/Mar/blogData_test-2012.03.08.00_00.csv",header=FALSE, na.strings=c(""))
mar.test.data.08 <- subset(mar.test.data.08, select = c(63:262,281))

mar.test.data.23<-read.csv("~/Downloads/BlogFeedback/Mar/blogData_test-2012.03.23.00_00.csv",header=FALSE, na.strings=c(""))
mar.test.data.23 <- subset(mar.test.data.23, select = c(63:262,281))

for( i in 1 : nrow(feb.test.data.06)){ if(feb.test.data.06$V281[i]>mean(feb.test.data.06$V281)) {feb.test.data.06$V281[i]<-1}else{feb.test.data.06$V281[i]<-0}}

for( i in 1 : nrow(feb.test.data.25)){ if(feb.test.data.25$V281[i]>mean(feb.test.data.25$V281)) {feb.test.data.25$V281[i]<-1}else{feb.test.data.25$V281[i]<-0}}

for( i in 1 : nrow(mar.test.data.08)){ if(mar.test.data.08$V281[i]>mean(mar.test.data.08$V281)) {mar.test.data.08$V281[i]<-1}else{mar.test.data.08$V281[i]<-0}}

for( i in 1 : nrow(mar.test.data.23)){ if(mar.test.data.23$V281[i]>mean(mar.test.data.23$V281)) {mar.test.data.23$V281[i]<-1}else{mar.test.data.23$V281[i]<-0}}

predict.feb.test.data.lg.06<-predict(logit.model.basic.dm,feb.test.data.06, se.fit = TRUE)
predict.feb.test.data.lg.06$fit[predict.feb.test.data.lg.06$fit > mean(predict.feb.test.data.lg.06$fit)] = 1
predict.feb.test.data.lg.06$fit[predict.feb.test.data.lg.06$fit <= mean(predict.feb.test.data.lg.06$fit)] = 0

cm.feb.test.data.lg.06 <- confusionMatrix(predict.feb.test.data.lg.06$fit, unlist(feb.test.data.06$V281))

predict.feb.test.data.lg.25<-predict(logit.model.basic.dm,feb.test.data.25, se.fit = TRUE)
predict.feb.test.data.lg.25$fit[predict.feb.test.data.lg.25$fit > mean(predict.feb.test.data.lg.25$fit)] = 1
predict.feb.test.data.lg.25$fit[predict.feb.test.data.lg.25$fit <= mean(predict.feb.test.data.lg.25$fit)] = 0

cm.feb.test.data.lg.25 <- confusionMatrix(predict.feb.test.data.lg.25$fit, unlist(feb.test.data.25$V281))


predict.feb.test.data.lg.06.sel<-predict(logit.model.basic.dm.sel,feb.test.data.06, se.fit = TRUE)
predict.feb.test.data.lg.06.sel$fit[predict.feb.test.data.lg.06.sel$fit > mean(predict.feb.test.data.lg.06.sel$fit)] = 1
predict.feb.test.data.lg.06.sel$fit[predict.feb.test.data.lg.06.sel$fit <= mean(predict.feb.test.data.lg.06.sel$fit)] = 0

cm.feb.test.data.lg.06.sel <- confusionMatrix(predict.feb.test.data.lg.06.sel$fit, unlist(feb.test.data.06$V281))

predict.feb.test.data.lg.25.sel<-predict(logit.model.basic.dm.sel,feb.test.data.25, se.fit = TRUE)
predict.feb.test.data.lg.25.sel$fit[predict.feb.test.data.lg.25.sel$fit > mean(predict.feb.test.data.lg.25.sel$fit)] = 1
predict.feb.test.data.lg.25.sel$fit[predict.feb.test.data.lg.25.sel$fit <= mean(predict.feb.test.data.lg.25.sel$fit)] = 0

cm.feb.test.data.lg.25.sel <- confusionMatrix(predict.feb.test.data.lg.25.sel$fit, unlist(feb.test.data.25$V281))


predict.mar.test.data.lg.08<-predict(logit.model.basic.dm,mar.test.data.08, se.fit = TRUE)
predict.mar.test.data.lg.08$fit[predict.mar.test.data.lg.08$fit > mean(predict.mar.test.data.lg.08$fit)] = 1
predict.mar.test.data.lg.08$fit[predict.mar.test.data.lg.08$fit <= mean(predict.mar.test.data.lg.08$fit)] = 0

cm.mar.test.data.lg.08 <- confusionMatrix(predict.mar.test.data.lg.08$fit, unlist(mar.test.data.08$V281))
cm.mar.test.data.lg.08$byClass["Balanced Accuracy"]

predict.mar.test.data.lg.23<-predict(logit.model.basic.dm,mar.test.data.23, se.fit = TRUE)
predict.mar.test.data.lg.23$fit[predict.mar.test.data.lg.23$fit > mean(predict.mar.test.data.lg.23$fit)] = 1
predict.mar.test.data.lg.23$fit[predict.mar.test.data.lg.23$fit <= mean(predict.mar.test.data.lg.23$fit)] = 0

cm.mar.test.data.lg.23 <- confusionMatrix(predict.mar.test.data.lg.23$fit, unlist(mar.test.data.23$V281))
cm.mar.test.data.lg.23$byClass["Balanced Accuracy"]

predict.mar.test.data.lg.08.sel<-predict(logit.model.basic.dm.sel,mar.test.data.08, se.fit = TRUE)
predict.mar.test.data.lg.08.sel$fit[predict.mar.test.data.lg.08.sel$fit > mean(predict.mar.test.data.lg.08.sel$fit)] = 1
predict.mar.test.data.lg.08.sel$fit[predict.mar.test.data.lg.08.sel$fit <= mean(predict.mar.test.data.lg.08.sel$fit)] = 0

cm.mar.test.data.lg.08.sel <- confusionMatrix(predict.mar.test.data.lg.08.sel$fit, unlist(mar.test.data.08$V281))
cm.mar.test.data.lg.08.sel$byClass["Balanced Accuracy"]

predict.mar.test.data.lg.23.sel<-predict(logit.model.basic.dm.sel,mar.test.data.23, se.fit = TRUE)
predict.mar.test.data.lg.23.sel$fit[predict.mar.test.data.lg.23.sel$fit > mean(predict.mar.test.data.lg.23.sel$fit)] = 1
predict.mar.test.data.lg.23.sel$fit[predict.mar.test.data.lg.23.sel$fit <= mean(predict.mar.test.data.lg.23.sel$fit)] = 0

cm.mar.test.data.lg.23.sel <- confusionMatrix(predict.mar.test.data.lg.23.sel$fit, unlist(mar.test.data.23$V281))
cm.mar.test.data.lg.23.sel$byClass["Balanced Accuracy"]
