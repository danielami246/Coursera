# Coursera

##Getting data

Firstly, we get rid of the columns that with more than 40% NA.

There are three types of NAs in the data sets; "NA", "\#DIV/0!" and an empty space "". We classify all of those as "NA". 
Moreover, the first seven columns of the data sets are just information about users and time/dates, which are not important for the prediction of classe, therefore we do not need those.

#' trainData <- read.csv("pml-training.csv", header = T, na.strings = c("NA", "#DIV/0!", ""))[,-(1:7)]


