# read feature names
feat.names = read.csv("features.txt", sep = "", header = FALSE, stringsAsFactors = FALSE)

# read activities
activities = read.csv("activity_labels.txt", sep = "", header = FALSE, stringsAsFactors = FALSE)
activities$V2

# read train files
train.X = read.csv("train/X_train.txt", header = FALSE, sep = "")
names(train.X)
names(train.X) = feat.names$V2

train.Y = read.csv("train/Y_train.txt", header = FALSE, sep = "")
names(train.Y) = "activity"

# remove all values but mean and std

var.names = names(train.X)
var.filters.mean = grep(x = var.names, pattern = "mean()")
var.names[var.filters.mean]

var.filters.std = grep(x = var.names, pattern = "std()")
var.names[var.filters.std]

var.filters.meanFreq = grep(x = var.names, pattern = "meanFreq()")
var.names[var.filters.meanFreq]

toKeep = setdiff(c(var.filters.mean, var.filters.std), var.filters.meanFreq)
toKeep

toEliminate = setdiff(1:ncol(train.X), toKeep)
toEliminate

train.X = train.X[ , -(toEliminate)]
summary(train.X)

# put activity names
library(plyr)
train.Y$activity = mapvalues(train.Y$activity, from = c("1", "2", "3", "4", "5", "6"), to = activities$V2)

# get subjects
train.subj = read.csv("train/subject_train.txt", header = FALSE, sep = "")

# merge labels
train = cbind(train.X, train.Y)

# which set the observations belong to
train$set = "train"


# read test files
test.X = read.csv("test/X_test.txt", header = FALSE, sep = "")
names(test.X)
names(test.X) = feat.names$V2

test.Y = read.csv("test/Y_test.txt", header = FALSE, sep = "")
names(test.Y) = "activity"

# remove all values but mean and std

var.names = names(test.X)
var.filters.mean = grep(x = var.names, pattern = "mean()")
var.names[var.filters.mean]

var.filters.std = grep(x = var.names, pattern = "std()")
var.names[var.filters.std]

var.filters.meanFreq = grep(x = var.names, pattern = "meanFreq()")
var.names[var.filters.meanFreq]

toKeep = setdiff(c(var.filters.mean, var.filters.std), var.filters.meanFreq)
toKeep

toEliminate = setdiff(1:ncol(test.X), toKeep)
toEliminate

test.X = test.X[ , -(toEliminate)]
summary(test.X)


# put activity names
library(plyr)
test.Y$activity = mapvalues(test.Y$activity, from = c("1", "2", "3", "4", "5", "6"), to = activities$V2)

# get subjects
test.subj = read.csv("test/subject_test.txt", header = FALSE, sep = "")

test = cbind(test.X, test.Y)

test$set = "test"

test$set

# merge training and testing set

dataset = rbind(test, train)
dataset$activity = as.factor(dataset$activity)
dataset$set = as.factor(dataset$set)
summary(dataset)

# create codebook  

time.dom.sigs = grep(names(dataset[1:66]), pattern = "^t")
names(dataset[time.dom.sigs])

freq.dom.sigs = setdiff(1:66, time.dom.sigs)
names(dataset[freq.dom.sigs])

mean.sigs = grep(names(dataset[1:66]), pattern = "mean")
names(dataset[mean.sigs])

std.sigs = grep(names(dataset[1:66]), pattern = "std")
names(dataset[std.sigs])

pat <- "^[t, f]";
temp = sub(pat, "\\1", names(dataset[1:66])[grepl(pat, names(dataset[1:66]))])

pat <- "\\-.+";
sigName = sub(pat, "\\1", temp[grepl(pat, temp)])

X.sigs = grep(names(dataset[1:66]), pattern = "X$")
names(dataset[X.sigs])

Y.sigs = grep(names(dataset[1:66]), pattern = "Y$")
names(dataset[Y.sigs])

Z.sigs = grep(names(dataset[1:66]), pattern = "Z$")
names(dataset[Z.sigs])



vec = character(66)

for (i in 1:length(mean.sigs)) {
  vec[mean.sigs[i]] = "mean value of"
  # append(vec[mean.sigs[i]], "mean value")
}

for (i in 1:length(std.sigs)) {
  vec[std.sigs[i]] = "standard deviation of"
  # append(vec[mean.sigs[i]], "mean value")
}

for (i in 1:66) {
  vec[i] = paste(vec[i], sigName[i])
}
sigName
vec

for (i in 1:length(time.dom.sigs)) {
  vec[time.dom.sigs[i]] = paste(vec[time.dom.sigs[i]], "in the time domain")
}

for (i in 1:length(freq.dom.sigs)) {
  vec[freq.dom.sigs[i]] = paste(vec[freq.dom.sigs[i]], "in the frequency domain (DFT)")
}

for (i in 1:length(X.sigs)) {
  vec[X.sigs[i]] = paste(vec[X.sigs[i]], "along the X axis.")
}

for (i in 1:length(Y.sigs)) {
  vec[Y.sigs[i]] = paste(vec[Y.sigs[i]], "along the Y axis.")
}

for (i in 1:length(Z.sigs)) {
  vec[Z.sigs[i]] = paste(vec[Z.sigs[i]], "along the Z axis.")
}

dataset$activity = as.factor(dataset$activity)

vec
names(dataset)
paste(levels(dataset$activity), collapse = " \n ")
vec = append(vec, "Ground truth activity. \n \n Levels: \n \n")
vec
vec[67] = paste(vec[67], paste(levels(dataset$activity), collapse = ", "))
vec

vec[68] = "Set that the sample belongs to: 'train', 'test'"
vec

names(dataset) = sub(x = names(dataset), pattern = "-mean\\(\\)", replacement = ".Mean")
names(dataset) = sub(x = names(dataset), pattern = "-std\\(\\)", replacement = ".Std")
names(dataset) = sub(x = names(dataset), pattern = "-", replacement = ".")
names(dataset)


fileConn = file("codebook.md")
writeLines("# Codebook for the tidy mobile activity dataset. \n## Features: \n \n", fileConn)
for (i in 1:68) {
  cat(paste("###", names(dataset)[i][1], ": \n"), file = "codebook.md", append = TRUE)
  cat(paste("    ", vec[i][1], " \n \n \n"), file = "codebook.md", append = TRUE)
}
close(fileConn)
 
write.table(x = dataset, file = "mainDataset.txt", row.name = FALSE) 
