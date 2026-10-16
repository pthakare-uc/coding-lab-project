#load the librarbies

library(tidyverse)

raw_chicago_data <- read_csv("data/Chicago_Public_Schools_-_School_Progress_Reports_SY2425_20261016.csv")

#Inspect the data

glimpse(raw_chicago_data)
head(raw_chicago_data)
colnames(raw_chicago_data)
dim(raw_chicago_data)

chicago_clean <- raw_chicago_data

