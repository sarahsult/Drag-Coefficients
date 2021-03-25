library(utils) #need for read.csv
library(stats) #for hypothesis tests
library(graphics) #for boxplots

#load data
data_hits <- read.csv("/Users/ssult/Documents/Third Year/Internship Applications/data_sample.csv", header=TRUE)

#1) looking at acceleration between the mound and the plate
  #-->the more the ball slows down the more air resistance there is (drag) all else being constant
#add an acceleration column to data_hits and populate in for each observation (the more neg the more it slowed down)
dist_to_plate_miles <- 50/5280
acceleration <- ((data_hits$plate_speed)^2 - (data_hits$release_speed)^2)/(2*dist_to_plate_miles)
data_hits$acceleration <- acceleration #acceleration very large because it is in miles/hr^2

#avg 2015
hits_2015 <- subset(data_hits, year == 2015)
avg_acc_2015 <- mean(hits_2015$acceleration)

#avg 2016
hits_2016 <- subset(data_hits, year == 2016)
avg_acc_2016 <- mean(hits_2016$acceleration)

#avg 2017
hits_2017 <- subset(data_hits, year == 2017)
avg_acc_2017 <- mean(hits_2017$acceleration)

#avg 2018
hits_2018 <- subset(data_hits, year == 2018)
avg_acc_2018 <- mean(hits_2018$acceleration)

#avg 2019
hits_2019 <- subset(data_hits, year == 2019)
avg_acc_2019 <- mean(hits_2019$acceleration)

#test to make sure variances are the same then test to see if means are different
#2015 vs. 2016
result_varTest1516 <- var.test(hits_2015$acceleration, hits_2016$acceleration) #greater than .05 --> equal
result_varTest1516
result_meanTest1516 <- t.test(hits_2015$acceleration, hits_2016$acceleration, var.equal = TRUE)
result_meanTest1516 #greater than .05 --> equal

#2015 vs. 2017
result_varTest1517 <- var.test(hits_2015$acceleration, hits_2017$acceleration) #less than .05 --> NOT equal
result_varTest1517
result_meanTest1517 <- t.test(hits_2015$acceleration, hits_2017$acceleration, var.equal = FALSE)
result_meanTest1517 #less than .05 --> NOT equal

#2015 vs. 2018
result_varTest1518 <- var.test(hits_2015$acceleration, hits_2018$acceleration) #less than .05 --> NOT equal
result_varTest1518
result_meanTest1518 <- t.test(hits_2015$acceleration, hits_2018$acceleration, var.equal = FALSE)
result_meanTest1518 #less than .05 --> NOT equal

#2015 vs. 2019
result_varTest1519 <- var.test(hits_2015$acceleration, hits_2019$acceleration) #less than .05 --> NOT equal
result_varTest1519
result_meanTest1519 <- t.test(hits_2015$acceleration, hits_2019$acceleration, var.equal = FALSE)
result_meanTest1519 #less than .05 --> NOT equal

#2016 vs. 2017
result_varTest1617 <- var.test(hits_2016$acceleration, hits_2017$acceleration) #less than .05 --> NOT equal
result_varTest1617
result_meanTest1617 <- t.test(hits_2016$acceleration, hits_2017$acceleration, var.equal = FALSE)
result_meanTest1617 #less than .05 --> NOT equal

#2016 vs. 2018
result_varTest1618 <- var.test(hits_2016$acceleration, hits_2018$acceleration) #less than .05 --> NOT equal
result_varTest1618
result_meanTest1618 <- t.test(hits_2016$acceleration, hits_2018$acceleration, var.equal = FALSE)
result_meanTest1618 #greater than .05 --> equal

#2016 vs. 2019
result_varTest1619 <- var.test(hits_2016$acceleration, hits_2019$acceleration) #less than .05 --> NOT equal
result_varTest1619
result_meanTest1619 <- t.test(hits_2016$acceleration, hits_2019$acceleration, var.equal = FALSE)
result_meanTest1619 #less than .05 --> NOT equal

#2017 vs. 2018
result_varTest1718 <- var.test(hits_2017$acceleration, hits_2018$acceleration) #greater than .05 --> equal
result_varTest1718
result_meanTest1718 <- t.test(hits_2017$acceleration, hits_2018$acceleration, var.equal = TRUE)
result_meanTest1718 #less than .05 --> NOT equal

#2017 vs. 2019
result_varTest1719 <- var.test(hits_2017$acceleration, hits_2019$acceleration) #greater than .05 --> equal
result_varTest1719
result_meanTest1719 <- t.test(hits_2017$acceleration, hits_2019$acceleration, var.equal = TRUE)
result_meanTest1719 #greater than .05 --> equal

#2018 vs. 2019
result_varTest1819 <- var.test(hits_2018$acceleration, hits_2019$acceleration) #greater than .05 --> equal
result_varTest1819
result_meanTest1819 <- t.test(hits_2018$acceleration, hits_2019$acceleration, var.equal = TRUE)
result_meanTest1819 #less than .05 --> NOT equal

boxplot(data_hits$acceleration ~ data_hits$year,
        main = "Hits by Acceleration",
        xlab = "Year",
        ylab = "Acceleration (miles/hr^2)",)

#2) Looking at the average distance homeruns traveled over the years
all_homeruns <- subset(data_hits, event_result == "home_run")
homeruns_2015 <- subset(hits_2015, event_result=="home_run")
homeruns_2016 <- subset(hits_2016, event_result=="home_run")
homeruns_2017 <- subset(hits_2017, event_result=="home_run")
homeruns_2018 <- subset(hits_2018, event_result=="home_run")
homeruns_2019 <- subset(hits_2019, event_result=="home_run")

#2015 vs. 2016
var_homeruns1516 <- var.test(homeruns_2015$hit_distance, homeruns_2016$hit_distance) #greater than .05 --> equal
var_homeruns1516
mean_homeruns1516 <- t.test(homeruns_2015$hit_distance, homeruns_2016$hit_distance, var.equal = TRUE)
mean_homeruns1516 #greater than .05 --> equal

#2015 vs. 2017
var_homeruns1517 <- var.test(homeruns_2015$hit_distance, homeruns_2017$hit_distance) #greater than .05 --> equal
var_homeruns1517
mean_homeruns1517 <- t.test(homeruns_2015$hit_distance, homeruns_2017$hit_distance, var.equal = TRUE)
mean_homeruns1517 #greater than .05 --> equal

#2015 vs. 2018
var_homeruns1518 <- var.test(homeruns_2015$hit_distance, homeruns_2018$hit_distance) #greater than .05 --> equal
var_homeruns1518
mean_homeruns1518 <- t.test(homeruns_2015$hit_distance, homeruns_2018$hit_distance, var.equal = TRUE)
mean_homeruns1518 #greater than .05 --> equal

#2015 vs. 2019
var_homeruns1519 <- var.test(homeruns_2015$hit_distance, homeruns_2019$hit_distance) #greater than .05 --> equal
var_homeruns1516
mean_homeruns1519 <- t.test(homeruns_2015$hit_distance, homeruns_2019$hit_distance, var.equal = TRUE)
mean_homeruns1519 #greater than .05 --> equal

#2016 vs. 2017
var_homeruns1617 <- var.test(homeruns_2016$hit_distance, homeruns_2017$hit_distance) #greater than .05 --> equal
var_homeruns1617
mean_homeruns1617 <- t.test(homeruns_2016$hit_distance, homeruns_2017$hit_distance, var.equal = TRUE)
mean_homeruns1617 #greater than .05 --> equal

#2016 vs. 2018
var_homeruns1618 <- var.test(homeruns_2016$hit_distance, homeruns_2018$hit_distance) #greater than .05 --> equal
var_homeruns1618
mean_homeruns1618 <- t.test(homeruns_2016$hit_distance, homeruns_2018$hit_distance, var.equal = TRUE)
mean_homeruns1618 #greater than .05 --> equal

#2016 vs. 2019
var_homeruns1619 <- var.test(homeruns_2016$hit_distance, homeruns_2019$hit_distance) #greater than .05 --> equal
var_homeruns1619
mean_homeruns1619 <- t.test(homeruns_2016$hit_distance, homeruns_2019$hit_distance, var.equal = TRUE)
mean_homeruns1617 #greater than .05 --> equal

#2017 vs. 2018
var_homeruns1718 <- var.test(homeruns_2017$hit_distance, homeruns_2018$hit_distance) #greater than .05 --> equal
var_homeruns1718
mean_homeruns1718 <- t.test(homeruns_2017$hit_distance, homeruns_2018$hit_distance, var.equal = TRUE)
mean_homeruns1718 #greater than .05 --> equal

#2017 vs. 2019
var_homeruns1719 <- var.test(homeruns_2017$hit_distance, homeruns_2019$hit_distance) #greater than .05 --> equal
var_homeruns1719
mean_homeruns1719 <- t.test(homeruns_2017$hit_distance, homeruns_2019$hit_distance, var.equal = TRUE)
mean_homeruns1719 #greater than .05 --> equal

#2018 vs. 2019
var_homeruns1819 <- var.test(homeruns_2018$hit_distance, homeruns_2019$hit_distance) #greater than .05 --> equal
var_homeruns1819
mean_homeruns1819 <- t.test(homeruns_2018$hit_distance, homeruns_2019$hit_distance, var.equal = TRUE)
mean_homeruns1819 #greater than .05 --> equal

boxplot(all_homeruns$hit_distance ~ all_homeruns$year,
        main = "Hits by Home Run Distance",
        xlab = "Year",
        ylab = "Home Run Distance (feet)",)


