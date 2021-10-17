#Load libraries-----
library("tidyverse")
library("data.table")

#READ AND ORGANIZE DATA-----------------------------------------
grades = read.table(url("http://www.math.uni.wroc.pl/~elsner/dydaktyka/dane/grades.txt"),
                    col.names = c("id", "GPA", "IQ", "Gender", "Psych"))

income = read.table(url("http://www.math.uni.wroc.pl/~elsner/dydaktyka/dane/income.dat"),
                    col.names = c("id", "Age", "Education", "Gender", "Income", "Job"),
                    colClasses = c("integer", "integer", "factor", "factor", "integer", "factor"))

levels(income$Gender) <- c("Male", "Female")
levels(income$Job) <- c("Private sector", "Public sector", "Self-employed")
levels(income$Education) <- c("did not reach high school", 
                              "some high school, without diploma", 
                              "high school diploma", 
                              "some college, no bachelor's degree", 
                              "bachelor's degree", "postgraduate degree")

#TASK 1a------------------------------------------------------
#funtion for printing stats and base for all plots of grades dtf
show_statistics <- function(vec){
  words <- c("min: ", "max: ", "spread: ", "median: ", "1st quartile: ", "3rd quartile: ", "mean: ", "standard deviation: ", "variance: ", "coefficient of variation: ")
  stats <- c(min(vec, na.rm = TRUE), 
            max(vec, na.rm = TRUE), 
            max(vec, na.rm = TRUE) - min(vec, na.rm = TRUE), #spread
            median(vec, na.rm = TRUE), 
            quantile(vec, na.rm = TRUE)[2], 
            quantile(vec, na.rm = TRUE)[4], 
            mean(vec, na.rm = TRUE), 
            sd(vec, na.rm = TRUE),
            var(vec, na.rm = TRUE),
            sd(vec, na.rm = TRUE) / abs(mean(vec, na.rm = TRUE)))
  print(deparse(substitute(vec)))
  print(paste(words, stats, sep = ""))
}

base_plot <- function(vec, binw){
  return (
    ggplot(grades, aes(x = vec)) +
      geom_histogram(binwidth =binw, 
                     color = "white",
                     fill = "blue",
                     alpha = 0.7) +
      theme_bw()
  )
}

#Histogram 1: IQ---------------
#skewed left and unimodal, with its peak around 115 points.
#It's spread can be read from show_statistics. Center = median = 110.
show_statistics(grades$IQ)
base_plot(grades$IQ, 5) +
  scale_x_continuous(breaks = seq(70, 140, by = 5)) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("IQ")

#Histogram 2: GPA---------------
#irregular shape, multimodal with main peak around 7. Skewed left.
show_statistics(grades$GPA)
base_plot(grades$GPA, 0.5) +
  scale_x_continuous(breaks = seq(0, 11, by = 0.5)) +
  scale_y_continuous(breaks = seq(0, 14, by = 1)) +
  xlab("GPA")

#PHistogram 3: Psych---------------
#skewed left, multimodal histogram with its main peak around 66 points.
show_statistics(grades$Psych)
base_plot(grades$Psych, 5) + 
  scale_x_continuous(breaks = seq(15, 85, by = 5)) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("Psychological test score")

#TASK 1b-----------------------------------------------------------------
grades_f <- filter(grades, Gender == "F")
grades_m <- filter(grades, Gender == "M")

#Histogram 1: IQ---------
#histogram for female students is unimodal and more symmetric, while histogram for male
#students is bimodal and skewed left. peaks of both histograms are located around value 
#of 105-110 points.
show_statistics(grades_f$IQ)
show_statistics(grades_m$IQ)
base_plot(grades$IQ, 5) + 
  scale_x_continuous(breaks = seq(60, 145, by = 10)) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("IQ") + 
  facet_wrap(~ Gender)

#Histogram2: GPA------------
#Both histograms are multimodal with main peaks around 7.5 points, skewed left.
show_statistics(grades_f$GPA)
show_statistics(grades_m$GPA)
base_plot(grades$GPA, 0.5) +
  scale_x_continuous(breaks = seq(0, 11, by = 0.5)) +
  scale_y_continuous(breaks = seq(0, 14, by = 1)) +
  xlab("GPA") +
  facet_wrap(~Gender)

#Histogram3: Psychological test-------------
#Histogram for female students is skewed left and unimodal, while histogram for
#male students is more irregular, bimodal.
show_statistics(grades_f$Psych)
show_statistics(grades_m$Psych)
base_plot(grades$Psych, 10) +
  scale_x_continuous(breaks = seq(20, 80, by = 5)) +
  scale_y_continuous(breaks = seq(0, 18, by = 1)) +
  xlab("Psychological test score") +
  facet_wrap(~Gender)

#TASK 2-------------------------------------------------------------------------
base_plot(grades$IQ, 1) +
  scale_x_continuous(breaks = seq(70, 140, by = 5)) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("binwidth = 1") #it's really unreadable and makes it harder to recognize statistical tendencies

base_plot(grades$IQ, 2) +
  scale_x_continuous(breaks = seq(70, 140, by = 5)) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  xlab("binwidth = 2") #it's still a bit unreadable, but definitely better

base_plot(grades$IQ, 3) +
  scale_x_continuous(breaks = seq(70, 140, by = 5)) +
  xlab("binwidth = 3") #I find it acceptable number of classes

base_plot(grades$IQ, 5) +
  scale_x_continuous(breaks = seq(70, 140, by = 5)) +
  xlab("binwidth = 5") #I find this the best number of classes, but binwidth = 3 or 4 is also good

base_plot(grades$IQ, 7) +
  scale_x_continuous(breaks = seq(70, 140, by = 5)) +
  xlab("binwidth = 7") #here we're losing too much data

base_plot(grades$IQ, 10) +
  scale_x_continuous(breaks = seq(70, 140, by = 5)) +
  xlab("binwidth = 10") 
#now students with IQ difference of 10 points are in the same bin. This is too much,
#considering the fact that it is 10/64 > 15% of range.

#TASK 3-------------------------------------
mark_outliers <- function(vec, a = 1.5){ #returns boolean vector showing wheter observation is an outlier
  left = a*quantile(vec)[2]
  right = a*quantile(vec)[4]
  cond <- vec <= right & vec >= left
  return(cond)
} #It can be used for recognizing or removing outliers

#I tried to create a function drawing lines that separate outliers, doesn't work.
#It was supposed to be added as a layer to ggplot (with +)
# outlier_lines <- function(vec, a = 1.5){
#   left = a*quantile(vec)[2]
#   right = a*quantile(vec)[4]
#   return(
#     geom_vline(xintercept = left, color = "tomato") +
#     geom_vline(xintercept = right, color = "tomato")
#   )
# }

#Histogram 1: all people. It's skewed right and unimodal and looks a little bit 
#like exponential drop from its peak at value around 25000. Some observations are 
#invisible, because thir values are too small compared to the highest ones.
show_statistics(income$Income)

#outliers lines
left = 1.5*quantile(income$Income)[2]
right = 1.5*quantile(income$Income)[4]

ggplot(income, aes(x = Income)) +
  geom_histogram(bins = 60,
                 color = "white",
                 fill = "blue",
                 alpha = 0.7) +
  theme_bw() +
  geom_vline(xintercept = left, color = "tomato") +
  geom_vline(xintercept = right, color = "tomato") +
  scale_x_continuous(labels = scales::number,
                     breaks = seq(-25000, 430000, by = 40000)) 
#maybe logarithmic scale would be better?

#Males and females separately:
income_f <- filter(income, Gender == "Female")
income_m <- filter(income, Gender == "Male")

show_statistics(income_f$Income)
show_statistics(income_m$Income)

#outliers lines
left_f = 1.5*quantile(income_f$Income)[2]
right_f = 1.5*quantile(income_f$Income)[4]
left_m = 1.5*quantile(income_m$Income)[2]
right_m = 1.5*quantile(income_m$Income)[4]

#Shape of the histograms is similar to shape of plot for both genders.
ggplot(income, aes(x = Income)) +
  geom_histogram(bins = 60,
                 color = "white",
                 fill = "blue",
                 alpha = 0.7) +
  theme_bw() +
  scale_x_continuous(labels = scales::number,
                     breaks = seq(-25000, 430000, by = 40000)) +
  facet_wrap(~Gender)
#maybe logarithmic scale would be better? + I don't know how to add outlier lines to facets