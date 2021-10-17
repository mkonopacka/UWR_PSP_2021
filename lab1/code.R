#Load libraries-----
library("ggplot2")
library("data.table")
library("dplyr")

#Load data---------------------------
data <- fread(file = "grades.txt", 
              data.table = FALSE, 
              check.names = TRUE, 
              stringsAsFactors = TRUE,
              col.names = c("Id", "GPA", "IQ", "Gender", "TestPsych"))

data1 <- fread(file = "income.dat", 
               data.table = FALSE, 
               check.names = TRUE, 
               stringsAsFactors = TRUE,
               col.names = c("Id", "Age", "Education", "Gender", "Income", "Sector"))

#Check wheter data includes some NA values (no, any NAs) ----------------------
is_NA_dtf <- is.na.data.frame(data)
is_NA_sum <- colSums(is_NA_dtf)
is_NA_sum
is_NA_dtf1 <- is.na.data.frame(data1)
is_NA_sum1 <- colSums(is_NA_dtf1)
is_NA_sum1

#TASK 1a+b--------------------------------------------------------
summary(data)

#Histogram1: IQ-------------
qplot(data = data,
      x = IQ,
      main = "Count : IQ, depending on gender",
      xlab = "IQ",
      ylab = "Count",
      geom = "histogram",
      col = I("black"),
      fill = Gender,
      binwidth = 5
    )

#Histogram2: GPA------------
qplot(data = data,
      x = GPA,
      main = "Count: GPA",
      xlab = "GPA",
      ylab = "Count",
      geom = "histogram",
      col = I("black"),
      fill = Gender,
      binwidth = 0.5
)

#Histogram3: Psychological test-------------
qplot(data = data,
      x = TestPsych,
      main = "Count - Test: depending on gender",
      xlab = "IQ",
      ylab = "Count",
      geom = "histogram",
      col = I("black"),
      fill = Gender,
      binwidth = 2
)

#TASK 2-------------------------------------------------------------------------
#too few classes -> we can't see the actual shape of data
#too many classes -> unreadable dala
qplot(data = data,
      x = IQ,
      main = "Too few classes",
      xlab = "IQ",
      ylab = "Count",
      geom = "histogram",
      col = I("black"),
      fill = I("green"),
      bins = 2
)
qplot(data = data,
      x = IQ,
      main = "Too many classes",
      xlab = "IQ",
      ylab = "Count",
      geom = "histogram",
      col = I("black"),
      fill = I("green"),
      bins = 78
)

#TASK 3------------------------------------------------------------
data1$Sector <- factor(data1$Sector)
data1$Gender <- factor(data1$Gender, labels = c("M", "F"))
data1$Education <- factor(data1$Education)
summary(data1)

#another histogram 
#outliers - obvious from the plot, should be calculated with range (-1.5IQR, 1.5IQR) where IQR = inter quartile range
#we can get quartiles for example from summary()
#TODO define del_outliers function
ggplot(data = data1, aes(x = Income, fill = Gender))+
  geom_histogram(color = "black",
                 bins = 40)+
  scale_fill_manual(values = c("navy", "red"))+
  labs(title = "Histogram: income depending on gender",
       y = "Count")

#TODO change grid size