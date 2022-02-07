install.packages("dplyr")
install.packages("ggplot2")

library("ggplot2")
library("dplyr")

data <- read.csv("GitHub/DataAnalytics2022_Lanya_Xiang/2010EPI_data.csv", header = TRUE)
View(data)

#Exercise 1
plot(ecdf(data$EPI), do.points=FALSE, verticals=TRUE) 

par(pty="s") 
qqnorm(data$EPI); qqline(data$EPI)

x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)


#Exercise 1 code
plot(ecdf(data$EPI),do.points=FALSE,verticals = TRUE) 
plot(ecdf(data$EPI),do.points=TRUE,verticals = TRUE) # points are visible on the plot.
par(pty="s")
help("qqnorm") # read the RStudio documentation for qqnorm
help("qqplot") # read the RStudio documentation for qqplot
qqnorm(data$EPI)
qqline(data$EPI) # adding the line on the Q-Q plot
x <- seq(30,95,1)
x
x2 <-seq(30,95,2)
x2
x2 <-seq(30,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)

#comparing distributions
boxplot(data$EPI,data$DALY)



#multivariate, linear basis and least-squares constraints
multivariate <- read.csv("C:/Users/Lanya/Documents/GitHub/DataAnalytics2022_Lanya_Xiang/Lab 1/multivariate.csv")
attach(multivariate)
mm<-lm(Homeowners~Immigrant)
mm
summary(mm)$coef # The output above shows the estimate of the regression beta coefficients (column Estimate) and 
# their significance levels (column Pr(>|t|).
# The intercept is 107494.898 and the coefficient of Immigrant variable is -6656.839.
# The estimated regression equation can be written as follow:
# Homeowners = 107494.898 + (-6656.839)*Immigrant 
# We can rewrite it as: 
# Homeowners = 107494.898 - 6656.839*Immigrant.

plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm,col=2,lwd=3)
# Using this formula, for each new value in Immigrant, you can predict the value for Homeowners.
# As an example:
# For Immigrant value = 0, we will get: Homeowners = 107494.898 - 6656.839*0 = 107494.898
# for Immigrant value = 20, we will get: Homeowners = 107494.898 - 6656.839*20 = -25641.88
# Predictions can be easily made using the R function predict().

# In the following example, we predict Homeowners for two Immigrant values: 0 and 20.
# you can pass the 0 and 20 values as a concatenated list for Immigrants as follows:
newImmigrantdata <- data.frame(Immigrant = c(0,  20))
mm %>% predict(newImmigrantdata)

abline(mm)
abline(mm,col=3,lwd=3) # line color = green, line width = 3
attributes(mm)
mm$coefficients

#creating plots
plot(mtcars$wt,mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data = mtcars)
ggplot(mtcars,aes(x=wt,y=mpg)) + geom_point()
plot(pressure$temperature, pressure$pressure, type = "l")
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature, pressure$pressure/2, col="red")
points(pressure$temperature, pressure$pressure/2, col="blue")
library(ggplot2)
qplot(pressure$temperature, pressure$pressure, geom="line")
qplot(temperature, pressure, data = pressure, geom = "line")
ggplot(pressure, aes(x=temperature,y=pressure)) + geom_line() + geom_point()
ggplot(pressure, aes(x=temperature,y=pressure)) + geom_line() + geom_point()


#Creating bar graphs
barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))#generate a table of counts
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl)) #treat cyl as discrete

#bar graph of counts
qplot(factor(cyl), data = mtcars)
ggplot(mtcars, aes(x=factor(cyl)))+geom_bar()


#creating histograms using ggplot
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10) #specify approximate number of bins with breaks
hist(mtcars$mpg, breaks = 5)
hist(mtcars$mpg, breaks = 12)
qplot(mpg, data=mtcars, binwidth=4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth = 4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth = 5)


#creating box plots using ggplot
plot(ToothGrowth$supp, ToothGrowth$len) #using plot() function and pass it as a factor of x values and a vector of y values
#formula syntax
boxplot(len ~ supp, data = ToothGrowth) #if the two vectors are in the same dataframe, you can use the formula syntax
#with this syntax you can combine two variables on the x axis
#put interaction of two variables on the x axis
boxplot(len~supp + dose, data = ToothGrowth)
#with ggplot you can get the same results above
library(ggplot2)
qplot(ToothGrowth$supp, ToothGrowth$len, geom = "boxplot")
#if the two vectors are in the same dataframe, you can use the following syntax
qplot(supp, len, data=ToothGrowth, geom= "boxplot")
#in ggplot2, the above is equivalent to:
ggplto(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()
#using 3 separate vectors
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom= "boxplot")
#you can write the same thing above, get the columns from the dataframe
qplot(interaction(supp, dose), len, data = ToothGrowth, geom = "boxplot")
#using ggplot you can do the same thing and it is:
ggplot(ToothGrowth, aes(x=interaction(supp, dose), y=len)) + geom_boxplot()
#plotting a function curve