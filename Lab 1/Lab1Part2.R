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




#Additional Examples

#Bar graphs and Visualization
# Chapter 3, Bar-Graphs, R Graphics cookbook.
# You have a data frame where one column represents the x position of each bar, and
# another column represents the vertical (y) height of each bar.
install.packages("gcookbook")
library(gcookbook) # for the dataset. pg_mean dataset is avialbe in this library.
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat = "identity")
BOD # there is not entry for Time == 6 ... did you see that ? 
# Time is numeric (continuous)
str(BOD)
ggplot(BOD, aes(x=Time, y=demand)) + geom_bar(stat = "identity")
# Convert Time to a discrete (categorical) variable with factor() function.
ggplot(BOD, aes(x=factor(Time), y=demand)) + geom_bar(stat = "identity")
# change the color of the bars and add an outline to the bars
# NOTE: In ggplot2, the default is to use the British spelling, colour, instead of
# the American spelling, color.
ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat = "identity", fill="lightblue", colour = "red")
ggplot(BOD, aes(x=factor(Time), y=demand)) +geom_bar(stat = "identity", fill="orange", colour = "red")
# Grouping Bars Together
# You want to group bars together by a second variable.
# In this example we'll use the cabbage_exp data set, which has two categorical variables,
# Cultivar and Date, and one continuous variable, Weight:

library(gcookbook) # For the data set
library(ggplot2)
cabbage_exp
# We'll map Date to the x position and map Cultivar to the fill color
ggplot(cabbage_exp, aes(x=Date, fill=Cultivar)) + geom_bar(position = "dodge")

library(gcookbook) # For the data set
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity")

# Making Bar Graph of Counts
ggplot(diamonds, aes(x=cut)) +geom_bar() # this is equvalent to using geom_bar(stat="bin)
# The diamonds data set has 53,940 rows, each of which represents information about one
data("diamonds")
diamonds
# In this example, the variable on the x-axis is discrete. If we use a continuous variable on
# the x-axis, we'll get a histogram
ggplot(diamonds,aes(x=carat)) + geom_bar()
# It turns out that in this case, the result is the same as if we had used geom_histogram() instead of geom_bar()
ggplot(diamonds, aes(x=carat)) + geom_histogram()


# Using Colors in Bar Graphs. Now we want to use different colors for the bars in our bar graph
# We can do this by using the "fill" asethetic. 
# We'll use the uspopchange data set for this example. It contains the percentage change
# in population for the US states from 2000 to 2010. We'll take the top 10 fastest-growing
# states and graph their percentage change. 
# We'll also color the bars by region (Northeast,South, North Central, or West)
# Taking Top 10 States 
library(gcookbook) # for the dataset
ups <- subset(uspopchange, rank(Change)>40)
ups
# Now we can make the graph, mapping Region to fill
ggplot(ups, aes(x=Abb, y= Change, fill=Region)) + geom_bar(stat = "identity")
# Do an Experiment with followings ... :=) 
ggplot(ups, aes(x=Abb, y=Change, fill=Region)) +geom_bin2d()
ggplot(ups, aes(x=Abb, y=Change, fill=Region)) + geom_col()
# The default colors aren't very appealing, so you may want to set them, using
# scale_fill_brewer() or scale_fill_manual().
ggplot(ups, aes(x=reorder(Abb,Change), y=Change, fill=Region)) + geom_bar(stat = "identity", colour= "red") +
  scale_fill_manual(values=c("#669933", "#FFCC66")) + xlab("US-States")
ggplot(ups, aes(x=reorder(Abb,Change), y=Change, fill=Region)) + geom_bar(stat = "identity", color = "purple") +
  scale_fill_manual(values=c("#224455","#DDCC33"))
# Coloring Negative and Positive Bars Differently
# You want to use different colors for negative and positive-valued bars.


library(gcookbook)
csub <- subset(climate, source="Berkeley" & Year >= 1900)
csub
csub$pos <- csub$Anomaly10y >=0
csub
ggplot(csub, aes(x=Year, y=Anomaly10y, fill= pos)) + geom_bar(stat = "identity", position = "identity")
# changing the color with scale_fill_manual
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) + geom_bar(stat="identity", colour="black", size=0.25) +
  scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE)
# Adjusting Bar Width and Spacing
# You want to adjust the width of bars and the spacing between them.
# To make the bars narrower or wider, set width in geom_bar(). The default value is 0.9; 
# larger values make the bars wider, and smaller values make the bars narrower
library(gcookbook) # for the datset
ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat="identity")
# Narrow Bars
ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat="identity", width = 0.5)
# Wider bars, maximum width = 1
ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat = "identity", width = 0.95)
# Different bar widths
ggplot(cabbage_exp, aes(x=Date, y= Weight, fill=Cultivar)) + geom_bar(stat = "identity", width = 0.5, position = "dodge")
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat = "identity", width = 0.5, position = position_dodge(0.7))
# Making a Sketched Bar Graph


library(gcookbook) # for the dataset
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat = "identity")
cabbage_exp
ggplot(cabbage_exp, aes(x= Date, y= Weight, fill=Cultivar)) + geom_bar(stat = "identity") + guides(fill=guide_legend(reverse = TRUE))
# Adding Lables to your Graphs
library(gcookbook) # For the data set
ggplot(cabbage_exp, aes(x=interaction(Date,Cultivar), y=Weight)) +geom_bar(stat = "identity") + geom_text(aes(label=Weight),vjust=1.5,colour="white")
library(ggplot2)
# Adjust y limits to be a little higher
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=-0.2) +
  ylim(0, max(cabbage_exp$Weight) * 1.05)

# Map y positions slightly above bar top - y range of plot will auto-adjust
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(y=Weight+0.1, label=Weight))

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=Weight), vjust=1.5, colour="white",
            position=position_dodge(.9), size=3)



# make a Cleveland dot plot
#The simplest way to create a dot plot is to use geom_point() function
library(gcookbook) # For the data set
tophit <- tophitters2001[1:25,] # take top 25 top hitters
tophit
ggplot(tophit, aes(x=avg, y=name)) + geom_point()
tophit[,c("name","lg","avg")]
ggplot(tophit, aes(x=avg, y= reorder(name,avg))) + geom_point(size=3, colour="red") + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour ="grey60",linetype="dashed"))

ggplot(tophit, aes(x=avg, y=reorder(name,avg))) + geom_point(size=2.5, colour="blue") + 
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey60", linetype = twodash))
# Get the names sorted by lg, then by avg
nameorder <- tophit$name[order(tophit$lg, tophit$avg)]
# Turn the name into factor, with levels in the order of nameorder
tophit$name <- factor(tophit$name, levels = nameorder)
ggplot(tophit, aes(x=avg, y=name)) +
  geom_segment(aes(yend=name), xend=0, colour="grey70")+
  geom_point(size=3, aes(colour=lg)) +
  scale_color_brewer(palette="Set1", limits=c("NL","AL")) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = c(1,0.55),
        legend.justification = c(1,0.5))

ggplot(tophit, aes(x=avg, y=name)) +
  geom_segment(aes(yend=name), xend=0, colour="grey40") +
  geom_point(size=3, aes(colour=lg)) +
  scale_color_brewer(palette="Set1", limits=c("NL","AL"), guide=FALSE) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(lg ~ ., scales = "free_y", space="free_y")


