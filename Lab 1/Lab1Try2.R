data <- read.csv("GitHub/DataAnalytics2022_Lanya_Xiang/2010EPI_data.csv", header = TRUE)
View(data)

#Exercise 1
summary(data$EPI) 	# stats
fivenum(data$EPI,na.rm=TRUE)
help(stem)
stem(data$EPI)		 # stem and leaf plot
help(hist)
hist(data$EPI)
hist(data$EPI, seq(30., 95., 1.0), prob=TRUE)
help(lines)
lines(density(data$EPI,na.rm=TRUE,bw=1.)) # or try bw=â???oSJâ???
help(rug)
rug(data$EPI)

plot(ecdf(data$EPI), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(data$EPI); qqline(data$EPI)

#Make a Q-Q plot against the generating distribution by: 
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for tdsn")
qqline(x)

#DALY

