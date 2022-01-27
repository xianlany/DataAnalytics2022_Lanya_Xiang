library(readxl)
data <- read_excel("GitHub/DataAnalytics2022_Lanya_Xiang/2010EPI_data.xls", sheet = "EPI2010_onlyEPIcountries")
data[] <-lapply(data, function(x) type.convert(as.character(x)))
View(data)

#Exercise 1
summary(data$EPI) # stats
fivenum(data$EPI,na.rm=TRUE)
