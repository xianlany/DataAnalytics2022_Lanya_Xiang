install.packages("ggplot")
install.packages("ggplot2")
install.packages("dplyr")

library("ggplot2")
library("dplyr")

data(diamonds)

View(diamonds)

ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds) + geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

#smaller <- diamonds %>% filter(carat < 3)
#ggplot(data = smaller, mapping = aes(x = carat)) + geom_histogram(binwidth = 0.1)

smaller <- diamonds %>% 
filter(carat < 3) 
ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

ggplot(data = smaller, mapping = aes(x =
                                       carat, color = cut)) + geom_freqpoly(binwidth
                                                                            = 0.1)
