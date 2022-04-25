install.packages("randomForest")

library(randomForest)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rpart)
library(rpart.plot)
library(class)
library(cluster)
library(car)

crops <- read.csv("crops.csv", header = TRUE)
corg <- read.csv("cropsorganized.csv", header= TRUE)
hunger <- read.csv("hunger.csv", header = TRUE)


#EDA on correlation coefficients using hunger variable and crop yield

corg <- na.omit(corg)
corg %>% drop_na(corg$Area.Harvested)

beans <- corg %>% filter(grepl('Beans, green', Item))
maize <- corg %>% filter(grepl('Maize', Item))
cassava <- corg %>% filter(grepl('Cassava', Item))
yam <- corg %>% filter(grepl('Yams', Item))
sweetpotato <- corg %>% filter(grepl('Sweet potatoes', Item))
rice <- corg %>% filter(grepl('Rice, paddy', Item))

undernourishment <- hunger
undernourishment <- undernourishment[c(3, 4, 9, 12)]
undernourishment <- na.omit(undernourishment)
undernourishment$Year <- as.integer(substr(undernourishment$Year.Code, 5, nchar(undernourishment$Year.Code)))
undernourishment$Type <- cut(undernourishment$Value, br = c(0, 10, 30, 50, 100), labels = c("Extremely Low", "Low", "Medium", "High"))
head(undernourishment)
str(undernourishment)

boxplot(undernourishment$Value)

maize_combined <- left_join(maize, undernourishment,  by = c("Area", "Year"))
maize_combined <- na.omit(maize_combined)

summary(maize_combined)

hist(maize_combined$Area.Code..FAO., breaks = 300)

nrow(beans)
nrow(maize)
nrow(cassava)
nrow(yam)
nrow(sweetpotato)
nrow(rice)
nrow(undernourishment)

View(crops)
View(corg)
View(hunger)
View(undernourishment)
View(maize_combined)

View(beans)
View(maize)
View(cassava)
View(yam)
View(sweetpotato)
View(rice)


ggplot(maize, aes(fill=Item, y=Area.Harvested, x=Area)) +
  geom_bar(position='dodge', stat='identity') +
  ggtitle('Maize Harvest in Africa') +
  xlab('Countries') +
  ylab('Area Harvested (in ha)') +
  scale_fill_manual('Crop', values=c('blue')) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


  
#multivariate regression

maize_mr <- lm(Value ~ Area.Harvested + Area.Code..FAO. + Year, data = maize_combined)

summary(maize_mr)
avPlots(maize_mr)

cor.test(maize_combined$Value, maize_combined$Area.Harvested)
cor.test(maize_combined$Value, maize_combined$Area.Code..FAO.)
cor.test(maize_combined$Value, maize_combined$Year)


#cook's distance

model = lm(Value ~ Area.Harvested + Area.Code..FAO. + Year, data = maize_combined)
plot(model, pch=18, col="red", which=c(4))

cooks.distance(model)
cd <- cooks.distance(model)
sort(round(cd, 10))

influential <- cd[(cd > (3 * mean(cd, na.rm=TRUE)))]
influential

names <- names(influential)
names

outliers <- maize_combined[names,]
outliers

nrow(outliers)

maize_combined_cleaned <- maize_combined %>% anti_join(outliers)



#multivar with no outlier data

maize_mr_cleaned <- lm(Value ~ Area.Harvested + Area.Code..FAO. + Year, data = maize_combined_cleaned)

summary(maize_mr_cleaned)

avPlots(maize_mr_cleaned)

cor.test(maize_combined_cleaned$Value, maize_combined_cleaned$Area.Harvested)
cor.test(maize_combined_cleaned$Value, maize_combined_cleaned$Area.Code..FAO.)
cor.test(maize_combined_cleaned$Value, maize_combined_cleaned$Year)


#mean squared analysis

set.seed(1)
sample1 <- sample(nrow(maize_combined), 0.33*nrow(maize_combined))
sample2 <- sample(nrow(maize_combined_cleaned), 0.33*nrow(maize_combined_cleaned))

original_model <- lm(Value ~ Area.Harvested + Area.Code..FAO. + Year, data = maize_combined, subset = sample1)
outlier_removed_model <- lm(Value ~ Area.Harvested + Area.Code..FAO. + Year, data = maize_combined_cleaned, subset = sample2)

mean((maize_combined$Value-predict(original_model, maize_combined))[-sample1]^2)
mean((maize_combined_cleaned$Value-predict(outlier_removed_model, maize_combined_cleaned))[-sample2]^2)




#random forest

#maize
maize_model <- maize_combined[c(5, 6, 7, 10)]
maize_model <- na.omit(maize_model)
head(maize_model)

sample_maize <- sample(1:nrow(maize_model),0.7 * nrow(maize_model))

maizetrain = maize_model[sample_maize,]
maizetest = maize_model[-sample_maize,]

#create tree
maize_rf <- randomForest(Type ~ Area.Harvested + Area.Code..FAO. + Year, data = maizetrain, type = "classification")
maize_rf

maize_rf2 <- randomForest(Type ~ Area.Harvested + Area.Code..FAO. + Year, data = maizetrain, mtree= 1000, ntry = 3, type = "classification")
maize_rf2


rf_pred <- predict(maize_rf, maizetrain, type='class')
table(rf_pred, maizetrain$Type)

rf_pred2 <- predict(maize_rf2, maizetrain, type='class')
table(rf_pred2, maizetrain$Type)


#decision tree
maize_dt <- rpart(Type ~ Area.Harvested + Area.Code..FAO. + Year, data = maizetrain)
rpart.plot(maize_dt)

pred_maizedt <- predict(maize_dt, maizetest, type = 'class')
table(maizetest$Type, pred_maizedt)


#k-nn classification

#maize
str(maize_model)
#maize_model$ï..Area.Code..FAO. <- as.factor(maize$ï..Area.Code..FAO.)
#summary(maize_model$ï..Area.Code..FAO.)

str(maize)

knn_maize_df <- maize_combined[c(5, 6, 9, 10)]
knn_maize_df <- na.omit(knn_maize_df)

View(knn_maize_df)

#knn_maize_df$ï..Area.Code..FAO. <- as.factor(knn_maize_df$ï..Area.Code..FAO.)
#summary(knn_maize_df$ï..Area.Code..FAO.)

nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Run normalization on first 4 columns of dataset because they are the predictors
knn_maize_df[1:3] <- as.data.frame(lapply(knn_maize_df[c(1,2,3)], nor))
View(knn_maize_df)

sample_maize_knn <- sample(1:nrow(knn_maize_df),0.7 * nrow(knn_maize_df))

knntrain = knn_maize_df[sample_maize_knn,]
knntest = knn_maize_df[-sample_maize_knn,]

nrow(knntrain)
sqrt(511) #=22.6 ~ 23

KNNpred <- knn(knntrain[1:3], knntest[1:3], cl = knntrain$Type, k = 23)

table(KNNpred,knntest$Type)
summary(KNNpred)



#k-means clustering 


summary(maize)

#help("sapply")
set.seed(300)
k.max <- 12

# tot.withinss = Total within-cluster sum of square
# iter.max = the maximum number of iterations allowed
# nstart = if centers is a number, how many random sets should be chosen.
wss <- sapply(1:k.max,function(k){kmeans(maize_combined[c(1,5,6,9)],k,nstart = 100,iter.max = 20)$tot.withinss})
wss # within sum of squares.
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares") #elbow method

icluster <- kmeans(maize_combined[(c(1,5,6,9))],4,nstart = 100)
table(icluster$cluster,maize_combined$ï..Area.Code..FAO.)

#c3 low, c1 medium, c2 high (try to visualize these, ggplot kmeans)
maize_combined$cluster <- as.character(icluster$cluster)

ggplot() +
  geom_point(data = maize_combined, 
             mapping = aes(x = Year, 
                           y = Area.Harvested, 
                           colour = cluster)) +
  geom_point(mapping = aes_string(x = icluster$centers[, "Value"], 
                                  y = icluster$centers[, "Area.Harvested"]),
             color = "lightgray", size = 6) +
  geom_text(mapping = aes_string(x = icluster$centers[, "Value"], 
                                 y = icluster$centers[, "Area.Harvested"],
                                 label = 1:4),
            color = "black", size = 4) +
  theme_light()

ggplot(maize_combined, aes(x = Value, y = Area, col = cluster)) + geom_point()

ggplot(maize_combined, aes(x = Value, y = Area.Harvested, col = cluster)) + geom_point() + stat_ellipse()

dis <- dist(maize_combined[(c(1,5,6,9))])^2
sil <- silhouette (icluster$cluster, dis)
windows() 
plot(sil)



