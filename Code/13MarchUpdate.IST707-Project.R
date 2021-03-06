#Loading required packages
library(readxl)
library(sqldf)
library(ggplot2)
library(tidyverse)
library(countrycode)
library(corrplot)
##library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(e1071)
library(dplyr)

r2_score <- function(y_true, y_pred){
  ssres <- sum((y_pred - y_true)^2)
  sstot <- sum((y_true - mean(y_true))^2)
  return (1 - ssres/sstot)
}

#### Step 1: Load the Data for all years for Happiness data 

setwd("C:\\Drive D\\WD\\happiness")
##setwd("~/Desktop/Spring '20")

happ2015 <- read.csv("2015.csv", stringsAsFactors = F)
happ2016 <- read.csv("2016.csv", stringsAsFactors = F)
happ2017 <- read.csv("2017.csv", stringsAsFactors = F)
happ2018 <- read.csv("2018.csv", stringsAsFactors = F, na.strings = 'N/A')
happ2019 <- read.csv("2019.csv", stringsAsFactors = F)


##### renaming all data to simple columns names
### removing error from happ 2015
happ2015<-happ2015[,-5]
colnames(happ2015)<-c("country","region","rank","score","GDP","family","life","freedom",
                      "corruption","generosity","dystopia")

happ2015<-happ2015[,-2]
happ2015<-happ2015[,-10]

#### Removing confidence intervals from 2016 data 
happ2016<-happ2016[,-5:-6]
colnames(happ2016)<-c("country","region","rank","score","GDP","family","life","freedom",
                      "corruption","generosity","dystopia")

happ2016<-happ2016[,-2]
happ2016<-happ2016[,-10]

##### removing whisker hign and low from 2017 data 
happ2017<-happ2017[,-4:-5]
colnames(happ2017)<-c("country","rank","score","GDP","family","life","freedom","generosity",
                      "corruption","dystopia")
happ2017 <- happ2017 %>% select(-dystopia)
### creating new column region for 2017 data since region is not present here 
##happ2017$region=''


##### renaming columns for 2018
colnames(happ2018)<-c("rank","country","score","GDP","family","life","freedom","generosity","corruption")

##### renaming columns for 2019
colnames(happ2019)<-c("rank","country","score","GDP","family","life","freedom","generosity","corruption")



happ <- happ2015 %>% mutate(year = 2015) %>% 
  bind_rows(happ2016 %>% mutate(year = 2016), 
            happ2017 %>% mutate(year = 2017),
            happ2018 %>% mutate(year = 2018),
            happ2019 %>% mutate(year = 2019))

happ <- happ %>% mutate(continent = countrycode(sourcevar = country,
                             origin = "country.name",
                             destination = "continent"))


happ %>% count(year)

########### Top 10 rank by countries ##########
happ %>% filter(rank <= 10) %>% ggplot(aes(x=year, y=rank, color=country)) + geom_line(size=1) +
  geom_point() + scale_y_continuous(breaks = 1:11)

#### Bottom 10 Rank by countries over years ####
happ %>% filter(rank >= 146) %>% ggplot(aes(x=year, y=rank, color=country)) + geom_line(size=1) +
  geom_point() + scale_y_continuous(breaks = 1:11)

happ$year<-as.factor(happ$year)
happ$rank<-as.factor(happ$rank)


happ %>% drop_na() %>% ggplot(aes(x=GDP, y=score, color=continent)) + geom_point() +
  ylab('Happiness Score')

happ %>% drop_na() %>% group_by(continent) %>% summarise(score = mean(score)) %>%
  ggplot(aes(x=reorder(continent, score), y= score)) + geom_col(fill='salmon', alpha=0.6) + xlab('Continent') + 
  ylab('Happiness Score')

happ %>% ggplot(aes(x=score)) + geom_histogram(fill='skyblue', alpha=0.6, bins=30)


happ %>% drop_na() %>% select_if(is.numeric) %>% cor() %>% corrplot(type='upper')

final <- happ %>% select(-rank, -country)

final %>% is.na() %>% colMeans()

final <- final %>% mutate(corruption = replace_na(corruption, mean(corruption, na.rm = T)))
final <- final %>% mutate(continent = replace_na(continent,'Europe'))
#The NA continent is Kosovo (still have to look up why this was NA but replacing the NA with Europe to make the model work)
final <- final %>% mutate(continent = factor(continent))

###linear regression###

#mylm <- lm(score ~ ., data=final)
#summary(mylm)

#set.seed(123)
#train_index <- createFolds(final$score, k = 5, returnTrain = T)
#The continent hides one by default. Africa is the default for the regression equation (chooses alphebetically). All other coefficients are zero.
plot(mylm) #hit Return in console. You want random scatter and this looks good. You don't want the megaphone pattern.
#1 Good
#2 Ok. Tail on the left violates the assumptions that the residuals are not normally distributed.
#Run model without some variables and see if this works better
#Try step() function
step(mylm)
#Eliminated values based on the p values then used the step() function-> this uses the AIC metric. Results in full model were same.

#m_r2 <- c()
#svm_r2 <- c()
#rf_r2 <- c()

#for (indx in train_index){
#  train <- final %>% slice(indx)
#  valid <- final %>% slice(-indx)
  
# model <- lm(score ~ ., data=train)
# y_pred <- predict(model, newdata = valid)
#  r2 <- r2_score(valid$score, y_pred)
#  lm_r2 <- c(lm_r2, r2)


###Partition Data###
index <- createDataPartition(final$score, p=.7, list = FALSE)
train <- final[index,]
test <- final[-index,]

library(sqldf)
train<-train[,-8]
test<-test[,-8]

###Linear Model###

mylm <- lm(score ~ GDP + family + life + freedom + corruption + generosity + continent, data=train)
summary(mylm)
step(mylm)

linear.prediction <- predict(mylm, newdata = test)
linear.rmse <- RMSE(linear.prediction, test$score)
#.495 
#Pick the model with the lowest RMSE

###randomForest###
model <- randomForest(score ~ ., data=train, ntree=50)
y_pred <- predict(model, newdata = test)
forest.rmse<- RMSE(y_pred, test$score)
#.401

#random forest has many decision trees
#Default= number of predictors / 3 

#create vector to store rmse values
min.rmse <- rep(NA, 7)  #Creates a vector of lenght 7 going to store rmse values inside

for (i in 2:7){ #8 because of 8 predictors
  rf.model <- randomForest(score ~ ., data=train, mtry=i) #Everytime it runs through the split it will try 2,3,4...
  rf.predict <- predict(rf.model, newdata = test)
  min.rmse[i-1] <- RMSE(rf.predict, test$score)  #[i-1] because the loop started at 2. i=2 so that vector has to start at 1.
}

#Default was 500 trees and I did 50. Now we did 500 trees and tried a different number of variables for each split.

which.min(min.rmse)
min.rmse[1]
#0.450425
#4th index corresponds to i=5.

###SVM###
#try other kernels
#try other values of gamma
#gridsearch() try this one.
#caret package and look at training() function.
#trcontrol (cross validation)
#tune.grid 

svm.pred <- predict(svm.model, test)
svm.rmse <- RMSE(svm.pred, test$score)
#.50

gamm <- seq(.5,1,.1) ##limit gamma values because algorithm won't converge
counter <- 0
svm.min.rmse <- rep(NA, 10) #Initializing the vector

for (i in gamm){
  counter=counter+1
  svm.model <- svm(score ~ ., data=train, cost = 100, kernel = "polynomial", gamma= i, type= "eps-regression") #continuous variable
  svm.predict <- predict(svm.model, newdata = test)
  svm.min.rmse[counter] <- RMSE(svm.predict, test$score)  
}

which.min(svm.min.rmse)
svm.min.rmse[3]
#0.4762931


###rpart###
#Decision trees are one of the oldest and simplest machine learning algorithms.
#Classification is predicting a class.
#Regression tree is used when the predicted outcome can be considered a real number.

rpart.model <- rpart(score~., data = train)
rpart.pred <- predict(rpart.model, valid[,-1])

######## Naive bayes #####

##compute svm confusion matrix
table(pred= svm.pred, true= valid[,1])
table(pred = rpart.pred, true = valid[,1])
##compute rpart confusion matrix

#Create table with linear, random, svm, rpart, RMSE(New column)
#caret package, train control option and tuning option.


################# R Part ####################

library(rpart)


colnames(train) <- make.names(colnames(train))
colnames(test) <- make.names(colnames(test))

str(train)

train_model <- rpart(score ~ GDP+family+life+freedom+corruption+corruption+generosity
                     , data = train, method = "anova")
rpart.plot(train_model,type=3,digits=3,fallen.leaves = T)

predict_model <- predict(train_model, test)

happiness2=select(train,continent,GDP,family,life,freedom)
oceania=which(happiness2$continent=='Oceania')
happiness2=happiness2[-oce,]
class=rpart(continent~.,data=happiness2,method='class')
rpart.plot(class,type=3,digits=3,fallen.leaves=T)

############### Naive Bayes ###################

happall<-happall[-1,]

happall<-sqldf("select continent,country,avg(score) as score, avg(GDP) as GDP,avg(family) as family,
                avg(life) as life, avg(freedom) as freedom, avg(corruption) as corruption,
                avg(generosity) as generosity from happ
                group by continent,country
                ")

happall<-sqldf("select *,score, CASE WHEN score<3 then 'Less than 3'
                               WHEN score>=3 and score<=4 then '3 to 4'
                               WHEN score>4 and score<=5 then '4 to 5'
                               WHEN score>5 and score<=6 then '5 to 6'
                               WHEN score>6 and score<=7 then '6 to 7'
                               WHEN score>7 and score<=8 then '7 to 8'
                          ELSE 'Above 7' END as score_bracket from happall
               ")

smp_size <- floor(0.75 * nrow(happall))

set.seed(123)
train_ind <- sample(seq_len(nrow(happall)), size = smp_size)

train <- happall[train_ind, ]
test <- happall[-train_ind, ]

library(naivebayes)

train$score_bracket<-as.factor(train$score_bracket)
test$score_bracket<-as.factor(test$score_bracket)

str(train)

model.naiveBayes <- naiveBayes(train$score_bracket ~ GDP+family+life+freedom+corruption+
                                                     generosity, data = train)

summary(model.naiveBayes)

prediction.naiveBayes <- predict(model.naiveBayes, newdata = test, type = "class")

table(`Actual Class` = test$score_bracket, `Predicted Class` = prediction.naiveBayes)

error.rate.naive <- sum(test$score_bracket != prediction.naiveBayes)/nrow(test)
print(paste0("Accuary (Precision): ", (1 - error.rate.naive)*100))

######################### Network ##############

# Libraries
library(igraph)
library(networkD3)

# create a dataset:

# Plot
p <- simpleNetwork(happ, height="100px", width="100px")

forceNetwork(Links = happ, Nodes = happall, Source = "country",
             Target = "country", Value = "GDP", NodeID = "score",
             Group = "continent", opacity = 0.4)

print(p,file="C:/Drive D/WD/happiness/network.html")


library(ggplot2)
library(dplyr)
library(plotly)
library(viridis)
library(hrbrthemes)
library(googleVis)

happ2019<- sqldf("select * from happ where year=2019")

Bubble <- gvisBubbleChart(happ2019, idvar="country", 
                          xvar="GDP", yvar="life",
                          colorvar="continent", sizevar="score",
                          options=list(
                            hAxis='{minValue:0, maxValue:1}'))

Bubble <- gvisBubbleChart(happ2019, idvar="country", 
                          xvar="GDP", yvar="life",
                          colorvar="continent", sizevar="score",
                          options=list(width = "automatic",
                                       height = "700"))

Bubble <- gvisBubbleChart(happ2019, idvar="country", 
                          xvar="GDP", yvar="life",
                          colorvar="continent", sizevar="score",
                          options=list(width = "automatic",
                                       height = "700"))


print(Bubble,file="C:/Drive D/WD/happiness/network.html")

plot_ly(happ2019, x = happ2019$GDP, y = happ2019$life, 
        text = paste("Country: ", happ2019$country),
        mode = "markers", color = happ2019$continent, size = happ2019$score)


######## Geo ############
Geo=gvisGeoChart(happ, locationvar="country", 
                 colorvar="GDP",
                 options=list(projection="kavrayskiy-vii"))
plot(Geo)

Geo=gvisGeoChart(happ, locationvar="country", 
                 colorvar="GDP",
                options=list(projection="kavrayskiy-vii",height="600",width="automatic",
                colorAxis="{colors:['red', 'orange', '#33A532']}",
                hAxis='{minValue:0, maxValue:2}'))

print(Geo,file="C:/Drive D/WD/happiness/Geo.html")

############# Motion chart #####
suppressPackageStartupMessages(library(googleVis))

options(gvis.plot.tag='chart')


happ$year<-as.numeric(happ$year)

motion_plot <- gvisMotionChart(happ,idvar="country",timevar = "year",xvar = "GDP",yvar = "life")
plot(motion_plot)

print(motion_plot,file="C:/Drive D/WD/happiness/motion_plot.html")

######### D3 heat map ############


happall_asia<-sqldf("select GDP,life,freedom,corruption,generosity,family from happall where continent='Asia' order by GDP desc ")
rownames_asia<-sqldf("select country from happall where continent='Asia' order by GDP desc")
row.names(happall_asia)<-rownames_asia[,1]

d3heatmap(happ2019_asia, scale="column", colors="Oranges")

happall_africa<-sqldf("select GDP,life,freedom,corruption,generosity,family from happall where continent='Africa' order by GDP desc ")
rownames_africa<-sqldf("select country from happall where continent='Africa' order by GDP desc")
row.names(happall_africa)<-rownames_africa[,1]

d3heatmap(happall_africa, scale="column", colors="Reds")

happall_europe<-sqldf("select GDP,life,freedom,corruption,generosity,family from happall where continent='Europe' order by GDP desc ")
rownames_europe<-sqldf("select country from happall where continent='Europe' order by GDP desc")
row.names(happall_europe)<-rownames_europe[,1]

d3heatmap(happall_europe, scale="column", colors="Greens")

happall_americas<-sqldf("select GDP,life,freedom,corruption,generosity,family from happall where continent='Americas' order by GDP desc ")
rownames_americas<-sqldf("select country from happall where continent='Americas' order by GDP desc")
row.names(happall_americas)<-rownames_americas[,1]

d3heatmap(happall_americas, scale="column", colors="Blues")

happall_oceania<-sqldf("select GDP,life,freedom,corruption,generosity,family from happall where continent='Oceania' order by GDP desc ")
rownames_oceania<-sqldf("select country from happall where continent='Oceania' order by GDP desc")
row.names(happall_oceania)<-rownames_oceania[,1]

d3heatmap(happall_oceania, scale="column", colors="Blues")





##Extra

plot(svm.pred, data = train)
pred <- predict(svm_model)

plot(svm.model, data = train,
     score.,
     slice = list(score))













#EXTRA 
#Creating random data set in R
set.seed(1)
x=matrix(rnorm(100*3), ncol = 3)
y=c(rep(-1,30), rep(1,70))
x[y==1,]= x[y==1,] +1
dat=data.fram(x=x, y=as.factor(y))

dtm <- rpart(score~., train, method="class")
rpart.plot(dtm, type=4, extra =101)
p <- predict(dtm, test[,5], p)
table(test[-5], p)

