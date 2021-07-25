
# package required
if(!require(openxlsx)) install.packages("openxlsx", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

library(openxlsx)
library(tidyverse)
library(caret)
library(randomForest)

# import .xlsx file  **Note: required openxlsx package**
energydat <- read.xlsx("https://archive.ics.uci.edu/ml/machine-learning-databases/00242/ENB2012_data.xlsx", sheet=1)

# display number of rows and columns
dim(energydat)

#check structure of the data
str(energydat)

#display first 6 rows of the data
head(energydat)

#assign appropriate column names
colnames(energydat) <- c("relative_compactness","surface_area","wall_area","roof_area","overall_height",
                 "orientation","glazing_area", "glazing_area_distribution", "heating_load", "cooling_load")

#display first 6 rows of the updated data table
energydat %>% head() %>% knitr::kable(caption = "First 6 rows of the dataset, energydat")

#checking for missing values
colSums(is.na(energydat))

# alternate check for missing values
any(is.na(energydat))

#summary stats of data
summary(energydat)

# number of variables and data entry
dim(energydat)

#**relative_compactness **
#compute table of average heating and cooling load for relative compactness
Compact_avg <- energydat %>% group_by(relative_compactness) %>% 
  summarize(count =n(), Heat_avg = mean(heating_load), cool_avg = mean(cooling_load)) 
Compact_avg %>% knitr::kable(caption = "Count & average heating and cooling load for relative compactness")

#plot average heating and cooling load vs relative compactness
Compact_avg %>% 
  gather(Load_type, average_load, -c(relative_compactness,count)) %>% 
  ggplot(aes(relative_compactness, average_load, color = Load_type)) + 
  geom_point()+ 
  ggtitle("Average heating and cooling load for relative compactness")
  

#**surface_area **
#compute table of average heating and cooling load for relative compactness
Surface_Area_avg <- energydat %>% group_by(surface_area) %>% 
  summarize(count =n(), Heat_avg = mean(heating_load), cool_avg = mean(cooling_load)) 
Surface_Area_avg

#plot average heating and cooling load vs surface area
Surface_Area_avg %>% 
  gather(Load_type, average_load, -c(surface_area,count)) %>% 
  ggplot(aes(surface_area, average_load, color = Load_type)) + 
  geom_point() + 
  ggtitle("Average heating and cooling load for surface area")

#**wall_area **
#compute table of average heating and cooling load for relative compactness
wall_area_avg <- energydat %>% group_by(wall_area) %>% 
  summarize(count =n(), Heat_avg = mean(heating_load), cool_avg = mean(cooling_load)) 
wall_area_avg

#plot average heating and cooling load vs wall area
wall_area_avg %>% 
  gather(Load_type, average_load, -c(wall_area,count)) %>% 
  ggplot(aes(wall_area, average_load, color = Load_type)) + 
  geom_point() + 
  ggtitle("Average heating and cooling load for wall area")

#**roof_area **
#compute table of average heating and cooling load for roof area
roof_area_avg <- energydat %>% group_by(roof_area) %>% 
  summarize(count =n(), Heat_avg = mean(heating_load), cool_avg = mean(cooling_load)) 
roof_area_avg

#plot average heating and cooling load vs roof area
roof_area_avg %>% 
  gather(Load_type, average_load, -c(roof_area,count)) %>% 
  ggplot(aes(roof_area, average_load, color = Load_type)) + 
  geom_point() + 
  ggtitle("Average heating and cooling load for roof area")

#**overall_height **
#compute table of average heating and cooling load for overall height
overall_height_avg <- energydat %>% group_by(overall_height) %>% 
  summarize(count =n(), Heat_avg = mean(heating_load), cool_avg = mean(cooling_load)) 
overall_height_avg

#plot average heating and cooling load vs overall height
overall_height_avg %>% 
  gather(Load_type, average_load, -c(overall_height,count)) %>% 
  ggplot(aes(overall_height, average_load, color = Load_type)) + 
  geom_point() + 
  ggtitle("Average heating and cooling load for overall height")

#**orientation_avg **
#compute table of average heating and cooling load for orientation
orientation_avg <- energydat %>% group_by(orientation) %>% 
  summarize(count =n(), Heat_avg = mean(heating_load), cool_avg = mean(cooling_load)) 
orientation_avg

#plot average heating and cooling load vs orientation
orientation_avg %>% 
  gather(Load_type, average_load, -c(orientation,count)) %>% 
  ggplot(aes(orientation, average_load, color = Load_type)) + 
  geom_point() + 
  ggtitle("Average heating and cooling load for orientation")

#**glazing_area **
#compute table of average heating and cooling load for glazing area
glazing_area_avg <- energydat %>% group_by(glazing_area) %>% 
  summarize(count =n(), Heat_avg = mean(heating_load), cool_avg = mean(cooling_load)) 
glazing_area_avg

#plot average heating and cooling load vs glazing area
glazing_area_avg %>% 
  gather(Load_type, average_load, -c(glazing_area,count)) %>% 
  ggplot(aes(glazing_area, average_load, color = Load_type)) + 
  geom_point() + 
  ggtitle("Average heating and cooling load for glazing area")

#**glazing_area_distribution **
#compute table of average heating and cooling load for glazing area distribution
glazing_area_dist_avg <- energydat %>% group_by(glazing_area_distribution) %>% 
  summarize(count =n(), Heat_avg = mean(heating_load), cool_avg = mean(cooling_load)) 
glazing_area_dist_avg

#plot average heating and cooling load vs glazing area distribution
glazing_area_dist_avg %>% 
  gather(Load_type, average_load, -c(glazing_area_distribution,count)) %>% 
  ggplot(aes(glazing_area_distribution, average_load, color = Load_type)) + 
  geom_point() + 
  ggtitle("Average heating and cooling load for glazing area distribution")

############################
# Data Cleaning
############################
#set seed to 1
set.seed(1, sample.kind = "Rounding")

# Split energydat into probe and evaluation set. evaluation set will be 10% of energydat data set
index <- createDataPartition(energydat$heating_load, times = 1, p = 0.1, list = FALSE)
probe <- energydat[-index,]
evaluation <- energydat[index,]

# removes files and table that are no longer needed
rm(energydat, index)

#set seed (i.e the start number used in random sequence generation)
set.seed(1, sample.kind = "Rounding")

# set number of digits to be printed 
options(digits = 5) 

#split probe into two sets, train and test
test_index <- createDataPartition(probe$heating_load, times = 1, p = 0.2, list = FALSE)
train_set <- probe[-test_index,]
test_set <- probe[test_index,]
rm(test_index)

# define function for computing RMSE for vectors of rating and corresponding predictors
RMSE <- function(true_load, predicted_load){sqrt(mean((true_load - predicted_load)^2))}

############################
# Model 1: Using the average
############################
# compute the average of all the load
heat_avg <- mean(train_set$heating_load)
cool_avg <- mean(train_set$cooling_load)

# RMSE obtained by predicting mu for all unknown rating 
naive_rmse_1 <- RMSE(test_set$heating_load, heat_avg)
naive_rmse_2 <- RMSE(test_set$cooling_load, cool_avg)

RMSE_results <- data.frame(Model ="Naive",
                           RMSE_heat = naive_rmse_1,
                           RMSE_cool = naive_rmse_2) 
RMSE_results

############################
# Model 2: Linear regression
############################

fit_heat <- lm(heating_load ~ . - cooling_load, data = train_set) # fit lm model for heating load using 8 variables
fit_cool <- lm(cooling_load ~ . - heating_load, data = train_set) # fit lm model for cooling load using 8 variables

summary(fit_heat) #print summary of linear regression model for heating load
summary(fit_cool) #print summary of linear regression model for cooling load

alias(fit_heat)
# NA as coefficient for roof_area suggest that the variable can be excluded from our model
#alias shows roof_area is dependent on surface area and wall area

# update lm model for heating load to exclude roof_area
fit_heat <- lm(heating_load ~ . - cooling_load - roof_area, data = train_set)
# update lm model for cooling load to exclude roof_area
fit_cool <- lm(cooling_load ~ . - heating_load - roof_area, data = train_set)

alias(fit_heat) # no dependencies any longer
summary(fit_heat)#print summary of linear regression model for heating load
summary(fit_cool)#print summary of linear regression model for cooling load

lm_heat_pred <- predict(fit_heat, test_set) #predict heating load using lm model
lm_cool_pred <- predict(fit_cool, test_set) #predict cooling load using lm model

lm_heat_rmse <- RMSE(test_set$heating_load,lm_heat_pred ) #compute and assign rmse for heating load
lm_heat_rmse # print rmse
# same as with roof_area included

lm_cool_rmse <- RMSE(test_set$cooling_load,lm_cool_pred) #compute and assign rmse for cooling load
lm_cool_rmse #print rmse

#excluding variables with p>0.05 from linear regression for heating load
fit_heat_2 <- lm(heating_load ~ . - cooling_load - roof_area - orientation - glazing_area_distribution, data = train_set)
lm_heat_pred_2 <- predict(fit_heat_2, test_set) # predict heating load using lm model
lm_heat_2_rmse <- RMSE(test_set$heating_load,lm_heat_pred_2) #compute and assign RMSE for heating load
lm_heat_2_rmse #print rmse

#excluding variables with p>0.05 from linear regression for cooling load
fit_cool_2 <- lm(cooling_load ~ . - heating_load - roof_area - orientation - glazing_area_distribution, data = train_set)
lm_cool_pred_2 <- predict(fit_cool_2, test_set) # predict heating load using lm model
lm_cool_2_rmse <- RMSE(test_set$cooling_load,lm_cool_pred_2) #compute and assign RMSE for heating load
lm_cool_2_rmse #print rmse

# Update RMSE table
RMSE_results <- bind_rows(RMSE_results,
                          data.frame(Model ="Linear Regression",  
                                     RMSE_heat = lm_heat_2_rmse,
                                     RMSE_cool = lm_cool_2_rmse))
RMSE_results #print RMSE table


##############
# Model 3: KNN
##############

## KNN model for heating load
set.seed(1, sample.kind = "Rounding") #set seed to 1

#cross-validation for knn model for heating load
knn_cv_heat <- train(heating_load ~ . - cooling_load, method = "knn", 
                      data = train_set,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      trControl = trainControl(method = "cv", number = 10, p = .9))


knn_cv_heat$bestTune # display optimized complexity parameter 

knn_heat_pred <- predict(knn_cv_heat, test_set) # predict heating load

knn_heat_rmse  <- RMSE(test_set$heating_load,knn_heat_pred) # compute rmse for heating load

knn_heat_rmse # print rmse

## KNN model for cooling load
set.seed(1, sample.kind = "Rounding") # set seed to 1

# use cross-validation to choose k 
knn_cv_cool <- train(cooling_load ~ . - heating_load, method = "knn", 
                     data = train_set,
                     tuneGrid = data.frame(k = seq(3, 51, 2)),
                     trControl = trainControl(method = "cv", number = 10, p = .9))

knn_cv_cool$bestTune # display optimized k value

knn_cool_pred <- predict(knn_cv_cool, test_set) # predict cooling load

knn_cool_rmse  <- RMSE(test_set$cooling_load,knn_cool_pred) # compute rmse for cooling load

knn_cool_rmse # print rmse

# Update RMSE table
RMSE_results <- bind_rows(RMSE_results,
                          data.frame(Model ="knn",  
                                     RMSE_heat = knn_heat_rmse,
                                     RMSE_cool = knn_cool_rmse))
RMSE_results #print RMSE table

#######################
#Model 4: Decision tree 
#######################

# decision tree model for heating load
set.seed(1, sample.kind = "Rounding") # set seed to 5

# use cross-validation to choose complexity parameter,cp 
rpart_heat <- train(heating_load ~ . - cooling_load, method = "rpart",
                     data = train_set,
                     tuneGrid= data.frame(cp = seq(0, 0.1, 0.002)))

ggplot(rpart_heat) # plot rmse vs cp

rpart_heat$bestTune # display optimized cp

rpart_heat_pred <- predict(rpart_heat, test_set) # predict heating load

rpart_heat_rmse <- RMSE(test_set$heating_load,rpart_heat_pred) # compute and assign rmse for heating load

rpart_heat_rmse # print RMSE

# plot final model
plot(rpart_heat$finalModel)
text(rpart_heat$finalModel, cex = 0.8)


## decision tree model for cooling load
set.seed(1, sample.kind = "Rounding") # set seed to 5

# use cross-validation to select complexity parameter,cp
rpart_cool <- train(cooling_load ~ . - heating_load, method = "rpart",
                    data = train_set,
                    tuneGrid= data.frame(cp = seq(0, 0.1, 0.004)))

ggplot(rpart_cool) # plot rmse vs cp

rpart_cool$bestTune # display optimized cp

rpart_cool_pred <- predict(rpart_cool, test_set) # predict cooling load

rpart_cool_rmse <- RMSE(test_set$cooling_load,rpart_cool_pred) # compute and assign rmse for cooling load

rpart_cool_rmse # print rmse

# plot final model
plot(rpart_cool$finalModel)
text(rpart_cool$finalModel, cex = 0.8)

# Update RMSE table
RMSE_results <- bind_rows(RMSE_results,
                          data.frame(Model ="Decision Tree",  
                                     RMSE_heat = rpart_heat_rmse,
                                     RMSE_cool = rpart_cool_rmse))
RMSE_results #print RMSE table

#########################
# Model 5 : Random forest
#########################

## random forest model for heating load
set.seed(5, sample.kind = "Rounding") # set seed to 5

# use cross-validation to select mtry
rf_heat <- train(heating_load ~ . - cooling_load, method = "rf", data= train_set, 
                  ntree = 100, tuneGrid = data.frame(mtry = seq(1:7)))

ggplot(rf_heat) #plot number of predictors vs RMSE

rf_heat$bestTune #display optimized mtry

library(randomForest)
check_tree <- randomForest(heating_load ~ . - cooling_load,
                       data= train_set,
                       ntree = 150,
                       minNode = rf_heat$bestTune$mtry)

plot(check_tree) #plot number of tree vs error 



rf_heat_pred <- predict(rf_heat, test_set) # predict heating load

rf_heat_rmse <- RMSE(test_set$heating_load,rf_heat_pred) # compute and assign rmse for heating load

rf_heat_rmse # print rmse

#important variable
varImp(rf_heat)

## random forest model for cooling load
set.seed(5, sample.kind = "Rounding")
rf_cool <- train(cooling_load ~ . - heating_load, method = "rf", data= train_set, 
                 ntree = 100, tuneGrid = data.frame(mtry = seq(1:7)))
            
#mtry value that maximizes accuracy
rf_cool$bestTune

rf_cool_pred <- predict(rf_cool, test_set) # predict cooling load

rf_cool_rmse <- RMSE(test_set$cooling_load,rf_cool_pred) # compute and assign rmse

rf_cool_rmse # print rmse

#important variable
varImp(rf_cool)

# Update RMSE table
RMSE_results <- bind_rows(RMSE_results,
                          data.frame(Model ="Random Forest",  
                                     RMSE_heat = rf_heat_rmse,
                                     RMSE_cool = rf_cool_rmse))
RMSE_results #print RMSE table

##############################
# Application to evaluation set
##############################
## Model 5 (random forest) for for predicting heating load in evaluation set
set.seed(5, sample.kind = "Rounding") # set seed to 5

# use cross-validation to select mtry
rf_heat <- train(heating_load ~ . - cooling_load, method = "rf", data= probe, 
                 ntree = 100, tuneGrid = data.frame(mtry = seq(1:7)))

ggplot(rf_heat)

pred_heat <- predict(rf_heat, evaluation) # predict heating load

heat_rmse <- RMSE(evaluation$heating_load,pred_heat) # compute and assign rmse for heating load

heat_rmse # print rmse

## Model 3 (knn) for predicting cooling load in evaluation set
set.seed(5, sample.kind = "Rounding") # set seed to 1

# use cross-validation to choose k 
knn_cool <- train(cooling_load ~ . - heating_load, method = "knn", 
                     data = probe,
                     tuneGrid = data.frame(k = seq(3, 51, 2)),
                     trControl = trainControl(method = "cv", number = 10, p = .9))

knn_cool$bestTune # display optimized k value

pred_cool <- predict(knn_cool, evaluation) # predict cooling load

cool_rmse  <- RMSE(evaluation$cooling_load,pred_cool) # compute rmse for cooling load

cool_rmse # print rmse

