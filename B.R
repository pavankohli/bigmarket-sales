#read the data
saletrain<-read.csv(file.choose())
saletest<-read.csv(file.choose())
#saletrain=as.data.table(saletrain)



#combining train and test
library(data.table)
saletest<-as.data.table(saletest)
saletest[,Item_Outlet_Sales := NA]
combi = rbind(saletrain, saletest) # combining train and test datasets
dim(combi)



#decreasing levels  
combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular"



saletrain = combi[1:nrow(saletrain)]



#checking missing values
colSums(is.na(combi))

#filling missing values with mean
missing_index = which(is.na(combi$Item_Weight))
for(i in missing_index){
  
  item = combi$Item_Identifier[i]
  combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier == item], na.rm = T)
  
}

#Replacing 0's in Item_Visibility variable
zero_index = which(combi$Item_Visibility == 0)
for(i in zero_index){
  
  item = combi$Item_Identifier[i]
  combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = T)
  
}


str(combi)
summary(combi)
# create a new feature 'Item_Type_new' 
perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene",
                   "Household", "Soft Drinks")
library(data.table)
combi[,Item_Type_new := ifelse(Item_Type %in% perishable, "perishable",
                               ifelse(Item_Type %in% non_perishable, "non_perishable", "not_sure"))]



table(combi$Item_Type, substr(combi$Item_Identifier, 1, 2))



combi[,Item_category := substr(combi$Item_Identifier, 1, 2)]
combi$Item_Fat_Content[combi$Item_category == "NC"] = "Non-Edible"


# years of operation of outlets
combi[,Outlet_Years := 2013 - Outlet_Establishment_Year]
combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year)
# Price per unit weight
combi[,price_per_unit_wt := Item_MRP/Item_Weight]


#dividing item MRP into clusters
Item_MRP_clusters = kmeans(combi$Item_MRP, centers = 4)
table(Item_MRP_clusters$cluster)

combi$Item_MRP_clusters = as.factor(Item_MRP_clusters$cluster)

str(combi)
#Label encoding for the categorical variables
combi[,Outlet_Size_num := ifelse(Outlet_Size == "Small", 0,
                                 ifelse(Outlet_Size == "Medium", 1, 2))]
combi[,Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0,
                                          ifelse(Outlet_Location_Type == "Tier 2", 1, 2))]
# removing categorical variables after label encoding
combi[, c("Outlet_Size", "Outlet_Location_Type") := NULL]




#One hot encoding for the categorical variable
ohe = dummyVars("~.", data = combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = T)
ohe_df = data.table(predict(ohe, combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")]))
combi = cbind(combi[,"Item_Identifier"], ohe_df)

#checking skewness
skewness(combi$Item_Visibility); skewness(combi$price_per_unit_wt)


combi[,Item_Visibility := log(Item_Visibility + 1)] # log + 1 to avoid division by zero
combi[,price_per_unit_wt := log(price_per_unit_wt + 1)]

str(combi)
#Scaling numeric predictors
num_vars = which(sapply(combi, is.numeric)) # index of numeric features
num_vars_names = names(num_vars)
num_vars_names
combi_numeric = combi[,setdiff(num_vars_names, "Item_Outlet_Sales"), with = F]
prep_num = preProcess(combi_numeric, method=c("center", "scale"))
combi_numeric_norm = predict(prep_num, combi_numeric)



combi[,setdiff(num_vars_names, "Item_Outlet_Sales") := NULL] # removing numeric independent variables
combi = cbind(combi, combi_numeric_norm)
summary(combi)

#dropping the columns with zeroes
#combi$`Item_Fat_Content.low fat`=NULL
#combi$Item_Fat_Content.reg=NULL
#boxplot(combi$`Item_Fat_Content.Low Fat`)

#replacing missing values with mean w.r.t item_identifier
#missing_index = which(is.na(combi$Item_Outlet_Sales))
#for(i in missing_index){
  
  #item = combi$Item_Identifier[i]
  #combi$Item_Outlet_Sales[i] = mean(combi$Item_Outlet_Sales[combi$Item_Identifier == item], na.rm = T)
  
#}


#replace missing values with mean
combi$`Item_Fat_Content.Low Fat`[is.na(combi$`Item_Fat_Content.Low Fat`)] <- round(mean(combi$`Item_Fat_Content.Low Fat`, na.rm = TRUE))
combi$Item_Fat_Content.Regular[is.na(combi$Item_Fat_Content.Regular)] <- round(mean(combi$Item_Fat_Content.Regular, na.rm = TRUE))

combi$`Item_Fat_Content.low fat`[is.na(combi$`Item_Fat_Content.low fat`)] <- round(mean(combi$`Item_Fat_Content.low fat`, na.rm = TRUE))
combi$Item_Fat_Content.reg[is.na(combi$Item_Fat_Content.reg)] <- round(mean(combi$Item_Fat_Content.reg, na.rm = TRUE))

#split back 
set.seed(123)
saletrain = combi[1:nrow(saletrain)]
saletest = combi[(nrow(saletrain) + 1):nrow(combi)]
saletest[,Item_Outlet_Sales := NULL]

#split saletrain
set.seed(123)
train_rows<-sample(x=1:nrow(saletrain),size=0.75*nrow(saletrain))
train<-saletrain[train_rows,]
validation<-saletrain[-train_rows,]

## Correlation Plot
cor_train = cor(saletrain)

library(corrplot)
corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)



names(combi)
str(train)
summary(train)


#removing missing values after splitting
#library(DMwR)
#train=centralImputation(train)
colSums(is.na(train))
#validation=centralImputation(validation)
colSums(is.na(saletest))

#linear regression
library(e1071)
library(caret)
#validation=as.data.frame(validation)
 # train=as.data.frame(train)
saletrain$Item_Identifier=NULL
saletest$Item_Identifier=NULL
#validation$Item_Outlet_Sales=NULL
model1=lm(Item_Outlet_Sales~.,train)
pred.NB1=predict(model1,validation)
pred.NB2=predict(model1,saletest)

#RMSE in another way
library(DMwR)
regr.eval(train$Item_Outlet_Sales,pred.NB1)

#RMSE(2119.263)
RMSE(train$Item_Outlet_Sales,pred.NB1)



write.csv(pred.NB1,"pred.NB1.csv")

#CROSS VALIDATION FOR LINEAR REGRESSION
library(caret)
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE,repeats = 10)


is.data.frame(saletrain)
model_reg<- train(Item_Outlet_Sales~., data = train, trControl=ctrl,method = "lm")


pred_test <- predict(model_reg,validation)
#RMSE(2119.263)
RMSE(train$Item_Outlet_Sales,pred_test)



str(saletrain)




#RANDOM FOREST
library(ipred)
library(party)
library(randomForest)
names(train) <- make.names(names(train))
names(validation) <- make.names(names(validation))
cTreeMod <-randomForest(Item_Outlet_Sales~., data=train,ntry=300,mtry = 4,importance = TRUE,
                        proximity = TRUE) 
predicted <- predict(cTreeMod, newdata =validation)

#RMSE in another way
library(DMwR)
regr.eval(saletrain$Item_Outlet_Sales,predicted)

#RMSE(2107.627)
RMSE(train$Item_Outlet_Sales,predicted)



#TUNING RANDOM FOREST
#not working
trainx1=train[,-1]
trainy2=train[,1]
t <- tuneRF(trainx1,trainy2,
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 200,
            trace = TRUE, 
            improve = 0.05)

predicted1 <- predict(t,trainy2)


varImpPlot(cTreeMod)
importance(cTreeMod)
varUsed(cTreeMod)





#LASSO REGRESSION
library(glmnet)
library(foreach)
set.seed(1235)
my_control = trainControl(method="cv", number=5)
Grid = expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0002))
library(caret)
x1 = saletrain[,-c("Item_Outlet_Sales")]
lasso_linear_reg_mod = train(trainx1,trainy2,
                             method='glmnet', trControl= my_control, tuneGrid = Grid)
names(x)


pre<-predict(lasso_linear_reg_mod,saletest)

#RMSE (2116.728)
RMSE(train$Item_Outlet_Sales,pre)

#RMSE in another way
library(DMwR)
regr.eval(train$Item_Outlet_Sales,pre)

# mean validation score(1132.704)
mean(lasso_linear_reg_mod$resample$RMSE)

#RIDGE REGRESSION

my_control = trainControl(method="cv", number=5)
Grid1 = expand.grid(alpha = 0, lambda = seq(0.001,0.1,by = 0.0002))

ridge_linear_reg_mod = train(trainx1,trainy2,
                             method='glmnet', trControl= my_control, tuneGrid = Grid1)



pre1<-predict(ridge_linear_reg_mod ,saletest)
#RMSE(2087.303)
RMSE(train$Item_Outlet_Sales,pre1)

#RMSE in another way
library(DMwR)
regr.eval(train$Item_Outlet_Sales,pre1)

# mean validation score(1136.956)
mean(ridge_linear_reg_mod$resample$RMSE)


#XGBOOST
param_list = list(
  
  objective = "reg:linear",
  eta=0.01,
  gamma = 1,
  max_depth=6,
  subsample=0.8,
  colsample_bytree=0.5
)
library(xgboost)
library(Matrix)
dtrain = xgb.DMatrix(data = as.matrix(saletrain[,-Item_Outlet_Sales]), label= saletrain$Item_Outlet_Sales)
dtest = xgb.DMatrix(data = as.matrix(saletest))

xgbcv = xgb.cv(params = param_list, 
               data = dtrain, 
               nrounds = 1000, 
               nfold = 5, 
               print_every_n = 10, 
               early_stopping_rounds = 30, 
               maximize = F)


xgb_model = xgb.train(data = dtrain, params = param_list, nrounds = 430)
summary(xgb_model)
pre3<-predict(xgb_model,newdata = dtest)

#RMSE
RMSE(dtrain$Item_Outlet_Sales,pre3)

#RMSE in another way
library(DMwR)
regr.eval(dtrain$Item_Outlet_Sales,pre3)

#VARIABLE IMPORTANCE
var_imp = xgb.importance(feature_names = setdiff(names(saletrain), c("Item_Identifier", "Item_Outlet_Sales")), 
                         model = xgb_model)
xgb.plot.importance(var_imp)

write.csv(predicted,"predicted.csv")


#decision tree regressor
library(rpart)
fit <- rpart(Item_Outlet_Sales~., 
             method="anova", data=train )
pre4<-predict(fit, newdata = validation,method = "anova",control = rpart.control(cp = 0.0012))
pre4


#plot
par(mfrow=c(1,2)) 
rsq.rpart(fit)

summary(fit)

#RMSE(2151.482)
RMSE(train$Item_Outlet_Sales,pre4)










#support vector regression
library(e1071)
svrmodel<-svm(Item_Outlet_Sales~., data =train, gamma=0.002, scale=TRUE)
predictsvr1=predict(svrmodel, validation)
names(saletest) <- make.names(names(saletest))
predictsvr123=predict(svrmodel,saletest)
#RMSE(1748.878)



write.csv(predictsvr123,"predictsvr123.csv")


RMSE(predictsvr1,train$Item_Outlet_Sales)


#tuning the SVR 

tc<-tune.control(cross = 5)

library(e1071)
tuneResult <- tune.svm(Item_Outlet_Sales~.,  data =train, cost = Cs, gamma = gammas,
                   tunecontrol = tc)
            

summary(tuneResult)
tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, newdata = validation)

#RMSE
RMSE(train$Item_Outlet_Sales,tunedModelY)


write.csv(predictsvr)

#my score for first submission is 1726 and the leader board position is 1833


#CROSS VALIDATION FOR SVM
library(caret)
library(e1071)
library(kernlab)
ctrl <- trainControl(method = "cv", number = 5, savePredictions = TRUE,classProbs = TRUE)



model_reg<- train(Item_Outlet_Sales~., data = train, trControl=ctrl,method = "svmRadial")


predictcv1<-predict(model_reg,validation)



RMSE(train$Item_Outlet_Sales,predictcv1)













#GBM
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)



my_control = trainControl(method="cv", number=10)

gbmFit2 <- train(Item_Outlet_Sales ~ ., data = saletrain, 
                 method = "gbm", 
                 trControl = my_control, 
                 verbose = FALSE, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)











