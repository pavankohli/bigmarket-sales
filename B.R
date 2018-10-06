#read the data
saletrain<-read.csv(file.choose())
saletest<-read.csv(file.choose())




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



Item_MRP_clusters = kmeans(combi$Item_MRP, centers = 4)
table(Item_MRP_clusters$cluster)

combi$Item_MRP_clusters = as.factor(Item_MRP_clusters$cluster)


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


#Scaling numeric predictors
num_vars = which(sapply(combi, is.numeric)) # index of numeric features
num_vars_names = names(num_vars)
combi_numeric = combi[,setdiff(num_vars_names, "Item_Outlet_Sales"), with = F]
prep_num = preProcess(combi_numeric, method=c("center", "scale"))
combi_numeric_norm = predict(prep_num, combi_numeric)



combi[,setdiff(num_vars_names, "Item_Outlet_Sales") := NULL] # removing numeric independent variables
combi = cbind(combi, combi_numeric_norm)

#split back 
saletrain = combi[1:nrow(saletrain)]
saletest = combi[(nrow(saletrain) + 1):nrow(combi)]
saletest[,Item_Outlet_Sales := NULL] # removing Item_Outlet_Sales as it contains only NA
summary(saletrain)
names(saletrain)
names(saletest)
#removing missing values after splitting
saletrain=centralImputation(saletrain)
colSums(is.na(saletrain))
saletest=centralImputation(saletest)
colSums(is.na(saletest))

#linear regression
library(e1071)
library(caret)
saletest=as.data.frame(saletest)
saletrain=as.data.frame(saletrain)
saletrain$Item_Identifier=NULL
saletest$Item_Identifier=NULL
model1=lm(Item_Outlet_Sales~.,saletrain)
pred.NB1=predict(model1,saletest)
pred.NB2=predict(model1,saletrain)

#RMSE in another way
library(DMwR)
regr.eval(saletrain$Item_Outlet_Sales,pred.NB1)

#RMSE(2142.063)
RMSE(saletrain$Item_Outlet_Sales,pred.NB1)



#RANDOM FOREST
library (ipred)
library(party)
library(randomForest)
names(saletrain) <- make.names(names(saletrain))
names(saletest) <- make.names(names(saletest))
cTreeMod <-randomForest(Item_Outlet_Sales~., data=saletrain,ntry=200,mtry = 4) 
predicted <- predict(cTreeMod, newdata = saletest)

#RMSE in another way
library(DMwR)
regr.eval(saletrain$Item_Outlet_Sales,predicted)

#RMSE(2108.403)
RMSE(saletrain$Item_Outlet_Sales,predicted)


#LASSO REGRESSION
library(glmnet)
my_control = trainControl(method="cv", number=5)
Grid = expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0002))
library(caret)
x = saletrain[,-c("Item_Outlet_Sales")]
lasso_linear_reg_mod = train(x = saletrain[,-c("Item_Outlet_Sales,Item_Identifier")], y = saletrain$Item_Outlet_Sales,
                             method='glmnet', trControl= my_control, tuneGrid = Grid)



pre<-predict(lasso_linear_reg_mod,newdata =x)

#RMSE 
RMSE(saletrain$Item_Outlet_Sales,pre)

#RMSE in another way
library(DMwR)
regr.eval(saletrain$Item_Outlet_Sales,pre)



#RIDGE REGRESSION

my_control = trainControl(method="cv", number=10)
Grid1 = expand.grid(alpha = 0, lambda = seq(0.001,0.1,by = 0.0002))

ridge_linear_reg_mod = train(x = saletrain[, -c( "Item_Outlet_Sales")], y = saletrain$Item_Outlet_Sales,
                             method='glmnet', trControl= my_control, tuneGrid = Grid1)



pre1<-predict(ridge_linear_reg_mod ,saletest)
#RMSE
RMSE(saletrain$Item_Outlet_Sales,pre1)

#RMSE in another way
library(DMwR)
regr.eval(saletrain$Item_Outlet_Sales,pre1)




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
dtrain = xgb.DMatrix(data = as.matrix(saletrain[,-c( "Item_Outlet_Sales")]), label= saletrain$Item_Outlet_Sales)
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
pre3<-predict(xgb_model,newdata = dtrain)

#RMSE
RMSE(saletrain$Item_Outlet_Sales,pre3)

#RMSE in another way
library(DMwR)
regr.eval(saletrain$Item_Outlet_Sales,pre3)

#VARIABLE IMPORTANCE
var_imp = xgb.importance(feature_names = setdiff(names(saletrain), c("Item_Identifier", "Item_Outlet_Sales")), 
                         model = xgb_model)
xgb.plot.importance(var_imp)

write.csv(predicted,"predicted.csv")
