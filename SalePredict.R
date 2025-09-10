#install.packages("tidyverse")
library(tidyverse)

train_data=read.csv("C:/Users/Seval/train.csv")
test_data=read.csv("C:/Users/Seval/test.csv")

head(train_data)
head(test_data)

dim(train_data) #1460 81
dim(test_data)  #1459 80

str(train_data)
summary(train_data)
str(test_data)
summary(test_data)

NA_train <- train_data %>% summarise(across(everything(), ~sum(is.na(.))))  %>% 
  pivot_longer(cols = everything(), names_to = "column", values_to = "NA_count") %>%
  arrange(desc(NA_count))
#column      NA_count
#<chr>          <int>
#1 PoolQC          1453
#2 MiscFeature     1406
#3 Alley           1369
#4 Fence           1179
#5 FireplaceQu      690

NA_test <- test_data %>% summarise(across(everything(),~sum(is.na(.)))) %>% 
  pivot_longer(cols = everything(), names_to = "column", values_to = "NA_count") %>%
  arrange(desc(NA_count)) 
#column      NA_count
#<chr>          <int>
#1 PoolQC          1456
#2 MiscFeature     1408
#3 Alley           1352
#4 Fence           1169
#5 FireplaceQu      730

#across Apply a function (or functions) across multiple columns 
#data-masking" functions like summarise() and mutate()
#across(.cols, .fns, ..., .names = NULL, .unpack = FALSE)
#gdf %>% mutate(across(v1:v2, ~ .x + n))

#pivot_longer() "lengthens" data, increasing the number of rows and decreasing 
#the number of columns. The inverse transformation is
#pivot_longer(
#data,
#cols,
#...,
#cols_vary = "fastest",
#names_to = "name",
#names_prefix = NULL,
#names_sep = NULL,
#names_pattern = NULL,
#names_ptypes = NULL,
#names_transform = NULL,
#names_repair = "check_unique",
#values_to = "value",
#values_drop_na = FALSE,
#values_ptypes = NULL,
#values_transform = NULL)

cors <- train_data %>% select(where(is.numeric), -SalePrice)  %>%
  summarise(across(everything(),~cor(.x,train_data$SalePrice, use = "complete.obs"))) %>%
  pivot_longer(everything(),names_to = "column",values_to = "correlation") %>%
  arrange(desc(correlation))

head(cors,5)
#column      correlation
#<chr>             <dbl>
#1 OverallQual       0.791
#2 GrLivArea         0.709
#3 GarageCars        0.640
#4 GarageArea        0.623
#5 TotalBsmtSF       0.614

#----------------------------------------------------------------------------------------------------------------------
train_fixed <- train_data

#categorical
train_fixed <- train_fixed %>% mutate(Alley=replace_na(Alley,"NoAlley"))
train_fixed <- train_fixed %>% mutate(BsmtCond=replace_na(BsmtCond,"NoBasement"))
train_fixed <- train_fixed %>% mutate(BsmtExposure=replace_na(BsmtExposure,"NoBasement"))
train_fixed <- train_fixed %>% mutate(BsmtFinType1=replace_na(BsmtFinType1,"NoBasement"))
train_fixed <- train_fixed %>% mutate(BsmtFinType2=replace_na(BsmtFinType2,"NoBasement"))
train_fixed <- train_fixed %>% mutate(BsmtQual=replace_na(BsmtQual,"NoBasement"))
train_fixed <- train_fixed %>% mutate(Fence=replace_na(Fence,"NoFence"))
train_fixed <- train_fixed %>% mutate(FireplaceQu=replace_na(FireplaceQu,"NoFireplace"))
train_fixed <- train_fixed %>% mutate(GarageCond=replace_na(GarageCond,"NoGarage"))
train_fixed <- train_fixed %>% mutate(GarageFinish=replace_na(GarageFinish,"NoGarage"))
train_fixed <- train_fixed %>% mutate(GarageQual=replace_na(GarageQual,"NoGarage"))
train_fixed <- train_fixed %>% mutate(GarageType=replace_na(GarageType,"NoGarage"))
train_fixed <- train_fixed %>% mutate(MasVnrType=replace_na(MasVnrType,"NoVeneer"))
train_fixed <- train_fixed %>% mutate(MiscFeature=replace_na(MiscFeature,"None"))
train_fixed <- train_fixed %>% mutate(PoolQC=replace_na(PoolQC,"NoPool"))
train_fixed <- train_fixed %>% mutate(Electrical=replace_na(Electrical,"Unknown"))

#numerical
train_fixed <-train_fixed %>% mutate(GarageYrBlt = if_else(GarageType=="NoGarage",0,GarageYrBlt))
train_fixed <-train_fixed %>% mutate(GarageYrBlt = if_else(is.na(GarageYrBlt),median(GarageYrBlt,na.rm = TRUE),GarageYrBlt))
train_fixed <-train_fixed %>% group_by(Neighborhood) %>% 
  mutate(LotFrontage = if_else(is.na(LotFrontage),median(LotFrontage,na.rm = TRUE),LotFrontage)) %>% ungroup()
train_fixed <-train_fixed %>% mutate(MasVnrArea = if_else(MasVnrType=="NoVeneer",0,MasVnrArea))
train_fixed <- train_fixed %>% mutate(MasVnrArea = if_else(is.na(MasVnrArea),median(MasVnrArea, na.rm=TRUE),MasVnrArea))

train_fixed %>%
  summarise(numeric_na = sum(is.na(select(., where(is.numeric)))),
            categorical_na = sum(is.na(select(., where(is.character)))))

#---------------------------------------------------------------------------------------------------------------------
test_fixed=test_data

test_fixed %>% filter(is.na(BsmtFinSF1)) %>% select(Id) #2121
test_fixed %>% filter(is.na(BsmtFinSF2)) %>% select(Id) #2121
test_fixed %>% filter(is.na(BsmtFullBath)) %>% select(Id) #2121 2189
test_fixed %>% filter(is.na(BsmtHalfBath)) %>% select(Id) #2121 2189
test_fixed %>% filter(is.na(BsmtUnfSF)) %>% select(Id) #2121
test_fixed %>% filter(is.na(Exterior1st)) %>% select(Id) #2152
test_fixed %>% filter(is.na(Exterior2nd)) %>% select(Id) #2152
test_fixed %>% filter(is.na(Functional)) %>% select(Id) #2217 2474
test_fixed %>% filter(is.na(GarageArea)) %>% select(Id) #2577
test_fixed %>% filter(is.na(GarageCars)) %>% select(Id) #2577
test_fixed %>% filter(is.na(KitchenQual)) %>% select(Id) #1556
test_fixed %>% filter(is.na(SaleType)) %>% select(Id) #2490
test_fixed %>% filter(is.na(TotalBsmtSF)) %>% select(Id) #2121
test_fixed %>% filter(is.na(Utilities)) %>% select(Id) #1916 1946

test_fixed <- test_fixed %>% mutate(Alley=replace_na(Alley,"NoAlley"))
test_fixed <- test_fixed %>% mutate(BsmtCond=replace_na(BsmtCond,"NoBasement"))
test_fixed <- test_fixed %>% mutate(BsmtExposure=replace_na(BsmtExposure,"NoBasement"))
test_fixed <- test_fixed %>% mutate(BsmtFinType1=replace_na(BsmtFinType1,"NoBasement"))
test_fixed <- test_fixed %>% mutate(BsmtFinType2=replace_na(BsmtFinType2,"NoBasement"))
test_fixed <- test_fixed %>% mutate(BsmtQual=replace_na(BsmtQual,"NoBasement"))
test_fixed <- test_fixed %>% mutate(Fence=replace_na(Fence,"NoFence"))
test_fixed <- test_fixed %>% mutate(FireplaceQu=replace_na(FireplaceQu,"NoFireplace"))
test_fixed <- test_fixed %>% mutate(GarageCond=replace_na(GarageCond,"NoGarage"))
test_fixed <- test_fixed %>% mutate(GarageFinish=replace_na(GarageFinish,"NoGarage"))
test_fixed <- test_fixed %>% mutate(GarageQual=replace_na(GarageQual,"NoGarage"))
test_fixed <- test_fixed %>% mutate(GarageType=replace_na(GarageType,"NoGarage"))
test_fixed <- test_fixed %>% mutate(KitchenQual=replace_na(KitchenQual,"Unknown"))
test_fixed <- test_fixed %>% mutate(MSZoning=replace_na(MSZoning,"Unknown"))
test_fixed <- test_fixed %>% mutate(MasVnrType=replace_na(MasVnrType,"NoVeneer"))
test_fixed <- test_fixed %>% mutate(MiscFeature=replace_na(MiscFeature,"None"))
test_fixed <- test_fixed %>% mutate(PoolQC=replace_na(PoolQC,"NoPool"))
test_fixed <- test_fixed %>% mutate(Utilities=replace_na(Utilities,"NoUtils"))

test_fixed <- test_fixed %>% mutate(BsmtFullBath = if_else(BsmtQual=="NoBasement",0,BsmtFullBath))
test_fixed <- test_fixed %>% mutate(BsmtFullBath= if_else(is.na(BsmtFullBath),median(BsmtFullBath,na.rm = TRUE),BsmtFullBath))
test_fixed <- test_fixed %>% mutate(BsmtHalfBath = if_else(BsmtQual=="NoBasement",0,BsmtHalfBath))
test_fixed <- test_fixed %>% mutate(BsmtHalfBath= if_else(is.na(BsmtHalfBath),median(BsmtHalfBath,na.rm = TRUE),BsmtHalfBath))
test_fixed <- test_fixed %>% mutate(GarageArea = if_else(GarageType=="NoGarage",0,GarageArea))
test_fixed <- test_fixed %>% mutate(GarageArea= if_else(is.na(GarageArea),median(GarageArea,na.rm = TRUE),GarageArea))
test_fixed <- test_fixed %>% mutate(GarageCars = if_else(GarageType=="NoGarage",0,GarageCars))
test_fixed <- test_fixed %>% mutate(GarageCars= if_else(is.na(GarageCars),median(GarageCars,na.rm = TRUE),GarageCars))
test_fixed <- test_fixed %>% mutate(GarageYrBlt = if_else(GarageType=="NoGarage",0,GarageYrBlt))
test_fixed <- test_fixed %>% mutate(GarageYrBlt= if_else(is.na(GarageYrBlt),median(GarageYrBlt,na.rm = TRUE),GarageYrBlt))
test_fixed <- test_fixed %>% group_by(Neighborhood) %>% 
  mutate(LotFrontage=if_else(is.na(LotFrontage),median(LotFrontage,na.rm = TRUE),LotFrontage)) %>% ungroup()
test_fixed <- test_fixed %>% mutate(MasVnrArea = if_else(MasVnrType=="NoVeneer",0,MasVnrArea))
test_fixed <- test_fixed %>% mutate(MasVnrArea= if_else(is.na(MasVnrArea),median(MasVnrArea,na.rm = TRUE),MasVnrArea))

test_fixed <- test_fixed %>%
  mutate(across(where(is.numeric), ~(replace_na(., round(median(., na.rm = TRUE))))))
test_fixed <- test_fixed %>% 
  mutate(across(where(is.character), ~replace_na(., "Unknown")))

#test_fixed <- test_fixed %>% filter(!Id %in% c(2121,2152,2217,2474,2490))

test_fixed %>%summarise(numeric_na = sum(is.na(select(., where(is.numeric)))),
                      categorical_na = sum(is.na(select(., where(is.character)))))

#------------------------------------------------------------------------------------------------------------------------------------

train_z_outliers <-train_fixed %>% select(where(is.numeric)) %>% 
                               mutate(across(-Id, ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE))) %>%
                               rowwise() %>%  
                               mutate(outlier_count = sum(c_across(-Id) > 3, na.rm = TRUE)) %>%
                               ungroup() %>%
                               select(Id, outlier_count)


#rowwise -group_by by row
#allows you to compute on a data frame a row-at-a-time. 
#This is most useful when a vectorised function doesn't exist.

#c_across
# is designed to work with rowwise() to make it easy to perform row-wise aggregations. 
#It has two differences from c():

train_z_outliers %>% arrange(desc(outlier_count)) #1299 1183 186 770

train_fixed %>% filter(Id==1299)
train_fixed %>% filter(Id==1183)
train_fixed %>% filter(Id==186)
train_fixed %>% filter(Id==770)

#------------------------------------------------------------------------------------------------------------------------------------
train_fixed <- train_fixed %>% mutate(MSSubClass =as.character(MSSubClass))
train_fixed <- train_fixed %>% mutate(OverallQual =as.character(OverallQual))
train_fixed <- train_fixed %>% mutate(OverallCond =as.character(OverallCond))

test_fixed <- test_fixed %>% mutate(MSSubClass =as.character(MSSubClass))
test_fixed <- test_fixed %>% mutate(OverallQual =as.character(OverallQual))
test_fixed <- test_fixed %>% mutate(OverallCond =as.character(OverallCond))

train_fixed <- train_fixed %>% select(-Id)
Y <- train_fixed$SalePrice
train_fixed <- train_fixed %>% select(-SalePrice)
#-----------------------------------------------------------------------------------------------------------------------------------
#install.packages("moments")
library(moments)
#install.packages("patchwork")
library(patchwork)

skewed_cols <- train_fixed %>% select(where(is.numeric)) %>% sapply(skewness) %>%
  enframe(name = "columns", value = "skewed") %>% filter(abs(skewed) > 0.75)

skewed_col_names <- skewed_cols %>% pull(columns)

train_fixed <- train_fixed %>% mutate(across(all_of(skewed_col_names), ~log1p(.x)))
test_fixed <- test_fixed %>% mutate(across(all_of(skewed_col_names), ~log1p(.x)))

top_skewed_col_names <- skewed_cols %>% arrange(desc(abs(skewed))) %>% head(5) %>% 
  pull(columns) 

for (col in top_skewed_col_names) {
  
  plot_before <- train_fixed %>%
    ggplot(aes(x = expm1(!!sym(col)))) +
    geom_histogram(bins = 50, fill = "blue", color = "black") +
    labs(title = paste0(col, " (Before Transformation)"), x = "Value") +
    theme_minimal()
  
  plot_after <- train_fixed %>%
    ggplot(aes(x = !!sym(col))) +
    geom_histogram(bins = 50, fill = "orange", color = "black") +
    labs(title = paste0(col, " (After Transformation)"), x = "Value") +
    theme_minimal()
  
  print(plot_before + plot_after)
}

plot_y_before <- data.frame(Y_value = Y) %>% ggplot(aes(x=Y_value)) + 
  geom_histogram(bins = 50, fill = "magenta", color = "black") +
  labs(title = "Y (Before Transformation)",x="Value") + theme_minimal()

Y <- log1p(Y)

plot_y_after <- data.frame(Y_value = Y) %>% ggplot(aes(x=Y_value)) + 
  geom_histogram(bins = 50, fill = "green", color = "black") +
  labs(title = "Y (After Transformation)",x="Value") + theme_minimal()

print(plot_y_before + plot_y_after)
#-----------------------------------------------------------------------------------------------------------------------------------
names(train_fixed)

train_fixed <- train_fixed %>%
  mutate(Total_Baths = BsmtFullBath + BsmtHalfBath + FullBath + HalfBath)

train_fixed <- train_fixed %>%
  mutate(Total_Porch_Area = OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch)
#Backticks for column like start with number etc.

train_fixed <- train_fixed %>%
  mutate(Total_Area = GrLivArea + TotalBsmtSF)

test_fixed <- test_fixed %>%
  mutate(Total_Baths = BsmtFullBath + BsmtHalfBath + FullBath + HalfBath)

test_fixed <- test_fixed %>%
  mutate(Total_Porch_Area = OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch)

test_fixed <- test_fixed %>%
  mutate(Total_Area = GrLivArea + TotalBsmtSF)

#-----------------------------------------------------------------------------------------------------------------------------------
train_fixed <- train_fixed %>% mutate(IsRemodelling = if_else((YearRemodAdd - YearBuilt >0),1,0))
train_fixed <- train_fixed %>% mutate(IsBasement = if_else(TotalBsmtSF >0,1,0))
train_fixed <- train_fixed %>% mutate(IsPool = if_else((PoolArea >0),1,0))

test_fixed <- test_fixed %>% mutate(IsRemodelling = if_else((YearRemodAdd - YearBuilt >0),1,0))
test_fixed <- test_fixed %>% mutate(IsBasement = if_else(TotalBsmtSF >0,1,0))
test_fixed <- test_fixed %>% mutate(IsPool = if_else(PoolArea >0,1,0))

#----------------------------------------------------------------------------------------------------------------------------------
#install.packages("fastDummies")
#library(fastDummies)

#train_fixed <- dummy_cols(train_fixed, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
#test_fixed <- dummy_cols(test_fixed, remove_first_dummy = TRUE, remove_selected_columns = TRUE)

test_ids <- test_fixed$Id

missing_cols <- setdiff(names(train_fixed), names(test_fixed))
test_fixed[missing_cols] <- 0
# $ for only one column [] for multiple column if used

test_fixed <- test_fixed %>% select(all_of(names(train_fixed)))
all.equal(names(train_fixed), names(test_fixed))

#---------------------------------------------------------------------------------------------------------------------------------
#install.packages("tidymodels")
#install.packages("car")
library(tidymodels)
library(car)

X=train_fixed

train_merged <- cbind(X, Y) %>% as_tibble()
names(train_merged)[names(train_merged) == "Y"] <- "log_sale_price"

names(train_merged)

#Train-Test Split
set.seed(42) 
data_split <- initial_split(train_merged, prop = 0.8) 

train_splited <- training(data_split)
test_splited <- testing(data_split)

final_recipe <- recipe(log_sale_price ~ ., data = train_splited) %>%
  step_novel(all_nominal_predictors()) %>%      
  step_unknown(all_nominal_predictors()) %>%    
  step_dummy(all_nominal_predictors()) %>%  
  step_zv(all_predictors()) %>%                 
  step_lincomb(all_predictors()) %>%           
  step_scale(all_predictors()) %>%
  step_center(all_predictors())

prep_rec <- prep(final_recipe, training = train_splited)
X_train  <- bake(prep_rec, new_data = train_splited)
X_test   <- bake(prep_rec, new_data = test_splited)

alias_name<- alias(lm(log_sale_price ~ ., data = X_train))
#this return null because some column coef's are NA
names(alias_name$Complete)

model <- lm(log_sale_price ~ ., data= X_train)

bad_cols <- names(coef(model))[is.na(coef(model))]
bad_cols

X_train_clean <- X_train[, !colnames(X_train) %in% bad_cols]
X_test_clean  <- X_test[,  !colnames(X_test)  %in% bad_cols]
vif_vals <- vif(lm(log_sale_price ~ ., data = X_train_clean))

high_vif <- names(vif_vals[vif_vals > 10])

X_train_reduced <- X_train_clean %>% select(-all_of(high_vif))
X_test_reduced <- X_test_clean %>% select(-all_of(high_vif))

lin_mod  <- lm(log_sale_price ~ ., data = X_train_reduced)
lin_pred <- predict(lin_mod, newdata = X_test_reduced)

met <- tibble(truth = test_splited$log_sale_price, estimate = lin_pred)
print(paste("Linear RMSE:", round(rmse(met, truth, estimate)$.estimate, 4)))
print(paste("Linear R2  :", round(rsq(met, truth, estimate)$.estimate, 4)))
#------------------------------------------------------------------------------------------------------------------
#install.packages("glmnet")
library(glmnet)

data_folds_5 <- vfold_cv(train_splited, v = 5, strata = log_sale_price)

lasso_model <- linear_reg(penalty = tune(), mixture = 1) %>% set_engine("glmnet")

lasso_param <- grid_regular(penalty(range = c(-4, 1), trans = log_trans()), levels = 50)

lasso_workflow <- workflow() %>%
  add_model(lasso_model) %>%
  add_recipe(final_recipe)

lasso_grid <- tune_grid(
  lasso_workflow,
  resamples = data_folds_5,
  grid = lasso_param,
  metrics = metric_set(rmse) ) #problem comes from rsq

best_lasso <- select_best(lasso_grid, metric = "rmse")
lasso_final_fit <- finalize_model(lasso_model, best_lasso)
lasso_fit <- fit(lasso_final_fit, log_sale_price ~ ., data = X_train_reduced)

lasso_pred <- predict(lasso_fit, new_data = X_test_reduced) %>% pull(.pred)
#---------------------------------------------------------------------------------------------------------------------
#install.packages("xgboost")
library(xgboost)

data_folds_3 <- vfold_cv(train_splited, v = 3, strata = log_sale_price)

xgb_model <- boost_tree(
  mode = "regression",
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
) %>%
  set_engine("xgboost")

xgb_grid_param <- grid_regular(
  trees(range = c(100, 200)),
  tree_depth(range = c(3, 5)),
  learn_rate(range = c(0.05, 0.1)))

xgb_workflow <- workflow() %>%
  add_model(xgb_model) %>%
  add_recipe(final_recipe)

xgb_grid <- tune_grid(
  xgb_workflow,
  resamples = data_folds_3,
  grid = xgb_grid_param,
  metrics = metric_set(rmse, rsq))

best_xgb <- select_best(xgb_grid, metric = "rmse")
xgb_final_fit <- finalize_model(xgb_model, best_xgb)
xgb_fit <- fit(xgb_final_fit, log_sale_price ~ ., data = X_train_reduced)
xgb_pred <- predict(xgb_fit, new_data = X_test_reduced) %>% pull(.pred)
#---------------------------------------------------------------------------------------------------------------------
#install.packages("ranger")
library(ranger)

rf_model <- rand_forest(
  mode="regression",
  trees = tune(),
  min_n = tune(),
) %>%
  set_engine("ranger") #or randomForest

rf_grid_param <- grid_regular(
  trees(range = c(100,200)),
  min_n(range = c(1,5)))
  
rf_workflow <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(final_recipe)

rf_grid <- tune_grid(
  rf_workflow,
  resamples = data_folds_3,
  grid = rf_grid_param,
  metrics = metric_set(rmse,rsq))

best_rf <-select_best(rf_grid, metric = "rmse")
rf_final_fit <- finalize_model(rf_model, best_rf)
rf_fit <- fit(rf_final_fit,log_sale_price ~ ., data = X_train_reduced)
rf_pred <- predict(rf_fit, new_data = X_test_reduced) %>% pull(.pred)
#--------------------------------------------------------------------------------------------------------------------
library(gbm)

gb_model <- gbm(
  data=X_train_reduced,
  formula = log_sale_price ~ .,
  distribution  = "gaussian", #mode
  n.trees = 100, #trees
  interaction.depth =3, #tree_depth
  shrinkage  = 0.05, #learn_rate
  bag.fraction = 0.8, #sample_size
  cv.folds = 3
  )

#gb_grid_param <- grid_regular(
#  n.trees(range= c(100,200)),
#  interaction.depth(range= c(3, 5)),
#  shrinkage(range=c(0.05, 0.1)),
#  bag.fraction(range = c(0.8, 1)))

#gb_workflow <- workflow() %>%
#  add_model(gb_model) %>%
#  add_recipe(final_recipe)

#gb_grid <- tune_grid(
#  gb_workflow,
#  resamples = data_folds_3,
#  grid= gb_grid_param,
#  metrics = metric_set(rmse,rsq))

#best_gb <- select_best(gb_grid, metric = "rmse")
#gb_final_fit <-finalize_model(gb_model,best_gb)
#gb_fit <- fit(gb_final_fit,log_sale_price ~ ., data = X_train_reduced)

best_iter <- gbm.perf(gb_model, method = "cv")
gb_pred <- predict(gb_model, newdata=X_test_reduced, n.trees = best_iter)
#-------------------------------------------------------------------------------------------------------------------
y_test <- test_splited$log_sale_price

all_predictions <- tibble(
  model = c("LinReg", "Lasso", "RF", "GBR", "XGBR"),
  pred = list(lin_pred, lasso_pred, rf_pred, gb_pred,xgb_pred),
  truth = list(y_test, y_test, y_test, y_test, y_test)
)

all_predictions

model_metrics <- all_predictions %>%
  mutate(
    RMSE  = map2_dbl(truth, pred, ~ rmse_vec(truth = .x, estimate = .y)),
    R2    = map2_dbl(truth, pred, ~ rsq_vec(truth = .x, estimate = .y))
  ) %>%
  select(model, RMSE, R2)

final_metrics <- model_metrics %>%
  mutate(across(c(RMSE, R2), ~ round(., 4)))

print(final_metrics)
#-------------------------------------------------------------------------------------------------------------------
test_fixed$Id <- test_ids

external_baked <- bake(prep_rec, new_data = test_fixed)

rf_pred_test <-predict(rf_fit,new_data = external_baked)

test_norm <- expm1(rf_pred_test)

SalePrice_pred <- data.frame(Id = test_data$Id, SalePrice = test_norm)

write.csv(SalePrice_pred, "SalePrice.csv", row.names = FALSE)
