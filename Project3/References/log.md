# SVM:

```R
> # SVM
> svmFit <- cases_train %>% train(risk ~ . - county_name - state,
+                                 method = "svmLinear",
+                                 data = .,
+                                 tuneLength = 15,
+                                 trControl = trainControl(method = "cv", indexOut = train_index))
> svmFit
Support Vector Machines with Linear Kernel 

2501 samples
  10 predictor
   3 classes: 'high', 'low', 'medium' 

No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 2250, 2251, 2251, 2252, 2251, 2251, ... 
Resampling results:

  Accuracy   Kappa    
  0.5373164  0.1797613

Tuning parameter 'C' was held constant at a value of 1
> svmFit$finalModel
Support Vector Machine object of class "ksvm" 

SV type: C-svc  (classification) 
 parameter : cost C = 1 

Linear (vanilla) kernel function. 

Number of Support Vectors : 2345 

Objective Function Value : -744 -744 -1800.436 
Training error : 0.465014 
```

C: It is the regularization parameter, C, of the error term.

# Xgboost:



```R
> # xgboost
> xgboostFit <- cases_train %>% train(risk ~ . - county_name - state,
+                                     method = "xgbTree",
+                                     data = .,
+                                     tuneLength = 5,
+                                     trControl = trainControl(method = "cv", indexOut = train_index),
+                                     tuneGrid = expand.grid(
+                                       nrounds = 20,
+                                       max_depth = 3,
+                                       colsample_bytree = .6,
+                                       eta = 0.1,
+                                       gamma=0,
+                                       min_child_weight = 1,
+                                       subsample = .5
+                                     ))
> xgboostFit
eXtreme Gradient Boosting 

2501 samples
  10 predictor
   3 classes: 'high', 'low', 'medium' 

No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 2252, 2251, 2251, 2250, 2251, 2250, ... 
Resampling results:

  Accuracy   Kappa    
  0.5805057  0.2587593

Tuning parameter 'nrounds' was held constant at a value of 20
Tuning parameter 'max_depth' was held constant at a value of

Tuning parameter 'colsample_bytree' was held constant at a value of 0.6
Tuning parameter 'min_child_weight' was held constant
 at a value of 1
Tuning parameter 'subsample' was held constant at a value of 0.5
> xgboostFit$finalModel
##### xgb.Booster
raw: 71.3 Kb 
call:
  xgboost::xgb.train(params = list(eta = param$eta, max_depth = param$max_depth, 
    gamma = param$gamma, colsample_bytree = param$colsample_bytree, 
    min_child_weight = param$min_child_weight, subsample = param$subsample), 
    data = x, nrounds = param$nrounds, num_class = length(lev), 
    objective = "multi:softprob")
params (as set within xgb.train):
  eta = "0.1", max_depth = "3", gamma = "0", colsample_bytree = "0.6", min_child_weight = "1", subsample = "0.5", num_class = "3", objective = "multi:softprob", validate_parameters = "TRUE"
xgb.attributes:
  niter
callbacks:
  cb.print.evaluation(period = print_every_n)
# of features: 8 
niter: 20
nfeatures : 8 
xNames : nonfamily_households female_pop median_age white_pop black_pop hispanic_pop amerindian_pop employed_pop 
problemType : Classification 
tuneValue :
	  nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
1      20         3 0.1     0              0.6                1       0.5
obsLevels : high low medium 
param :
	list()
```





# ANN

```R
> # ANN
> nnetFit <- cases_train %>% train(risk ~ . - county_name - state,
+                                  method = "nnet",
+                                  data = .,
+                                  tuneLength = 5,
+                                  trControl = trainControl(method = "cv", indexOut = train_index),
+                                  trace = FALSE)
> nnetFit
Neural Network 

2501 samples
  10 predictor
   3 classes: 'high', 'low', 'medium' 

No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 2250, 2251, 2250, 2252, 2252, 2249, ... 
Resampling results across tuning parameters:

  size  decay  Accuracy   Kappa     
  1     0e+00  0.4638404  0.01480140
  1     1e-04  0.4649908  0.01785181
  1     1e-03  0.4701669  0.02568520
  1     1e-02  0.5069546  0.11366452
  1     1e-01  0.5141402  0.13565105
  3     0e+00  0.4954231  0.08795482
  3     1e-04  0.5269530  0.15134727
  3     1e-03  0.4913511  0.07550700
  3     1e-02  0.5313803  0.16739861
  3     1e-01  0.5321372  0.17270314
  5     0e+00  0.5054184  0.10863559
  5     1e-04  0.5325278  0.16399601
  5     1e-03  0.5261963  0.15172670
  5     1e-02  0.5537262  0.21386715
  5     1e-01  0.5477182  0.20129995
  7     0e+00  0.5258170  0.15463392
  7     1e-04  0.5306154  0.16210579
  7     1e-03  0.5177865  0.13599497
  7     1e-02  0.5513294  0.20931384
  7     1e-01  0.5453197  0.19750687
  9     0e+00  0.5529215  0.21133518
  9     1e-04  0.5525102  0.21038726
  9     1e-03  0.5181978  0.13459996
  9     1e-02  0.5545086  0.21533268
  9     1e-01  0.5481086  0.20143306

Accuracy was used to select the optimal model using the largest value.
The final values used for the model were size = 9 and decay = 0.01.
> nnetFit$finalModel
a 8-9-3 network with 111 weights
inputs: nonfamily_households female_pop median_age white_pop black_pop hispanic_pop amerindian_pop employed_pop 
output(s): .outcome 
options were - softmax modelling  decay=0.01
```

Fixing the learning rate at 0.01 and not using momentum, we would expect that a very small learning rate decay would be preferred, as a large learning rate decay would rapidly result in a learning rate that is too small for the model to learn effectively.



# SVM-Predict



```R
> # SVM
> cases_test$risk_predicted <- predict(svmFit, cases_test)
> counties_test <- counties %>% left_join(cases_test %>% 
+                                           mutate(county = county_name %>% str_to_lower() %>% 
+                                                    str_replace('\\s+county\\s*$', '')))
Joining, by = c("state", "county")
> ggplot(counties_test, aes(long, lat)) + 
+   geom_polygon(aes(group = group, fill = risk), color = "black", size = 0.1) + 
+   coord_quickmap() + 
+   scale_fill_manual(values = c('high' = 'red', 'medium' = 'orange','low' = 'blue'))
> cases_test <- cases_test %>% mutate_if(is.character,factor)
> confusionMatrix(data = cases_test$risk_predicted, ref = cases_test$risk)
Confusion Matrix and Statistics

          Reference
Prediction high low medium
    high      0   0      0
    low       5 118     85
    medium   88 127    201

Overall Statistics
                                          
               Accuracy : 0.5112          
                 95% CI : (0.4712, 0.5511)
    No Information Rate : 0.4583          
    P-Value [Acc > NIR] : 0.004571        
                                          
                  Kappa : 0.1327          
                                          
 Mcnemar's Test P-Value : < 2.2e-16       

Statistics by Class:

                     Class: high Class: low Class: medium
Sensitivity                0.000     0.4816        0.7028
Specificity                1.000     0.7625        0.3639
Pos Pred Value               NaN     0.5673        0.4832
Neg Pred Value             0.851     0.6947        0.5913
Prevalence                 0.149     0.3926        0.4583
Detection Rate             0.000     0.1891        0.3221
Detection Prevalence       0.000     0.3333        0.6667
Balanced Accuracy          0.500     0.6221        0.5334
```

# XGBOOST-Predict

```R
> # xgBoost
> cases_test$risk_predicted <- predict(xgboostFit, cases_test)
> counties_test <- counties %>% left_join(cases_test %>% 
+                                           mutate(county = county_name %>% str_to_lower() %>% 
+                                                    str_replace('\\s+county\\s*$', '')))
Joining, by = c("state", "county")
> ggplot(counties_test, aes(long, lat)) + 
+   geom_polygon(aes(group = group, fill = risk), color = "black", size = 0.1) + 
+   coord_quickmap() + 
+   scale_fill_manual(values = c('high' = 'red', 'medium' = 'orange','low' = 'blue'))
> cases_test <- cases_test %>% mutate_if(is.character,factor)
> confusionMatrix(data = cases_test$risk_predicted, ref = cases_test$risk)
Confusion Matrix and Statistics

          Reference
Prediction high low medium
    high      0   0      0
    low       9 126     85
    medium   84 119    201

Overall Statistics
                                         
               Accuracy : 0.524          
                 95% CI : (0.484, 0.5638)
    No Information Rate : 0.4583         
    P-Value [Acc > NIR] : 0.0005823      
                                         
                  Kappa : 0.1573         
                                         
 Mcnemar's Test P-Value : < 2.2e-16      

Statistics by Class:

                     Class: high Class: low Class: medium
Sensitivity                0.000     0.5143        0.7028
Specificity                1.000     0.7520        0.3994
Pos Pred Value               NaN     0.5727        0.4975
Neg Pred Value             0.851     0.7054        0.6136
Prevalence                 0.149     0.3926        0.4583
Detection Rate             0.000     0.2019        0.3221
Detection Prevalence       0.000     0.3526        0.6474
Balanced Accuracy          0.500     0.6331        0.5511
```

# ANN-Predict

```R
> # NN
> cases_test$risk_predicted <- predict(nnetFit, cases_test)
> counties_test <- counties %>% left_join(cases_test %>% 
+                                           mutate(county = county_name %>% str_to_lower() %>% 
+                                                    str_replace('\\s+county\\s*$', '')))
Joining, by = c("state", "county")
> ggplot(counties_test, aes(long, lat)) + 
+   geom_polygon(aes(group = group, fill = risk), color = "black", size = 0.1) + 
+   coord_quickmap() + 
+   scale_fill_manual(values = c('high' = 'red', 'medium' = 'orange','low' = 'blue'))
> cases_test <- cases_test %>% mutate_if(is.character,factor)
> confusionMatrix(data = cases_test$risk_predicted, ref = cases_test$risk)
Confusion Matrix and Statistics

          Reference
Prediction high low medium
    high      0   0      0
    low      12 124     78
    medium   81 121    208

Overall Statistics
                                         
               Accuracy : 0.5321         
                 95% CI : (0.492, 0.5718)
    No Information Rate : 0.4583         
    P-Value [Acc > NIR] : 0.0001322      
                                         
                  Kappa : 0.1706         
                                         
 Mcnemar's Test P-Value : < 2.2e-16      

Statistics by Class:

                     Class: high Class: low Class: medium
Sensitivity                0.000     0.5061        0.7273
Specificity                1.000     0.7625        0.4024
Pos Pred Value               NaN     0.5794        0.5073
Neg Pred Value             0.851     0.7049        0.6355
Prevalence                 0.149     0.3926        0.4583
Detection Rate             0.000     0.1987        0.3333
Detection Prevalence       0.000     0.3429        0.6571
Balanced Accuracy          0.500     0.6343        0.5648
> confusionMatrix(data = cases_test$risk_predicted, ref = cases_test$risk)
```

