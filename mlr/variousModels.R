# notation 
#
# algo.lrn      -     a learner of algorithmn "algo"
# algo.mod      -     a fitted model of algorithmn "algo"
# algo.tsk      -     a task of the algorithmn "algo"
# algo.tsk.pred -     a prediction of the ds.test data in algo.tsk, using the algorithmn "algo"
# algo.nd.pred  -     a prediction of new data "nd", using the algorithmn "algo"
#
# ds.train      -     a subset of the dataset "ds", used for training 
# ds.test       -     a subset of the dataset "ds", used for testing

# remove objects in session with rm(list = ls(all = TRUE))

# install and load required packages, if not installed already
list.of.packages <- c("survminer","ggplot2","mlr", "survival", "CoxBoost", "party", "KMsurv", "survAUC")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)



# import and process data
# telco_churn = read.csv("telco_customer_churn.csv")  # read csv file
telco_churn = read.csv("https://community.watsonanalytics.com/wp-content/uploads/2015/03/WA_Fn-UseC_-Telco-Customer-Churn.csv")

telco_churn = na.omit(telco_churn) # remove rows with missing values
colnames(telco_churn)[6] <- "time"
n = nrow(telco_churn)
telco_churn <- telco_churn[sample(n),] # shuffle row-wise
#telco_churn.train = sample(n, size = 0.8*n) 
#telco_churn.test = setdiff(1:n, telco_churn.train)
telco_churn.train_seq = seq(round(n*0.8))
telco_churn.test_seq  = seq(from = round(n*0.8) + 1, to = n)
#telco_churn.train = telco_churn[1:round(telco_churn.train_seq),]
#telco_churn.test = telco_churn[round(n*0.8) + 1 : n,]
telco_churn$Churn <- as.character(telco_churn$Churn)
telco_churn$Churn[telco_churn$Churn == "Yes"] <- T
telco_churn$Churn[telco_churn$Churn == "No"] <- F
telco_churn$Churn <- as.logical(telco_churn$Churn)

telco_churn_cor = telco_churn

# have a look at the correlations between user attributes
#telco_churn_cor = telco_churn[which(telco_churn$Churn == T),]
telco_churn_cor$PhoneService <- as.character(telco_churn_cor$PhoneService)
telco_churn_cor$PhoneService[telco_churn_cor$PhoneService == "Yes"] <- 1
telco_churn_cor$PhoneService[telco_churn_cor$PhoneService == "No"] <- 0
telco_churn_cor$PhoneService <- as.numeric(telco_churn_cor$PhoneService)

telco_churn_cor$MultipleLines <- as.character(telco_churn_cor$MultipleLines)
telco_churn_cor$MultipleLines[telco_churn_cor$MultipleLines == "Yes"] <- 2
telco_churn_cor$MultipleLines[telco_churn_cor$MultipleLines == "No"] <- 1
telco_churn_cor$MultipleLines[telco_churn_cor$MultipleLines == "No phone service"] <- 0
telco_churn_cor$MultipleLines <- as.numeric(telco_churn_cor$MultipleLines)

telco_churn_cor$OnlineSecurity <- as.character(telco_churn_cor$OnlineSecurity)
telco_churn_cor$OnlineSecurity[telco_churn_cor$OnlineSecurity == "Yes"] <- 2
telco_churn_cor$OnlineSecurity[telco_churn_cor$OnlineSecurity == "No"] <- 1
telco_churn_cor$OnlineSecurity[telco_churn_cor$OnlineSecurity == "No internet service"] <- 0
telco_churn_cor$OnlineSecurity <- as.numeric(telco_churn_cor$OnlineSecurity)

telco_churn_cor$OnlineBackup <- as.character(telco_churn_cor$OnlineBackup)
telco_churn_cor$OnlineBackup[telco_churn_cor$OnlineBackup == "Yes"] <- 2
telco_churn_cor$OnlineBackup[telco_churn_cor$OnlineBackup == "No"] <- 1
telco_churn_cor$OnlineBackup[telco_churn_cor$OnlineBackup == "No internet service"] <- 0
telco_churn_cor$OnlineBackup <- as.numeric(telco_churn_cor$OnlineBackup)

telco_churn_cor$DeviceProtection <- as.character(telco_churn_cor$DeviceProtection)
telco_churn_cor$DeviceProtection[telco_churn_cor$DeviceProtection == "Yes"] <- 2
telco_churn_cor$DeviceProtection[telco_churn_cor$DeviceProtection == "No"] <- 1
telco_churn_cor$DeviceProtection[telco_churn_cor$DeviceProtection == "No internet service"] <- 0
telco_churn_cor$DeviceProtection <- as.numeric(telco_churn_cor$DeviceProtection)

telco_churn_cor$StreamingTV <- as.character(telco_churn_cor$StreamingTV)
telco_churn_cor$StreamingTV[telco_churn_cor$StreamingTV == "Yes"] <- 1
telco_churn_cor$StreamingTV[telco_churn_cor$StreamingTV == "No"] <- 0
telco_churn_cor$StreamingTV[telco_churn_cor$StreamingTV == "No internet service"] <- 0
telco_churn_cor$StreamingTV <- as.numeric(telco_churn_cor$StreamingTV)

telco_churn_cor$StreamingMovies <- as.character(telco_churn_cor$StreamingMovies)
telco_churn_cor$StreamingMovies[telco_churn_cor$StreamingMovies == "Yes"] <- 2
telco_churn_cor$StreamingMovies[telco_churn_cor$StreamingMovies == "No"] <- 1
telco_churn_cor$StreamingMovies[telco_churn_cor$StreamingMovies == "No internet service"] <- 0
telco_churn_cor$StreamingMovies <- as.numeric(telco_churn_cor$StreamingMovies)

telco_churn_cor$TechSupport <- as.character(telco_churn_cor$TechSupport)
telco_churn_cor$TechSupport[telco_churn_cor$TechSupport == "Yes"] <- 2
telco_churn_cor$TechSupport[telco_churn_cor$TechSupport == "No"] <- 1
telco_churn_cor$TechSupport[telco_churn_cor$TechSupport == "No internet service"] <- 0
telco_churn_cor$TechSupport <- as.numeric(telco_churn_cor$TechSupport)

telco_churn_cor$InternetService <- as.character(telco_churn_cor$InternetService)
telco_churn_cor$InternetService[telco_churn_cor$InternetService == "Fiber optic"] <- 2
telco_churn_cor$InternetService[telco_churn_cor$InternetService == "DSL"] <- 1
telco_churn_cor$InternetService[telco_churn_cor$InternetService == "No"] <- 0
telco_churn_cor$InternetService <- as.numeric(telco_churn_cor$InternetService)


telco_churn_cor$PaperlessBilling <- as.character(telco_churn_cor$PaperlessBilling)
telco_churn_cor$PaperlessBilling[telco_churn_cor$PaperlessBilling == "Yes"] <- 1
telco_churn_cor$PaperlessBilling[telco_churn_cor$PaperlessBilling == "No"] <- 0
telco_churn_cor$PaperlessBilling <- as.numeric(telco_churn_cor$PaperlessBilling)


telco_churn_cor$Partner <- as.character(telco_churn_cor$Partner)
telco_churn_cor$Partner[telco_churn_cor$Partner == "Yes"] <- 1
telco_churn_cor$Partner[telco_churn_cor$Partner == "No"] <- 0
telco_churn_cor$Partner <- as.numeric(telco_churn_cor$Partner)

telco_churn_cor$Dependents <- as.character(telco_churn_cor$Dependents)
telco_churn_cor$Dependents[telco_churn_cor$Dependents == "Yes"] <- 1
telco_churn_cor$Dependents[telco_churn_cor$Dependents == "No"] <- 0
telco_churn_cor$Dependents <- as.numeric(telco_churn_cor$Dependents)

telco_churn_cor$Contract <- as.character(telco_churn_cor$Contract)
telco_churn_cor$Contract[telco_churn_cor$Contract == "One year"] <- 1
telco_churn_cor$Contract[telco_churn_cor$Contract == "Two year"] <- 2
telco_churn_cor$Contract[telco_churn_cor$Contract == "Month-to-month"] <- 0
telco_churn_cor$Contract <- as.numeric(telco_churn_cor$Contract)


if (T){
  # KM
  library(survminer)
  fit <- survfit(Surv(time, Churn) ~ 1,
                 data = telco_churn_cor)
  # Visualize with survminer
  ggsurvplot(fit, data = telco_churn_cor, risk.table = TRUE, title = "Kaplan-Meier curve", ylim = c(0.50, 1), palette="#C92A2F")
}



# works
cor(telco_churn_cor[, -which(names(telco_churn_cor) %in% c("Churn","customerID","time",  "PaymentMethod", "gender", "Dependents"))], telco_churn_cor["time"], method = "spearman")

telco_churn_cor$Churn <- as.logical(telco_churn_cor$Churn)
if (F) {
  # cox proportional hazard
  coxph.lrn = makeLearner("surv.coxph") 
  coxph.tsk = makeSurvTask(id = "telco_churn_cor", data = telco_churn_cor[, -which(names(telco_churn_cor) %in% c("TotalCharges", "customerID"))], target = c("time", "Churn"))
  coxph.mod = train(coxph.lrn, coxph.tsk, subset = telco_churn.train_seq) 
  coxph.tsk.pred = predict(coxph.mod, task = coxph.tsk, subset = telco_churn.test_seq)
  # possible performance measures
  listMeasures(coxph.tsk)
  # what does this tell me? 
  performance(coxph.tsk.pred, measures = mlr::cindex) # 0.8728485 # 0.8473736 # 0.8716186 # 0.8677882 # 0.8595095 # 0.8572907 # 0.8647923 # 0.8683304 # 0.869991 # 0.8578821 # avg: 0.8637424899999999
  performance(coxph.tsk.pred, measure = cindex.uno, model = coxph.mod, task = coxph.tsk) # 0.8623853 # 0.9268293 # 0.8461538 # 0.9864865 # 0.9418605 # 0.8888889 # 0.99999 # 0.6666667 # 0.7565789 # 0.9345238 # avg: 0.88103637
  performance(coxph.tsk.pred, measures = ibrier, model = coxph.mod, task = coxph.tsk) # 0.178092 # 0.1753829 # 0.1777078 # 0.1769075 # 0.1766885 # 0.1767159 # 0.1753943 # 0.1748194 # 0.1775474 # 0.1753555 # avg: 0.17646112
}


if (F){
  # cox proportional hazard with Componentwise Likelihood based Boosting
  coxboost.lrn = makeLearner("surv.CoxBoost") 
  coxboost.tsk = makeSurvTask(id = "telco_churn_cor" ,data = telco_churn_cor[, -which(names(telco_churn_cor) %in% c("TotalCharges", "customerID"))], target = c("time", "Churn"))
  coxboost.mod = train(coxboost.lrn, coxboost.tsk, subset = telco_churn.train_seq) 
  coxboost.tsk.pred = predict(coxboost.mod, task = coxboost.tsk, subset = telco_churn.test_seq)
  # possible performance measures
  listMeasures(coxboost.tsk)
  performance(coxboost.tsk.pred, measures = mlr::cindex) # 0.8299091 # 0.8282858 # 0.8413414 # 0.8422842 # 0.8534125 # 0.8375552 # 0.8377701 # 0.8264238 # 0.8293538 # 0.8496555
  performance(coxboost.tsk.pred, measures = cindex.uno, model = coxboost.mod, task = coxboost.tsk) # 0.7511111  # 0.79375 # 0.9047619 # 0.969697 # 0.7853982 # 0.76 # 0.7969697 # 0.875 # 0.2038835 # 0.908046
}








if(F){
  # Cox Proportional Hazards Model with Componentwise Likelihood based Boosting,
  # tuned for the optimal number of boosting steps
  cvcoxboost.lrn = makeLearner("surv.cv.CoxBoost") 
  cvcoxboost.tsk = makeSurvTask(id = "telco_churn_cor", data = telco_churn_cor[, -which(names(telco_churn_cor) %in% c("TotalCharges", "customerID"))], target = c("time", "Churn"))
  cvcoxboost.mod = train(cvcoxboost.lrn, cvcoxboost.tsk, subset = telco_churn.train_seq) 
  cvcoxboost.tsk.pred = predict(cvcoxboost.mod, task = cvcoxboost.tsk, subset = telco_churn.test_seq)#telco_churn.test_seq)
  
  # possible performance measures
  listMeasures(cvcoxboost.tsk)
  
  performance(cvcoxboost.tsk.pred, measures = mlr::cindex) # 0.8476182
  performance(cvcoxboost.tsk.pred, measures = cindex.uno, model = cvcoxboost.mod, task = cvcoxboost.tsk) # 0.8255814
}




if (F){
  
  # Random Forest based on Conditional Inference Trees (with bagging)
  cforest.lrn = makeLearner("surv.cforest") 
  cforest.tsk = makeSurvTask(id = "telco_churn_cor", data = telco_churn_cor[, -which(names(telco_churn_cor) %in% c("TotalCharges", "customerID"))], target = c("time", "Churn"))
  cforest.mod = train(cforest.lrn, cforest.tsk, subset = telco_churn.train_seq) 
  # will give me -Inf as some results
  cforest.tsk.pred = predict(cforest.mod, task = cforest.tsk, subset = telco_churn.test_seq)
  
  # possible performance measures
  listMeasures(cforest.tsk)
  
  cforest.tsk.pred$data = cforest.tsk.pred$data[cforest.tsk.pred$data$response != -Inf,]
  
  # performance, excluding -inf
  performance(cforest.tsk.pred, measures = mlr::cindex) # 0.7069276 # 0.725202 # 0.7122673 # 0.7148753 # 0.7055392 # 0.7202865 # 0.7390247 # 0.7069126 # 0.7077237 # 0.6993006 
  mlr::performance(cforest.tsk.pred, measures = cindex.uno, model = cforest.mod, task = cforest.tsk) # 5.832794e-13 # 0.725202 # 8.737003e-15 # 6.371290e-14 # 2.387208e-16 # 1.122545e-13 # 0.8 # 0.75 # 1.535217e-16 # 0.6666667 
  
  mlr::performance(cforest.tsk.pred, measures = ibrier, model = cforest.mod, task = cforest.tsk) 
}