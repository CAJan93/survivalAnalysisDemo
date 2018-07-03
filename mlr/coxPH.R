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
list.of.packages <- c("mlr", "survival", "CoxBoost", "party", "KMsurv", "survAUC")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)



# import and process data
data("kidtran") # get data
kidtran = na.omit(kidtran) # remove rows with missing values
kidtran$delta = (kidtran$delta == 1) # convert to logical
n = nrow(kidtran)
kidtran <- kidtran[sample(n),] # shuffle row-wise
kidtran.train = sample(n, size = 0.8*n) 
kidtran.test = setdiff(1:n, kidtran.train)
# kidtran.train = kidtran[1:round(n*0.8),]
# kidtran.test = kidtran[round(n*0.8) + 1 : n,]



# cox proportional hazard
coxph.lrn = makeLearner("surv.coxph") 
coxph.tsk = makeSurvTask(data = kidtran, target = c("time", "delta"))
coxph.mod = train(coxph.lrn, coxph.tsk, subset = kidtran.train) 
coxph.tsk.pred = predict(coxph.mod, task = coxph.tsk, subset = kidtran.test)
# Does this give me how the risk of a person 
# is different from the avg. risk? 
exp(coxph.tsk.pred$data$response)
# possible performance measures
listMeasures(coxph.tsk)
# what does this tell me? 
performance(coxph.tsk.pred, measures = mlr::cindex) # 0.9131075 
performance(coxph.tsk.pred, measure = cindex.uno, model = coxph.mod, task = coxph.tsk) # 0.8663421 
performance(coxph.tsk.pred, measures = ibrier, model = coxph.mod, task = coxph.tsk) # 0.144805 



# cox proportional hazard with Componentwise Likelihood based Boosting
coxboost.lrn = makeLearner("surv.CoxBoost") 
coxboost.tsk = makeSurvTask(data = kidtran, target = c("time", "delta"))
coxboost.mod = train(coxboost.lrn, coxboost.tsk, subset = kidtran.test) 
coxboost.tsk.pred = predict(coxboost.mod, task = coxboost.tsk, subset = kidtran.test)
# Does this give me how the risk of a person 
# is different from the avg. risk? 
exp(coxboost.tsk.pred$data$response)
# possible performance measures
listMeasures(coxboost.tsk)
performance(coxboost.tsk.pred, measures = mlr::cindex) # 0.81958
performance(coxboost.tsk.pred, measures = cindex.uno, model = coxboost.mod, task = coxboost.tsk) # 0.8456482
performance(coxboost.tsk.pred, measures = mlr::ibrier, model = coxboost.mod, task = coxboost.tsk)



# Cox Proportional Hazards Model with Componentwise Likelihood based Boosting,
# tuned for the optimal number of boosting steps
cvcoxboost.lrn = makeLearner("surv.cv.CoxBoost") 
cvcoxboost.tsk = makeSurvTask(data = kidtran, target = c("time", "delta"))
cvcoxboost.mod = train(cvcoxboost.lrn, cvcoxboost.tsk, subset = kidtran.train) 
cvcoxboost.tsk.pred = predict(cvcoxboost.mod, task = cvcoxboost.tsk, subset = kidtran.test)

exp(cvcoxboost.tsk.pred$data$response)
# possible performance measures
listMeasures(cvcoxboost.tsk)

performance(cvcoxboost.tsk.pred, measures = mlr::cindex) # 0.8379178 
performance(cvcoxboost.tsk.pred, measures = cindex.uno, model = cvcoxboost.mod, task = cvcoxboost.tsk) # 0.8461206 
performance(cvcoxboost.tsk.pred, measures = mlr::ibrier, model = cvcoxboost.mod, task = cvcoxboost.tsk)



# Random Forest based on Conditional Inference Trees (with bagging)
cforest.lrn = makeLearner("surv.cforest") 
cforest.tsk = makeSurvTask(data = kidtran, target = c("time", "delta"))
cforest.mod = train(cforest.lrn, cforest.tsk, subset = kidtran.train) 
# will give me -Inf as some results
cforest.tsk.pred = predict(cforest.mod, task = cforest.tsk, subset = kidtran.test)

exp(cforest.tsk.pred$data$response)
# possible performance measures
listMeasures(cforest.tsk)

cforest.tsk.pred$data = cforest.tsk.pred$data[cforest.tsk.pred$data$response != -Inf,]

# performance
performance(cforest.tsk.pred, measures = mlr::cindex)
mlr::performance(cforest.tsk.pred, measures = mlr::cindex, model = cforest.mod, task = cforest.tsk) # 0.8657095
mlr::performance(cforest.tsk.pred, measures = cindex.uno, model = cforest.mod, task = cforest.tsk) # 0.7590004 
mlr::performance(cforest.tsk.pred, measures = ibrier, model = cforest.mod, task = cforest.tsk)

