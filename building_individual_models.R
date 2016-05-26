###### loading court data ######
courts <- read.csv("ATP\\court_data.csv")


###### games data ###########3
alldata <- read.csv("ATP\\dataset_20160520.csv",header=TRUE, sep = ",")



###adding feautres ####

alldata$matches_30_comparison <- alldata$matches_player_30_days / alldata$matches_opponents_30_days
alldata$matches_60_comparison <- alldata$matches_player_60_days / alldata$matches_opponents_60_days
alldata$matches_90_comparison <- alldata$matches_player_90_days / alldata$matches_opponents_90_days
alldata$percentage_won_30_comparison <- alldata$percentage_won_player_30 - alldata$percentage_won_30
alldata$percentage_won_60_comparison <- alldata$percentage_won_player_60  - alldata$percentage_won_60
alldata$percentage_won_90_comparison <- alldata$percentage_won_player_90  - alldata$percentage_won_90
alldata$sets_times_days_since_previous_match <- alldata$sets_previous_match * alldata$days_since_previous_match



### Clean NA values
for(i in 1:ncol(alldata)){
  if(is.numeric(alldata[,i])){
    alldata[is.na(alldata[,i]),i] = -1
  }else{
    alldata[,i] = as.character(alldata[,i])
    alldata[is.na(alldata[,i]),i] = "NAvalue"
    alldata[,i] = as.factor(alldata[,i])
  }
}

### Clean INF values
for(i in 1:ncol(alldata)){
  if(is.numeric(alldata[,i])){
    alldata[is.infinite(alldata[,i]),i] = -1
  }else{
    alldata[,i] = as.character(alldata[,i])
    alldata[is.infinite(alldata[,i]),i] = "NAvalue"
    alldata[,i] = as.factor(alldata[,i])
  }
}



####### Adding info on courts to dataset #######
require(sqldf)
alldata <- sqldf('SELECT * from alldata JOIN courts ON alldata.ID_T_O = courts.ID_T')



####### building individual models ###########
player_id <- 11953

playerdata <- subset(alldata,alldata$player == player_id)

playerdata$court <- as.character(playerdata$ID_C_T)

#creating RF model:



require(randomForest)

rf1 = randomForest(as.factor(playerdata$win)~., data= playerdata[,match(c(
						"simpleodds" 
						,"matches_opponents_30_days"
						,"percentage_won_30"
						,"matches_opponents_60_days"
						,"percentage_won_60"
						,"matches_opponents_90_days"
						,"percentage_won_90"
						,"matches_opponents_120_days"
						,"percentage_won_120"
						,"matches_opponents_180_days"
						,"percentage_won_180"
						,"matches_player_30_days"
						,"percentage_won_player_30"
						,"matches_player_60_days"
						,"percentage_won_player_60"
						,"matches_player_90_days"
						,"percentage_won_player_90"
						,"matches_player_120_days"
						,"percentage_won_player_120"
						,"matches_player_180_days"
						,"percentage_won_player_180"
						,"percentage_player_won_30_vs_60"
						,"percentage_player_won_30_vs_90"
						,"percentage_player_won_60_vs_90"
						,"percentage_opponent_won_30_vs_60"
						,"percentage_opponent_won_30_vs_90"
						,"percentage_opponent_won_60_vs_90"
						,"evolution_won_30_vs_60_player_vs_opponent"
						,"evolution_won_30_vs_90_player_vs_opponent"
						,"evolution_won_60_vs_90_player_vs_opponent"
						,"sets_previous_match"
						,"days_since_previous_match"
						,"matches_30_comparison"
						,"matches_60_comparison"
						,"matches_90_comparison"
						,"percentage_won_30_comparison"
						,"percentage_won_60_comparison"
						,"percentage_won_90_comparison"
						,"sets_times_days_since_previous_match"
						,"court"
						,"K1"
						,"K2"), names(playerdata))], ntree = 10000, do.trace = 1)

# oob 2.52%




predict_data <- read.csv('ATP\\predict_data.csv',header=TRUE)

predict_data <- sqldf('SELECT * from predict_data JOIN courts ON predict_data.ID_T_O = courts.ID_T')


predict_data$court <- as.character(predict_data $ID_C_T)


predict(rf1, predict_data,type='prob')	



############ Individual SVM ###################3
require(e1071)


svm_train <- playerdata[,match(c(   "win"
						,"simpleodds" 
						,"matches_opponents_30_days"
						,"percentage_won_30"
						,"matches_opponents_60_days"
						,"percentage_won_60"
						,"matches_opponents_90_days"
						,"percentage_won_90"
						,"matches_opponents_120_days"
						,"percentage_won_120"
						,"matches_opponents_180_days"
						,"percentage_won_180"
						,"matches_player_30_days"
						,"percentage_won_player_30"
						,"matches_player_60_days"
						,"percentage_won_player_60"
						,"matches_player_90_days"
						,"percentage_won_player_90"
						,"matches_player_120_days"
						,"percentage_won_player_120"
						,"matches_player_180_days"
						,"percentage_won_player_180"
						,"percentage_player_won_30_vs_60"
						,"percentage_player_won_30_vs_90"
						,"percentage_player_won_60_vs_90"
						,"percentage_opponent_won_30_vs_60"
						,"percentage_opponent_won_30_vs_90"
						,"percentage_opponent_won_60_vs_90"
						,"evolution_won_30_vs_60_player_vs_opponent"
						,"evolution_won_30_vs_90_player_vs_opponent"
						,"evolution_won_60_vs_90_player_vs_opponent"
						,"sets_previous_match"
						,"days_since_previous_match"
						,"matches_30_comparison"
						,"matches_60_comparison"
						,"matches_90_comparison"
						,"percentage_won_30_comparison"
						,"percentage_won_60_comparison"
						,"percentage_won_90_comparison"
						,"sets_times_days_since_previous_match"
						,"K1"
						,"K2"
						,"ID_R_O"
						), names(playerdata))]


 obj <- tune(svm, kernel = 'linear', win~., data = svm_train, 
              ranges = list(gamma = 2^(-1:1), cost = 2^(2:4), epsilon = seq(0.01,0.31, by = 0.05) ),
		  
              #tunecontrol = tune.control(sampling = "fix")
             )





############ Individual Xgboost model ###################3




##### Xgboost :
require(xgboost)

playerdata$court <- as.numeric(playerdata$ID_C_T)

xgb_train <- playerdata[,match(c(
						"simpleodds" 
						,"matches_opponents_30_days"
						,"percentage_won_30"
						,"matches_opponents_60_days"
						,"percentage_won_60"
						,"matches_opponents_90_days"
						,"percentage_won_90"
						,"matches_opponents_120_days"
						,"percentage_won_120"
						,"matches_opponents_180_days"
						,"percentage_won_180"
						,"matches_player_30_days"
						,"percentage_won_player_30"
						,"matches_player_60_days"
						,"percentage_won_player_60"
						,"matches_player_90_days"
						,"percentage_won_player_90"
						,"matches_player_120_days"
						,"percentage_won_player_120"
						,"matches_player_180_days"
						,"percentage_won_player_180"
						,"percentage_player_won_30_vs_60"
						,"percentage_player_won_30_vs_90"
						,"percentage_player_won_60_vs_90"
						,"percentage_opponent_won_30_vs_60"
						,"percentage_opponent_won_30_vs_90"
						,"percentage_opponent_won_60_vs_90"
						,"evolution_won_30_vs_60_player_vs_opponent"
						,"evolution_won_30_vs_90_player_vs_opponent"
						,"evolution_won_60_vs_90_player_vs_opponent"
						,"sets_previous_match"
						,"days_since_previous_match"
						,"matches_30_comparison"
						,"matches_60_comparison"
						,"matches_90_comparison"
						,"percentage_won_30_comparison"
						,"percentage_won_60_comparison"
						,"percentage_won_90_comparison"
						,"sets_times_days_since_previous_match"
						,"K1"
						,"K2"
						,"ID_R_O"
						,"court"), names(playerdata))]




param <- list("objective" = "binary:logistic",  #we're doing classification

              "eval_metric" = "auc")  #error = Accuracy

         

cv.nround <- 5000  #how many trees we want to generate

cv.nfold <- 3 #how many folds in cross-validation



y <- as.integer(playerdata$win)  #XGBOOST doesn't work with factor variables

 

 

xgb_cv_1 = xgb.cv(param=param, data = as.matrix(xgb_train), label = y,

                 nrounds = cv.nround, eta=0.03, max.delta.step = 2, nfold= cv.nfold)  


xgb_1 = xgboost(param=param, data = as.matrix(xgb_train), label = y,

                 nrounds = cv.nround, eta=0.03, max.delta.step = 2)  




predict_data$court <- as.numeric(predict_data $ID_C_T)


xgb_predict <- predict_data[,match(c(
						"simpleodds" 
						,"matches_opponents_30_days"
						,"percentage_won_30"
						,"matches_opponents_60_days"
						,"percentage_won_60"
						,"matches_opponents_90_days"
						,"percentage_won_90"
						,"matches_opponents_120_days"
						,"percentage_won_120"
						,"matches_opponents_180_days"
						,"percentage_won_180"
						,"matches_player_30_days"
						,"percentage_won_player_30"
						,"matches_player_60_days"
						,"percentage_won_player_60"
						,"matches_player_90_days"
						,"percentage_won_player_90"
						,"matches_player_120_days"
						,"percentage_won_player_120"
						,"matches_player_180_days"
						,"percentage_won_player_180"
						,"percentage_player_won_30_vs_60"
						,"percentage_player_won_30_vs_90"
						,"percentage_player_won_60_vs_90"
						,"percentage_opponent_won_30_vs_60"
						,"percentage_opponent_won_30_vs_90"
						,"percentage_opponent_won_60_vs_90"
						,"evolution_won_30_vs_60_player_vs_opponent"
						,"evolution_won_30_vs_90_player_vs_opponent"
						,"evolution_won_60_vs_90_player_vs_opponent"
						,"sets_previous_match"
						,"days_since_previous_match"
						,"matches_30_comparison"
						,"matches_60_comparison"
						,"matches_90_comparison"
						,"percentage_won_30_comparison"
						,"percentage_won_60_comparison"
						,"percentage_won_90_comparison"
						,"sets_times_days_since_previous_match"
						,"K1"
						,"K2"
						,"ID_R_O"
						,"court"), names(predict_data))]







pred_xgb_1 <- predict(xgb_1,as.matrix(xgb_predict))


pred_xgb_1






