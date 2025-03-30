##Load the necessary libraries
library(e1071)
library(caret)
library(pROC)

##Data cleaning and preparations
data=read.csv("/Users/kk/Desktop/Rstudio/DataMiningMachineLearning/dmml/group_3.csv")
data$success=ifelse(data$gross>(2*data$budget)&data$imdb_score>7,1,0)  #Develop success variables (1=successful, 0=unsuccessful)
data$actor_total_likes=rowSums(data[,c("actor_1_facebook_likes",
                                       "actor_2_facebook_likes",
                                       "actor_3_facebook_likes")],na.rm=TRUE)  #Construct new variable: sum of actor likes
vars=c("gross","budget","imdb_score","duration","director_facebook_likes",
       "actor_total_likes","title_year","success")
data_model=data[,vars]  #Selection of variables for modelling
data_model=na.omit(data_model)  #Remove missing values
numeric_vars=setdiff(names(data_model),"success")
data_model[numeric_vars]=scale(data_model[numeric_vars])  #Standardised numeric variables (excluding success)

##Preparations before modelling
set.seed(123)  #Setting random seeds to ensure reproducible results
train_index=sample(1:nrow(data_model),0.8*nrow(data_model))  #Divide the dataset into a training set and a test set (80% training set, 20% test set)
train_data=data_model[train_index, ]
test_data=data_model[-train_index, ]
train_features=train_data[,-which(names(train_data)=="success")]  #Building features and labels
test_features=test_data[,-which(names(test_data)=="success")]
train_labels=as.factor(train_data$success)
test_labels=as.factor(test_data$success)

##Modelling
#Cross-validation to select the best cost & gamma
tune_linear=tune.svm(success~.,data=train_data,kernel="linear",
                     cost=c(0.1, 1, 10, 100))
tune_rbf=tune.svm(success~.,data=train_data,kernel="radial",
                  cost=c(0.1,1,10,100),gamma=c(0.01,0.1,1,5,10))

#Getting the optimal parameters
best_cost_linear=tune_linear$best.parameters$cost
best_cost_rbf=tune_rbf$best.parameters$cost
best_gamma_rbf=tune_rbf$best.parameters$gamma
print(paste("The best Linear SVM cost:",best_cost_linear))
print(paste("The best RBF SVM cost:",best_cost_rbf,
            "The best gamma:",best_gamma_rbf))
#For the linear SVM, the optimal cost was selected as 100, while for the RBF kernel, the best parameters were cost = 10 and gamma = 0.1.

#Training SVM Models
svm_linear=svm(success~.,data=train_data,kernel="linear",
               cost=best_cost_linear,scale=FALSE)
svm_rbf=svm(success~.,data=train_data,kernel="radial",
            cost=best_cost_rbf,gamma=best_gamma_rbf,scale=FALSE)

#Linear SVM Prediction
predictions_linear=predict(svm_linear,test_features,decision.values=TRUE)
svm_probs_linear=attributes(predictions_linear)$decision.values
#RBF SVM Prediction
predictions_rbf=predict(svm_rbf,test_features,decision.values=TRUE)
svm_probs_rbf=attributes(predictions_rbf)$decision.values

##Model evaluation
#Calculate the optimal classification threshold
roc_curve_linear=roc(test_labels,svm_probs_linear)
roc_curve_rbf=roc(test_labels,svm_probs_rbf)
best_threshold_linear=coords(roc_curve_linear,"best",ret="threshold")
best_threshold_rbf=coords(roc_curve_rbf,"best",ret="threshold")
print(paste("Optimal Linear SVM Classification Threshold.",best_threshold_linear))
print(paste("Optimal RBF SVM Classification Threshold.",best_threshold_rbf))

#Reclassification
predictions_adjusted_linear=as.factor(ifelse(svm_probs_linear>best_threshold_linear$threshold,1,0))
predictions_adjusted_rbf=as.factor(ifelse(svm_probs_rbf>best_threshold_rbf$threshold,1,0))
#Rather than relying on the default classification threshold (e.g. 0.5 or 0), we used the ROC curve to determine the optimal threshold that maximizes classification performance.
#Based on this threshold, predictions were recalibrated to improve the balance between precision and recall.
#This reclassification step led to a more effective and realistic prediction of movie success, especially when handling class imbalance.

#Calculate the confusion matrix
conf_matrix_linear=confusionMatrix(predictions_adjusted_linear,test_labels)
conf_matrix_rbf=confusionMatrix(predictions_adjusted_rbf,test_labels)
print("Linear SVM Evaluation Results:")
print(conf_matrix_linear)
print("RBF SVM Evaluation Results:")
print(conf_matrix_rbf)

#Calculate AUC value
auc_linear=auc(roc_curve_linear)
auc_rbf=auc(roc_curve_rbf)
print(paste("Linear SVM AUC:",auc_linear))
print(paste("RBF SVM AUC:",auc_rbf))

#Calculate F1-score
precision_linear=conf_matrix_linear$byClass["Precision"]
recall_linear=conf_matrix_linear$byClass["Recall"]
f1_score_linear=2*(precision_linear*recall_linear)/(precision_linear+recall_linear)
precision_rbf=conf_matrix_rbf$byClass["Precision"]
recall_rbf=conf_matrix_rbf$byClass["Recall"]
f1_score_rbf=2*(precision_rbf*recall_rbf)/(precision_rbf+recall_rbf)
print(paste("Linear SVM F1-score:",f1_score_linear))
print(paste("RBF SVM F1-score:",f1_score_rbf))

#Plotting ROC curves
ggplot()+
  geom_line(aes(x=1-roc_curve_linear$specificities,y=roc_curve_linear$sensitivities,
                color="Linear SVM"))+
  geom_line(aes(x=1-roc_curve_rbf$specificities,y=roc_curve_rbf$sensitivities,
                color="RBF SVM"))+
  labs(title="ROC Curve: SVM",x="1-Specificity",y="Sensitivity")+
  scale_color_manual(name="Model",
                     values=c("Linear SVM"="blue","RBF SVM"="red"))+
  theme_minimal()
#The ROC curves of both models indicate high overall discriminative ability.