#Load the necessary libraries
library(e1071)
library(caret)
library(pROC)

#Read data
data=read.csv("/Users/kk/Desktop/Rstudio/DataMiningMachineLearning/dmml/group_3.csv")

#Develop success variables (1=successful, 0=unsuccessful)
data$success=ifelse(data$gross>(2*data$budget)&data$imdb_score>7,1,0)

#Standardised processing
data$gross=scale(data$gross)
data$budget=scale(data$budget)
data$imdb_score=scale(data$imdb_score)
#Setting the normalisation function
normalize=function(x){
  return((x-min(x))/(max(x)-min(x)))
}
#Normalisation
data$gross=normalize(data$gross)
data$budget=normalize(data$budget)
data$imdb_score=normalize(data$imdb_score)
#Save normalised data
write.csv(data,"processed_data.csv",row.names=FALSE)

#Read the normalised data
processed_data=read.csv("processed_data.csv")

#Setting random seeds to ensure reproducible results
set.seed(123)

#Divide the dataset into a training set and a test set (80% training set, 20% test set)
train_index=sample(1:nrow(processed_data),0.8*nrow(processed_data))
train_data=processed_data[train_index,]
test_data=processed_data[-train_index,]

#Selection of features for SVM training
train_features=train_data[,c("budget","gross","imdb_score")]
test_features=test_data[,c("budget","gross","imdb_score")]
train_features$success=as.factor(train_data$success)
test_features$success=as.factor(test_data$success)

#Define the `success` tag
train_labels=train_data$success
test_labels=test_data$success

#Make sure `success` is of type factor
train_labels=as.factor(train_labels)
test_labels=as.factor(test_labels)

#Cross-validation to select the best cost & gamma
tune_linear=tune.svm(success~.,data=train_features,kernel="linear",
                     cost=c(0.1, 1, 10, 100))
tune_rbf=tune.svm(success~.,data=train_features,kernel="radial",
                  cost=c(0.1,1,10,100),gamma=c(0.01,0.1,1,5,10))

#Getting the optimal parameters
best_cost_linear=tune_linear$best.parameters$cost
best_cost_rbf=tune_rbf$best.parameters$cost
best_gamma_rbf=tune_rbf$best.parameters$gamma
print(paste("The best Linear SVM cost:",best_cost_linear))
print(paste("The best RBF SVM cost:",best_cost_rbf,
            "The best gamma:",best_gamma_rbf))

#Training SVM Models
svm_linear=svm(success~.,data=train_features,kernel="linear",
               cost=best_cost_linear,scale=FALSE)
svm_rbf=svm(success~.,data=train_features,kernel="radial",
            cost=best_cost_rbf,gamma=best_gamma_rbf,scale=FALSE)

#Linear SVM Prediction
predictions_linear=predict(svm_linear,test_features,decision.values=TRUE)
svm_probs_linear=attributes(predictions_linear)$decision.values

#RBF SVM Prediction
predictions_rbf=predict(svm_rbf,test_features,decision.values=TRUE)
svm_probs_rbf=attributes(predictions_rbf)$decision.values

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
