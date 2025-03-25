install.packages("caret")
library(caret)
# 设置训练控制，使用交叉验证
train_control <- trainControl(method = "cv", number = 10)  # 10折交叉验证
# 训练KNN模型并进行交叉验证，选择最佳k值
knn_model_cv <- train(success ~ gross + budget + imdb_score, 
                      data = train_data, 
                      method = "knn", 
                      trControl = train_control, 
                      tuneLength = 20)  # 尝试k值从1到20
# 查看最优的k值
print(knn_model_cv$bestTune)
# 查看交叉验证的结果
print(knn_model_cv)
# 使用训练好的KNN模型进行预测
knn_predictions <- predict(knn_model_cv, newdata = test_data)
# 计算预测准确率
knn_accuracy <- sum(knn_predictions == test_data$success) / nrow(test_data)
print(paste("KNN模型准确度: ", knn_accuracy))

