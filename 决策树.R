library(rpart)
# 使用决策树训练模型
dt_model <- rpart(success ~ gross + budget + imdb_score, data = train_data, method = "class")
# 查看决策树的详细信息，选nsplit=7
printcp(dt_model)
# 可视化决策树
plot(dt_model)
text(dt_model, use.n = TRUE, all = TRUE, cex = 0.8)
# 使用决策树模型进行预测
dt_predictions <- predict(dt_model, newdata = test_data, type = "class")
# 计算决策树的准确率
dt_accuracy <- sum(dt_predictions == test_data$success) / nrow(test_data)
print(paste("决策树模型准确度: ", dt_accuracy))
