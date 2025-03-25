data<- read.csv("/Users/yueyuebear/Desktop/DMML/group_3.csv")
# 标准化处理
data$gross <- scale(data$gross)
data$budget <- scale(data$budget)
data$imdb_score <- scale(data$imdb_score)
# 归一化处理函数
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# 归一化处理
data$gross <- normalize(data$gross)
data$budget <- normalize(data$budget)
data$imdb_score <- normalize(data$imdb_score)
# 保存标准化/归一化后的数据
write.csv(data, "processed_data.csv", row.names = FALSE)

processed_data<-read.csv("processed_data.csv")
# 设置随机种子以确保结果可重复
set.seed(123)
# 将数据集分为训练集和测试集（80%训练集，20%测试集）
train_index <- sample(1:nrow(processed_data), 0.8 * nrow(processed_data))
train_data <- processed_data[train_index, ]
test_data <- processed_data[-train_index, ]
