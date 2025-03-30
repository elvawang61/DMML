library(dplyr)
# 读取原始数据
data <- read.csv("group_3.csv.csv")

# 创建成功标准列--收入是预算的2倍，评分在7以上
data$success <- ifelse(data$gross >= 2 * data$budget & data$imdb_score >= 7, 1, 0)

# 将success设定成因子类型
data$success <- as.factor(data$success)

# 标准化处理
data$gross_scale <- scale(data$gross)
data$budget_scale <- scale(data$budget)
data$imdb_score_scale <- scale(data$imdb_score)

# 归一化处理函数
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# 归一化处理
data$gross_scale <- normalize(data$gross_scale)
data$budget_scale <- normalize(data$budget_scale)
data$imdb_score_scale <- normalize(data$imdb_score_scale)

#将清洗完成的数据保存至processed_data
write.csv(data, "D:\\Glasgow\\DMML\\group_assignment\\processed_data.csv", row.names = FALSE, quote = TRUE)


#重新读入清洗后的数据
processed_data<-read.csv("processed_data.csv")

# 将success设定成因子类型
processed_data$success <- as.factor(processed_data$success)

# 设置随机种子以确保结果可重复
set.seed(123)

#将actor1、2、3的facebook_likes加起来，形成新的一列
processed_data$actor_facebook_likes <- processed_data[, "actor_1_facebook_likes"] + 
                                       processed_data[, "actor_2_facebook_likes"] +
                                       processed_data[, "actor_3_facebook_likes"]

# 将数据集分为训练集和测试集（80%训练集，20%测试集）
train_index <- sample(1:nrow(processed_data), 0.8 * nrow(processed_data))
train_data <- processed_data[train_index, ]
test_data <- processed_data[-train_index, ]


str(processed_data)

