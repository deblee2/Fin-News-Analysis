#RepRisk 추출
data <- read.csv(file="C:\\Users\\user\\Documents\\RepRisk.csv", header=TRUE)
head(data)
# RepRisk 데이터 개수
nrow(data)
str(data)

library(dplyr)

# 업종별 고유한 값 
industry_list <- distinct(data, sectors)
nrow(industry_list)

# 회사별 고유한 값
nrow(distinct(data, name))

