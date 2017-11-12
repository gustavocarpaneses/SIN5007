#carregando pacotes
install.packages("e1071")
library("e1071")

#carregando arquivo
myData <- read.csv(file="C:/temp/sin5007/data/breast-cancer-wisconsin.data", header=FALSE, sep=",")

#retira a coluna que tem o identificador e a classifica??o
myData.active <- myData[, 2:11]

model <- svm(V11~., data = myData.active)

print(model)
summary(model)