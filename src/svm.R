#carregando pacotes
install.packages("e1071")
library("e1071")

#carregando arquivo
myData <- read.csv(file="C:/temp/sin5007/data/breast-cancer-wisconsin-with-class-names.data", header=FALSE, sep=",")

#retira a coluna que tem o identificador
myData.withoutIdentifier <- myData[, 2:11]

#retira a coluna que tem o identificador e a classificação
myData.withoutIdentifierAndClassification <- myData[, 2:10]

#apenas a coluna de classificação
myData.classification <- myData[, 11:11]

#constrói o modelo
model <- svm(myData.withoutIdentifierAndClassification, myData.classification)

summary(model)

#aplica o modelo
pred <- predict(model, myData.withoutIdentifierAndClassification)

resultado <- table(pred, myData.classification)

tp <- resultado[1,1]
fp <- resultado[1,2]
tn <- resultado[2,2]
fn <- resultado[2,1]

precisao <- tp/(tp+fp)
sensibilidade <- tp/(tp+fn)
errototal <- (fn+fp)/(tp+fp+tn+fn)