#carregando arquivo
myData <- read.csv(file="../data/breast-cancer-wisconsin.data", header=FALSE, sep=",")

#retira a coluna que tem o identificador e a classifica??o
myData.active <- myData[, 2:10]