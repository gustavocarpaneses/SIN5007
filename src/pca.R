#carregando arquivo
myData <- read.csv(file="../data/breast-cancer-wisconsin.data", header=FALSE, sep=",")

#retira a coluna que tem o identificador e a classifica??o
myData.withoutIdentifierAndClassification <- myData[, 2:10]

#pca
res.pca <- princomp(myData.withoutIdentifierAndClassification)

screeplot(res.pca)
summary(res.pca)
print(res.pca)