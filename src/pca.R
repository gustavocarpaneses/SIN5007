#carregando arquivo
setwd('C:\\Users\\AdrianoDev\\Documents\\Mestrado\\Reconhecimento Padrões\\cancer\\SIN5007')
#myData <- read.csv(file="../data/breast-cancer-wisconsin.data", header=FALSE, sep=",")

myData <- read.csv(file="data/breast-cancer-wisconsin-with-class-names.data", header=FALSE, sep=",")

#retira a coluna que tem o identificador e a classificação
myData.withoutIdentifierAndClassification <- myData[, 2:10]

#pca
res.pca <- princomp(myData.withoutIdentifierAndClassification)

screeplot(res.pca)
summary(res.pca)
print(res.pca)

#Cria um conjunto de dados somente com o PCA Score
arquivo = 'data/breast-cancer-wisconsin-with-class-names-PCA-Score.data'
write.table(cbind.data.frame(myData[,1],res.pca$scores[,1:9], myData[,11]), arquivo, sep=",", quote = FALSE, col.names = FALSE, row.names = FALSE)

