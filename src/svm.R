#carregando pacotes
#install.packages("e1071")
#library("e1071")

#carrega os folds
foldFileName = "C:/temp/sin5007/data/breast-cancer-wisconsin-with-class-names-fold-%d.data"

pcaComponents = 4
k = 5
t = 1

myData <- list()
myDataWithoutIdentifier <- list()
myDataWithoutIdentifierAndClassification <- list()
myPcaDataWithoutIdentifierAndClassification <- list()
myDataClassification <- list()

for(t in 1:k){
  
  #carregando arquivo
  myData[[t]] <- read.csv(file=sprintf(foldFileName, t), header=FALSE, sep=",")
  
  #retira a coluna que tem o identificador
  myDataWithoutIdentifier[[t]] <- myData[[t]][, 2:11]
  
  #retira a coluna que tem o identificador e a classificação
  myDataWithoutIdentifierAndClassification[[t]] <- myData[[t]][, 2:10]
  
  res.pca <- princomp(myDataWithoutIdentifierAndClassification[[t]])
  myPcaDataWithoutIdentifierAndClassification[[t]] <- res.pca$scores[,1:pcaComponents]
  
  #apenas a coluna de classificação
  myDataClassification[[t]] <- myData[[t]][, 11:11]

}

t = 1
tp <- 0
fp <- 0
tn <- 0
fn <- 0

tpPca <- 0
fpPca <- 0
tnPca <- 0
fnPca <- 0

tpRelief <- 0
fpRelief <- 0
tnRelief <- 0
fnRelief <- 0

if(exists("trainingFoldPca")){
  remove("trainingFoldPca")
}

for(t in 1:k){

  tf = 1
  
  trainingFold <- list()
  trainingFoldClassification <- factor()
  
  for(tf in 1:k){
    #combina todos os folds, exceto o atual (que vai ser utilizado para o teste)
    if(tf != t){
      trainingFold <- rbind(trainingFold, myDataWithoutIdentifierAndClassification[[tf]])
      
      if(!exists("trainingFoldPca")){
        trainingFoldPca <- myPcaDataWithoutIdentifierAndClassification[[tf]]
      }else{
        trainingFoldPca <- rbind(trainingFoldPca, myPcaDataWithoutIdentifierAndClassification[[tf]])  
      }
      
      trainingFoldClassification <- factor(c(
        as.character(trainingFoldClassification), 
        as.character(myDataClassification[[tf]])))
    }
    
  }
  
  #constrói o modelo baseado no training Fold (com todas as características)
  model <- svm(trainingFold, trainingFoldClassification)
  
  #constrói o modelo baseado no training Fold (PCA)
  modelPca <- svm(trainingFoldPca, trainingFoldClassification)
  
  #constrói o modelo baseado no training Fold (RELIEF)
  #quando fizemos o relief identificamos que a última coluna
  #é uma característica irrelevante, por isso 1:8
  modelRelief <- svm(trainingFold[,1:8], trainingFoldClassification)
  
  #summary(model)
  
  #aplica o modelo no fold de teste
  pred <- predict(model, myDataWithoutIdentifierAndClassification[[t]])
  
  #aplica o modelo no fold de teste (PCA)
  predPca <- predict(modelPca, myPcaDataWithoutIdentifierAndClassification[[t]])
  
  #aplica o modelo no fold de teste (RELIEF)
  predRelief <- predict(modelRelief, myDataWithoutIdentifierAndClassification[[t]][,1:8])
  
  resultado <- table(pred, myDataClassification[[t]])
  resultadoPca <- table(predPca, myDataClassification[[t]])
  resultadoRelief <- table(predRelief, myDataClassification[[t]])
  
  print(resultado)
  print(resultadoPca)
  print(resultadoRelief)
  
  tp <- tp + resultado[1,1]
  fp <- fp + resultado[1,2]
  tn <- tn + resultado[2,2]
  fn <- fn + resultado[2,1]
  
  tpPca <- tpPca + resultadoPca[1,1]
  fpPca <- fpPca + resultadoPca[1,2]
  tnPca <- tnPca + resultadoPca[2,2]
  fnPca <- fnPca + resultadoPca[2,1]
  
  tpRelief <- tpRelief + resultadoRelief[1,1]
  fpRelief <- fpRelief + resultadoRelief[1,2]
  tnRelief <- tnRelief + resultadoRelief[2,2]
  fnRelief <- fnRelief + resultadoRelief[2,1]
}

resultadoFinal.precisao <- tp/(tp+fp)
resultadoFinal.sensibilidade <- tp/(tp+fn)
resultadoFinal.errototal <- (fn+fp)/(tp+fp+tn+fn)

resultadoFinalPca.precisao <- tpPca/(tpPca+fpPca)
resultadoFinalPca.sensibilidade <- tpPca/(tpPca+fnPca)
resultadoFinalPca.errototal <- (fnPca+fpPca)/(tpPca+fpPca+tnPca+fnPca)

resultadoFinalRelief.precisao <- tpRelief/(tpRelief+fpRelief)
resultadoFinalRelief.sensibilidade <- tpRelief/(tpRelief+fnRelief)
resultadoFinalRelief.errototal <- (fnRelief+fpRelief)/(tpRelief+fpRelief+tnRelief+fnRelief)