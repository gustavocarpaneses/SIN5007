#carregando pacotes
#install.packages("e1071")
#library("e1071")

#carrega os folds
foldFileName = "C:/temp/sin5007/data/breast-cancer-wisconsin-with-class-names-fold-%d.data"

k = 5
t = 1

myData <- list()
myDataWithoutIdentifier <- list()
myDataWithoutIdentifierAndClassification <- list()
myDataClassification <- list()

for(t in 1:k){
  
  #carregando arquivo
  myData[[t]] <- read.csv(file=sprintf(foldFileName, t), header=FALSE, sep=",")
  
  #retira a coluna que tem o identificador
  myDataWithoutIdentifier[[t]] <- myData[[t]][, 2:11]
  
  #retira a coluna que tem o identificador e a classificação
  myDataWithoutIdentifierAndClassification[[t]] <- myData[[t]][, 2:10]
  
  #apenas a coluna de classificação
  myDataClassification[[t]] <- myData[[t]][, 11:11]

}

t = 1
tp <- 0
fp <- 0
tn <- 0
fn <- 0

for(t in 1:k){

  tf = 1
  
  trainingFold <- list()
  trainingFoldClassification <- factor()
  
  for(tf in 1:k){
    #combina todos os folds, exceto o atual (que vai ser utilizado para o teste)
    if(tf != t){
      trainingFold <- rbind(trainingFold, myDataWithoutIdentifierAndClassification[[tf]])
      trainingFoldClassification <- factor(c(
        as.character(trainingFoldClassification), 
        as.character(myDataClassification[[tf]])))
    }
    
  }
  
  #constrói o modelo
  model <- svm(trainingFold, trainingFoldClassification)
  
  summary(model)
  
  #aplica o modelo no fold de teste
  pred <- predict(model, myDataWithoutIdentifierAndClassification[[t]])
  
  resultado <- table(pred, myDataClassification[[t]])
  
  print(resultado)
  
  tp <- tp + resultado[1,1]
  fp <- fp + resultado[1,2]
  tn <- tn + resultado[2,2]
  fn <- fn + resultado[2,1]
}

precisao <- tp/(tp+fp)
sensibilidade <- tp/(tp+fn)
errototal <- (fn+fp)/(tp+fp+tn+fn)