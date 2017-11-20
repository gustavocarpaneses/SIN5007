library("e1071")
#Quantidade de Fold

treinoNaive <- function(tipo){
  FP <- 0
  FN <- 0
  TP <- 0
  TN <- 0  
f = 1
for(f in 1:5){
  r = 1
  treino = list()
  teste = list()
  arquivo = ''

  while(r <= 5)
  {
    if(tipo == 'PCA'){
      arquivo<-sprintf("C:\\Users\\AdrianoDev\\Documents\\Mestrado\\Reconhecimento Padrões\\cancer\\SIN5007\\data\\breast-cancer-wisconsin-with-class-names-PCA-Score-fold-%d.data", r)  
    }else if (tipo == 'RELIEF'){
      arquivo<-sprintf("C:\\Users\\AdrianoDev\\Documents\\Mestrado\\Reconhecimento Padrões\\cancer\\SIN5007\\data\\breast-cancer-wisconsin-with-class-names-Relief-fold-%d.data", r)  
    }else{
      arquivo<-sprintf("C:\\Users\\AdrianoDev\\Documents\\Mestrado\\Reconhecimento Padrões\\cancer\\SIN5007\\data\\breast-cancer-wisconsin-with-class-names-fold-%d.data", r)  
    }
    
    if(r != f){
      temp <- read.table(arquivo, sep = ",", header = FALSE)
      treino <- rbind(temp, treino)
      
    }else{
      teste <- read.table(arquivo, sep = ",", header = FALSE)
    }
    r = r+1
  }
  
  modelo_NB <- ''
  if(tipo == 'PCA'){
    modelo_NB <- naiveBayes(treino[2:6], as.factor(treino[,11]))  
  }else if (tipo == 'RELIEF'){
    modelo_NB <- naiveBayes(treino[2:9], as.factor(treino[,11]))  
  }else{
    modelo_NB <- naiveBayes(treino[2:10], as.factor(treino[,11]))  
  }
  
  l = 1

  while(l <= length(teste[,1])){
    p <- predict(modelo_NB, teste[l,], type="class" )
    if(teste$V11[l] == 'benign' && p == 'benign'){
      TP <- TP + 1
    }
    else if(teste$V11[l] == 'benign' && p == 'malignant'){
      FN <- FN + 1 
    }
    else if(teste$V11[l] == 'malignant' && p == 'malignant'){
      TN <- TN + 1
    }
    else if(teste$V11[l] == 'malignant' && p == 'benign'){
      FP <- FP + 1
    }
    
    l <- l+1
  }
  
}
print("===========================================================================")
print(sprintf("TP: %d FN: %d TN:%d FP: %d", TP, FN, TN, FP))
print(sprintf("Taxa de Falsa Aceitação (FAR): %f ", FP/length(teste[,1])))
print(sprintf("Taxa de Falsa Rejeição (FRR): %f ", FN/length(teste[,1])))
print(sprintf("Sensibilidadeou recallou TP rate: %f ", (TP/(TP+FN))))
print(sprintf("Especificidade: %f ",  (TN/(TN+FP))))
print(sprintf("FP rate: %f ", (FP/(TN+FP))))
print(sprintf("Precisão: %f ",  (TP/(TP+FP))))
print("===========================================================================")

}

treinoNaive('PCA')
#treinoNaive('RELIEF')
#treinoNaive('TODAS')
rm(list = ls())
