library('neuralnet')

treinoRede <- function(tipo){
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
    
    modelo_Rede <- ''
    saida <- ''
    resp <- ''
    if(tipo == 'PCA'){

      modelo_Rede <- neuralnet( formula = ifelse(treino[,11] %in% c('malignant'), 0, 1)~treino$V2+treino$V3+treino$V4+treino$V5 + treino$V6, 
                 data = treino, hidden = c(2), learningrate = 0.1, algorithm = "backprop", threshold = 0.1, linear.output = F )
      saida<- compute(x = modelo_Rede, covariate = teste[2:6])
      

    }else if(tipo == 'RELIEF'){
      modelo_Rede <- neuralnet( formula = ifelse(treino[,11] %in% c('malignant'), 0, 1)~treino$V2+treino$V3+treino$V4+treino$V5 + treino$V6 + treino$V7 + treino$V8 + treino$V9, 
                  data = treino, hidden = c(5), learningrate = 0.1, algorithm = "backprop", threshold = 0.1, linear.output = F )
      saida<- compute(x = modelo_Rede, covariate = teste[2:9])
      print(length(teste[,1]))
    }else{
      modelo_Rede <- neuralnet( formula = ifelse(treino[,11] %in% c('malignant'), 0, 1)~treino$V2+treino$V3+treino$V4+treino$V5 + treino$V6 + treino$V7 + treino$V8 + treino$V9 + treino$V10, 
                                data = treino, hidden = c(10), learningrate = 0.01, algorithm = "backprop", threshold = 0.1, linear.output = F )
      saida<- compute(x = modelo_Rede, covariate = teste[2:10])
    }
   
    resp <- table(ifelse(round(saida$net.result) %in% c(0), 'malignant', 'beningn'), teste$V11 )
    if(is.na(resp[1])) resp[1] = 0
    if(is.na(resp[2])) resp[2] = 0
    if(is.na(resp[3])) resp[3] = 0
    if(is.na(resp[4])) resp[4] = 0
    
    
    FP <- FP + resp[3]
    FN <- FN + resp[2]
    TP <- TP + resp[1]
    TN <- TN + resp[4]
    

  }
  
  print("===========================================================================")
  print(sprintf("TP: %f FN: %f TN:%f FP: %f", TP, FN, TN, FP))
  print(sprintf("Taxa de Falsa Aceitação (FAR): %f ", FP/length(teste[,1])))
  print(sprintf("Taxa de Falsa Rejeição (FRR): %f ", FN/length(teste[,1])))
  print(sprintf("Sensibilidadeou recallou TP rate: %f ", (TP/(TP+FN))))
  print(sprintf("Especificidade: %f ",  (TN/(TN+FP))))
  print(sprintf("FP rate: %f ", (FP/(TN+FP))))
  print(sprintf("Precisão: %f ",  (TP/(TP+FP))))
  print("===========================================================================")
  
}

#treinoRede("PCA")
#treinoRede("RELIEF")
treinoRede("Todas")

rm(list = ls())

