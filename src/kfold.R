kfold <- function(k){
dataset <- read.table("C:\\Users\\AdrianoDev\\Documents\\Mineracao\\dataset.csv", header=FALSE, sep=";")

qtde2 <- (length(dataset[,1])/k)*0.65   
qtde4 <- (length(dataset[,1])/k) - qtde2
t = 1
  for(t in 1:k){
    labelDois = qtde2;
    labelQuatro = qtde4;
    arquivo<-sprintf("C:\\Users\\AdrianoDev\\Documents\\Mineracao\\Fold%d.csv", t)
    while(labelDois >= 1 || labelQuatro >= 1 && length(dataset[,1]) != 0){
      n <- sample(1:length(dataset[,1]),1)
      str<-sprintf("Randmom %d Fold X: %d Length dataset: %d label4: %f label2: %f qtde2: %f qtde4: %f", n, t, length(dataset[,1]), labelQuatro, labelDois, qtde2, qtde4)
      #print(str)
      if(dataset$V11[n] == 4 && labelQuatro >= 1){
        write.table(dataset[n,], file = arquivo,  sep=";", append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
        labelQuatro = labelQuatro - 1;
        dataset <- dataset[-c(n),] 
      }
      else if(dataset$V11[n] == 2 && labelDois >= 1){
        write.table(dataset[n,], file = arquivo,  sep=";", append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
        labelDois = labelDois - 1;
        dataset <- dataset[-c(n),] 
      }
    }
  }

while(length(dataset[,1]) != 0){
  t = 1
  while(t <= k && length(dataset[,1]) != 0 ){
    arquivo<-sprintf("C:\\Users\\AdrianoDev\\Documents\\Mineracao\\Fold%d.csv", t)
    n <- sample(1:length(dataset[,1]),1)
    write.table(dataset[n,], file = arquivo,  sep=";", append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
    dataset <- dataset[-c(n),]
    t = t + 1;
  }
}
}
kfold(5)

  
