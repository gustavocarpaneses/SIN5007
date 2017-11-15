library("RSNNS")
dataset <- read.table('C:\\Users\\AdrianoDev\\Documents\\Mestrado\\Reconhecimento Padrões\\cancer\\SIN5007\\data\\breast-cancer-wisconsin-with-class-names-PCA-Score-fold-1.data', sep = ',')
datasetTest <- read.table('C:\\Users\\AdrianoDev\\Documents\\Mestrado\\Reconhecimento Padrões\\cancer\\SIN5007\\data\\breast-cancer-wisconsin-with-class-names-PCA-Score-fold-2.data', sep = ',')

mlp(
  x = dataset[2:6],
  
  y = ifelse(datasetTest[,11] %in% c('malignant'), 0, 1),
  size = 10,
  maxit = 500,
  initFunc = "Randomize_Weights",
  learnFunc = "Std_Backpropagation",
  learnFuncParams = c(0.1),
  hiddenActFunc = "Act_Logistic",
  shufflePatterns = TRUE,
  linOut = TRUE,
  inputsTest = datasetTest[2:6],
  targetsTest = datasetTest[,11]
)