#carregando arquivo
myData <- read.csv(file="../data/breast-cancer-wisconsin.data", header=FALSE, sep=",")

#retira a coluna que tem o identificador
myData.active <- myData[, 2:11]

#executa o algoritmo relief, indicando que a classe está na coluna "V11"
weights <- relief(V11~., myData.active, neighbours.count = 5, sample.size = 200)

#exibe os pesos
print(weights)

#realiza o corte
subset <- cutoff.k(weights, 8)

#características selecionadas
print(subset)