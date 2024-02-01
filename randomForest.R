library(randomForest)
library(e1071)
library(caTools)
dataset <- read_csv("moments_total.csv")

### Random Cross Validation
set.seed(2)
split = sample.split(dataset$Etiqueta, SplitRatio = .7)
summary(split)

### Training
training_set = subset(dataset, split == TRUE)

### Test
test_set = subset(dataset, split == FALSE)

### Entrenar el Clasificador
# Funcion kernel determina la funcion y va aprendiendo la curva
rf1 = randomForest(formula = as.factor(Etiqueta)~., data = training_set)

### Predicciones del clasificador
pred1 = predict(rf1,newdata = test_set[,-17])

### Matriz Confusion
library(caret)
con = confusionMatrix(pred1,as.factor(test_set$Etiqueta))
mc = t(con$table)
accuracy = con$overall[1]
precision = con$byClass[,5]
recall = con$byClass[,6]
f1 = con$byClass[,7]
