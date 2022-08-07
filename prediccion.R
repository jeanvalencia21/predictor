#cargo informacion de los pacientes
pacientes <- read.csv(file = "dengue_data.csv")
str(pacientes)

#Creo tablas de entrenamiento y prueba
filas_para_entrenar <- sample(1:999, 750)
archivo_entrenamiento <- pacientes[filas_para_entrenar, ]
archivo_prueba <- pacientes[-filas_para_entrenar, ]

#instalo libreria rpart
library(rpart)

#se crea el modelo del arbol de decisicion
arbol_enfermedad <- rpart(Class ~ ., data=archivo_entrenamiento)

#instalo la libreria para el plot
library(rpart.plot)

#grafico el plot , extra=104 es para respuestas binarias
rpart.plot(arbol_enfermedad, type=2, extra=104)

#guardo el archivo de entrenamiento
write.csv(archivo_entrenamiento, "archivoentrenamiento")

#Prediccion del modelo de la tabla de pruebas
prediccion <- predict(arbol_enfermedad, archivo_prueba, type="class")
prediccion

#se crea la matriz de confusion
matriz_de_confusion <- table(archivo_prueba$Class, prediccion)
matriz_de_confusion

#Precision global de prediccion
precision <- sum(diag(matriz_de_confusion))/nrow(archivo_prueba)
precision

#Precision no enfermos
precision_no <- ((matriz_de_confusion[1,1]))/sum(matriz_de_confusion[1,])
precision_no

#Precision si enfermos
precision_si <- ((matriz_de_confusion[2,2]))/sum(matriz_de_confusion[2,])
precision_si

#se carga la informacion de nuevos pacientes para predecir
pacientes_nuevos <- read.csv(file="dengue_query.csv")

#se aplica el modelo a los nuevos pacientes
prediccion_nuevos <- predict(arbol_enfermedad, pacientes_nuevos, type="class")
prediccion_nuevos

#se agrega la prediccion al archivo que contiene a los pacientes nuevos
pacientes_predichos <- cbind(pacientes_nuevos, prediccion_nuevos)
pacientes_predichos

#cambie el nombre de la columna
colnames(pacientes_predichos)[19] <- "Prediccion"

#ejecuto para ver si se cambio
pacientes_predichos

#se guarda el archivo de la prediccion de los nuevos pacientes
write.csv(pacientes_predichos, "pacientes_predichos")
