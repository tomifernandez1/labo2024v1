# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
paquetes <- c("data.table", "rpart", "rpart.plot","languageserver")

# Check each package
for (pkg in paquetes) {
  # If the package is not installed, install it
  if (!pkg %in% installed.packages()) {
    install.packages(pkg)
  }
}

require("data.table")
require("rpart")
require("rpart.plot")


# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\tomif\\Desktop\\Maestria\\laboratorio_1\\labo2024v1") # Establezco el Working Directory

# cargo el dataset
dataset <- fread(".\\datasets\\dataset_pequeno.csv")
dataset[, clase_binaria1:= ifelse (clase_ternaria =='BAJA+2',"SUMA","RESTA")]
dataset[, clase_ternaria := NULL]


dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
        formula = "clase_binaria1 ~ .",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = -0.1, # esto significa no limitar la complejidad de los splits
        minsplit = 640, # minima cantidad de registros para que se haga el split
        minbucket = 2, # tamaño minimo de una hoja
        maxdepth = 10
) # profundidad maxima del arbol


# grafico el arbol
prp(modelo,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0
)


# aplico el modelo a los datos nuevos
prediccion <- predict(
        object = modelo,
        newdata = dapply,
        type = "prob"
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "SUMA"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
        file = "./exp/KA2001/K101_binaria1.csv",
        sep = ","
)
