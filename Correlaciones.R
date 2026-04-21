########## Correlacion

## Correlación Parcial:
#Si la correlación entre X e Y es significativa, pero al calcular la correlación parcial (controlando por Z) el p-valor deja de ser significativo, entonces la relación era indirecta.
library(ppcor)

#1) P valor base (correlaciones sig entre variables?)
cor.test(Datos$Var1, Datos$Var2, method = "spearman") # "pearson" relacion lineal (V continuas y norm), "spearman" no son normales o relaciones no lineales

#2) correlacion parcial
pcor.test(Datos$Variable1, Datos$Variable2, Datos$VariableControl)

#3) Modelos de Ecuaciones Estructurales (SEM) Permiten descomponer la correlación total en Efectos Directos e Efectos Indirectos y otorgan un $p$-valor para cada "flecha" de tu modelo
library(lavaan)
modelo <- ' VarRespuesta ~ b*VarIndependiente + c*VarMediadora
            VarMediadora ~ a*VarIndependiente 
            indirecto := a*c '