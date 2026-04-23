####### Grafico de inversiones de energia en distintos aspectos ###
#relativizar es una buena forma de supar peras con manzanas pero si relativizamos y sumamos cosas que tienen 
#inversiones energiticas muy distintas... vamos a tener barras gruesas de cosas que no son realmente relevantes...

#quiza en lugar de hacer barras apiladas es mas visual hacer barras una al lado de la otra
Relativizado <- read_excel("BD Cobre.xlsx", sheet ="relativizado",col_names = TRUE)
names(Relativizado)

