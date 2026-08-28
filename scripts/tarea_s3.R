# -----------------------------------------------------------------------------
# 
# Autor: Camila Bravo Figueroa
# Fecha: 27-08-2026
# Descripción: Muestra y analiza el salario de un grupo de trabajadores
# su nivel de educación y los años de experiencia.
# 
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# A continuación se crean 3 vectores asociados al salario en pesos de 5 
# trabajadores, su nivel de educación en categorías y los años de experiencia.

salario <- c(600000, 750000, 900000, 1200000, 2000000)
nivel_educacion <- c("Media", "Técnico", "Universitario", "Universitario", 
                     "Postgrado")
anios_experiencia <- c(2, 5, 8, 12, 20)

# ¿Por qué? -> Se separan estas tres variables en vectores propios porque 
# representan variables conceptualmente distintas del mismo grupo de 
# trabajadores y así se pueden manipular y analizar de forma independiente 
# antes de cruzarlas.

# -----------------------------------------------------------------------------
# Verificación de tipos de datos
class(salario)              # el vector salario tiene datos numéricos.
class(nivel_educacion)      # el vector nivel de educación tiene datos de texto.
class(anios_experiencia)    # el vector año de experiencia tiene datos numéricos.

# ¿Por qué? ->  Se verifica el tipo de cada vector antes de operar con ellos, 
# para asegurarse de que R los trate como numéricos o texto según corresponda y 
# evitar errores en cálculos posteriores.

# -----------------------------------------------------------------------------
# verificación del tamaño del vector
length(salario)             # el vector salario tiene 5 elementos.
length(nivel_educacion)     # el vector nivel de educación tiene 5 elementos.
length(anios_experiencia)   # el vector años de experiencia tiene 5 elementos.

# ¿Por qué? -> Se confirma que los tres vectores tengan la misma longitud, ya 
# que representan al mismo grupo de trabajadores y deben poder cruzarse elemento 
# a elemento sin errores.

# -----------------------------------------------------------------------------
# Extracción del segundo elemento del salario y los 3 últimos.
salario [2]             # el segundo elemento del salario es 750000
salario [c(3, 4, 5)]    # los 3 últimos elementos son 900000, 1200000 y 2000000.

# ¿Por qué? -> Se extraen elementos puntuales para mostrar que se puede 
# consultar el salario de un trabajador específico o de un subgrupo, en caso de
# querer revisar un caso individual o un grupo determinado.

# -----------------------------------------------------------------------------
# Extracción de todos los elementos menos el último
salario[-length(salario)]   # todos los elementos menos el último son 600000,  
                            # 750000, 900000 y 1200000.

# ¿Por qué? -> Se excluye el último elemento para ilustrar cómo aislar un caso 
# (por ejemplo, el de mayor salario) del resto del grupo, sin necesidad de 
# conocer su posición exacta de antemano.

# -----------------------------------------------------------------------------
# Filtro de los salarios mayores que 1000000
salario[salario > 1000000]  # los elementos mayores que 1000000 son 1200000 y 
                            # 2000000.

sum(salario > 1000000)      # hay 2 salarios mayores que 1000000.
mean(salario > 1000000)     # el 40% de los salarios son mayores que 1000000.

# ¿Por qué? -> Se filtra por este umbral para identificar a los trabajadores 
# mejor pagados del grupo, y se cuantifica cuántos son y qué proporción 
# representan, un dato útil para saber quiénes están en el rango salarial más 
# alto y evaluar sus remuneraciones.

# -----------------------------------------------------------------------------
# Filtro de salarios menores que 850000
salario[salario < 850000]   # los salarios menores que 850000 son 600000 y 
                            # 750000.

sum(salario < 850000)       # hay 2 salarios menores que 850000.
mean (salario < 850000)     # el 40% de los salarios son menores que 850000.

# ¿Por qué? -> Se filtra por este umbral para detectar a los trabajadores con 
# salarios más bajos, un paso necesario si se quiere evaluar ajustes de sueldo 
# mínimo o brechas salariales dentro del grupo.

# -----------------------------------------------------------------------------
# Filtro de salarios de trabajadores con educación Postgrado
salario[nivel_educacion == "Postgrado"]   # el salario de los trabajadores con
                                          # Postgrado es 2000000.

sum(nivel_educacion == "Postgrado")     # hay sólo un trabajador con Postgrado.
mean(nivel_educacion == "Postgrado")    # el 20% de los trabajadores tiene nivel
                                        # de educación Postgrado.

# ¿Por qué? -> Se cruza salario con nivel educativo para explorar si el nivel 
# de estudios está asociado a mejores remuneraciones, y se mide qué proporción 
# del grupo alcanza el nivel más alto de educación.

# -----------------------------------------------------------------------------
# Filtro de trabajadores con más de 10 años de experiencia
anios_experiencia[anios_experiencia > 10]   # los años de experiencia que 
                                            # superan los 10 son 12 y 20.

sum(anios_experiencia > 10)     # hay 2 trabajadores con más de 10 años de 
                                # experiencia.
mean(anios_experiencia > 10)    # el 40% de los trabajadores tiene más de 10 
                                # años de experiencia.

# ¿Por qué? -> Se filtra por años de experiencia para identificar al personal 
# con mayor estudio del grupo, dato relevante para decisiones de ascensos o 
# retención de talento experimentado.

# -----------------------------------------------------------------------------
# Asignación de bono de gratificación por años de experiencia de 50000 x c/ año.
gratificación <- anios_experiencia * 50000

# ¿Por qué? -> Se aplica esta operación a todo el vector de una sola vez, 
# aprovechando que R permite multiplicar cada año de experiencia por el valor 
# del bono sin necesidad de un cálculo separado por trabajador.

#------------------------------------------------------------------------------
# Asignación del bono al salario de cada trabajador.
salario_total <- salario + gratificación

# ¿Por qué? -> Se suma el bono al salario base para obtener la remuneración real 
# que recibe cada trabajador, que es la cifra que interesa para comparaciones de 
# ingreso real entre ellos.

# -----------------------------------------------------------------------------
# ESTADÍSTICAS DEL VECTOR DE AÑOS DE EXPERIENCIA

# Calculo del promedio de años de experiencia.
mean(anios_experiencia) 
# En promedio, los trabajadores tienen 9,4 años de experiencia.

# Calculo de la mediana de los años de experiencia.
median(anios_experiencia)
# La mediana corresponde a 8 años, lo cual sugiere que el trabajador central 
# del grupo posee 8 años de experiencia.

# Cálculo de la desviación estándar de los años de experiencia.
sd(anios_experiencia)
# La desviación estándar de 6,99 años indica que la experiencia de los 
# trabajadores se desvía, en promedio, se desvían en 6,99 años de la media.

# Cálculo de cuantiles de los años de experiencia.
quantile(anios_experiencia)
# Los cuantiles indican que la mayoría de los trabajadores se concentra entre 5 
# y 12 años de experiencia, mientras que el máximo observado es de 20 años.

# ¿Por qué? -> Se calculan estas cuatro medidas en conjunto porque cada una 
# aporta algo distinto: el promedio resume el nivel general de experiencia, la 
# mediana evita la influencia de valores extremos, la desviación estándar mide 
# qué tan dispersos están los datos, y los cuantiles muestran cómo se distribuye 
# el grupo en tramos, dando una visión más completa que cualquiera de ellas por 
# separado.

# -----------------------------------------------------------------------------
