

# Nombre: Funes Pablo Sebastian  Fecha: 12/04/2026



# Desafío 1: Generador Automático de Formas Canónicas (SOP)

# Creamos los vectores de las variables con todas las combinaciones posibles
A <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
B <- c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE)
C <- c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE)

# Creamos la tabla con las variables
tabla <- data.frame(A, B, C)

# Añadimos una columna nueva que calcula el resultado de la expresión lógica
tabla$resultado <- (A | B) & !C 


# Utilizamos una función de filtrado de R para mostrar únicamente las filas donde salida == TRUE
subset(tabla, resultado == TRUE)

# En este caso la tabla da este resultado
#       A     B     C resultado
#  3 FALSE  TRUE FALSE      TRUE
#  5  TRUE FALSE FALSE      TRUE
#  7  TRUE  TRUE FALSE      TRUE

# y la forma SOP (Suma de Productos) canónica resultante de esos mintérminos:    m2 + m4 + m6    ó   A'BC' + AB'C' + ABC'





# Desafío 2: El Desafío de la Universalidad (NAND)

# esta función crea una función que actúa como un NAND 
nand_gate <- function(a, b) { !(a & b) }

# creamos la función OR usando solo NAND
or_con_nand <- function(a, b) { nand_gate ( nand_gate(a, a), nand_gate(b, b) ) }

#Genera una tabla de verdad comparando el resultado de tu "OR hecho con NANDs" contra el operador | de R.

A <- c(FALSE,FALSE,TRUE,TRUE)
B <- c(FALSE,TRUE,FALSE,TRUE)

tabla <- data.frame(A , B)
tabla$resultado_A_OR_B <- (A | B)
tabla$resultado_A_OR_B_hecho_con_NANDs <- (or_con_nand(A , B))
print(tabla)

# Este es el resultado que nos da la tabla y nos demuestra que al compararlos es resultado es igual!

#    A     B     resultado_A_OR_B      resultado_A_OR_B_hecho_con_NANDs
# 1 FALSE FALSE            FALSE                            FALSE
# 2 FALSE  TRUE             TRUE                             TRUE
# 3  TRUE FALSE             TRUE                             TRUE
# 4  TRUE  TRUE             TRUE                             TRUE





# Desafío 3: El "Kill Switch" y Minimización de Reglas de Negocio

# Creamos los vectores con 10 casos distintos
ingresos <- c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
antiguedad <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE)
vip <- c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
deuda <- c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE)

# Programamos la función original
F_original <- ((ingresos & antiguedad) | vip) & !deuda

# Simplificación de la fórmula original
F_minima <- ingresos & !deuda

#Usamos la función identical para verificación si la simplificación es válida o si se perdió lógica en el camino
identical(F_original, F_minima)

# Esto nos da de resultado [1] FALSE , demostrando que se perdió lógica en el camino y que no es válida la simplificación para todos los casos

# La función original incluye más condiciones y la simplificación las elimina 

# Deuda actúa como un "Kill Switch" porque es requisito fundamental para aprobar el crédito,
# si deuda es igual a True(1) , al estar negada con un NOT,el valor pasa a ser False(0) y como antes tiene una compuerta AND(*),
# cualquier cosa AND False, es == False, sin importar los valores anteriores, es decir que "mata" toda la lógica anterior
# "Si tiene deuda, NO se aprueba el crédito bajo ninguna circunstancia" ,por eso deuda actúa como un "Kill Switch"