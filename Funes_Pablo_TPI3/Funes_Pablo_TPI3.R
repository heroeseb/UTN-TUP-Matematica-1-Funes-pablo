# Alumno: Funes Pablo Sebastian
# Fecha: 26/04/2026
# Legajo: 54219

# ============================================================
# Desafío 1: Generador Algorítmico de Conjunto Potencia P(A)
# ============================================================

# 1. Crear el conjunto A
# Definimos un vector que representará un conjunto matemático
A <- c("violeta", "verde", "azul", "naranja")

# 2. Generar el conjunto potencia P(A)
# El conjunto potencia es el conjunto de TODOS los subconjuntos posibles de A
P_A <- list()  # Inicializamos una lista vacía para almacenar los subconjuntos

# Recorremos desde 0 hasta la cantidad de elementos de A
# Esto representa el tamaño de los subconjuntos que vamos a generar
for (i in 0:length(A)) {
  
  if (i == 0) {
    # Caso especial: subconjunto vacío
    # Por definición, todo conjunto potencia contiene el conjunto vacío
    P_A <- c(P_A, list(c()))
    
  } else {
    # Generamos todas las combinaciones posibles de A con i elementos
    combinaciones <- combn(A, i, simplify = FALSE)
    
    # Agregamos esas combinaciones a la lista del conjunto potencia
    P_A <- c(P_A, combinaciones)
  }
}

# 3. Verificar propiedad fundamental: |P(A)| = 2^n
# Donde n es la cantidad de elementos del conjunto A

cantidad_subconjuntos <- length(P_A)  # Total de subconjuntos generados
n <- length(A)  # Cantidad de elementos en A

cat("Cantidad de subconjuntos:", cantidad_subconjuntos, "\n")

# Verificación lógica de la propiedad matemática
# Si es TRUE, el algoritmo funciona correctamente
cat("¿Se cumple |P(A)| = 2^n?:", cantidad_subconjuntos == 2^n, "\n")


# 4. Obtener subconjuntos propios
# Subconjuntos propios = todos los subconjuntos EXCEPTO el conjunto A

subconjuntos_propios <- list()

for (subconjunto in P_A) {
  
  # setequal compara conjuntos sin importar el orden
  # Si NO son iguales a A, entonces es un subconjunto propio
  if (!setequal(subconjunto, A)) {
    subconjuntos_propios <- c(subconjuntos_propios, list(subconjunto))
  }
}

# Mostrar subconjuntos propios
cat("\nSubconjuntos propios:\n")

for (i in seq_along(subconjuntos_propios)) {
  cat("Subconjunto", i, ":", subconjuntos_propios[[i]], "\n")
}

cat("Cantidad subconjuntos propios:", length(subconjuntos_propios), "\n")



# ============================================================
# Desafío 2: Verificador de Isomorfismo y Leyes de De Morgan 
# ============================================================

# 1. Definir universo y subconjuntos
# El universo representa el conjunto total de referencia
U <- 1:10

# Subconjuntos de U
A <- c(1, 2, 3, 4)
B <- c(3, 4, 5, 6)

# 2. Calcular complementos
# Complemento de A = elementos de U que NO están en A
comp_A <- setdiff(U, A)

# Complemento de B
comp_B <- setdiff(U, B)

# Lado izquierdo de la ley:
# ¬(A ∪ B) → complemento de la unión
lado_izq <- setdiff(U, union(A, B))

# Lado derecho de la ley:
# (¬A ∩ ¬B) → intersección de complementos
lado_der <- intersect(comp_A, comp_B)

# 3. Verificación de la Primera Ley de De Morgan
# Se utiliza setequal para verificar igualdad de conjuntos

cat("\nLey de De Morgan:\n")
cat("¿Son iguales ambos lados?:", setequal(lado_izq, lado_der), "\n")



# ============================================================
# Desafío 3: El "Data Splitter" y Validación de Particiones 
# ============================================================

# 1. Definir universo
# Representa el conjunto total de datos (por ejemplo, registros)
D <- 1:12

# 2. Definir subconjuntos
# Estos deben formar una partición válida del universo
P1 <- c(1, 2, 3, 4)
P2 <- c(5, 6, 7, 8)
P3 <- c(9, 10, 11, 12)

# 3. Función para validar partición
validar_particion <- function(U, P1, P2, P3) {
  
  # -------------------------------
  # CONDICIÓN 1: DISJUNTIVIDAD
  # -------------------------------
  # Los subconjuntos NO deben tener elementos en común
  
  disjuntos <- length(intersect(P1, P2)) == 0 &&
    length(intersect(P1, P3)) == 0 &&
    length(intersect(P2, P3)) == 0
  
  # -------------------------------
  # CONDICIÓN 2: RECUBRIMIENTO
  # -------------------------------
  # La unión de todos los subconjuntos debe ser igual al universo
  
  union_total <- union(union(P1, P2), P3)
  recubrimiento <- setequal(U, union_total)
  
  # -------------------------------
  # RESULTADO FINAL
  # -------------------------------
  if (disjuntos && recubrimiento) {
    return("Partición Válida")
  } else {
    return("Error en Partición")
  }
}

# 4. Ejecutar validación
cat("\nValidación de partición:\n")

resultado <- validar_particion(D, P1, P2, P3)

cat(resultado, "\n")
