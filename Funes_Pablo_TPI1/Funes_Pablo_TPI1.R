# Desafío 1: Automatización de Tablas de Verdad

# Creamos los vectores de las variables con todas las combinaciones posibles
p <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
q <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
r <- c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE)

# Creamos la tabla con las variables
tabla <- data.frame(p, q, r)

# Añadimos una columna nueva que calcula el resultado de la expresión lógica
tabla$resultado <- (p | q) & !r

# Mostramos la tabla completa
print(tabla)

# La expresión (p | q) & !r es una CONTINGENCIA,
# ya que su valor depende de las combinaciones de p, q y r
# (en algunos casos da TRUE y en otros FALSE)


# Desafío 2: Motor de Decisiones (Caso: Ingreso a Sistemas)

# Definimos las variables lógicas
clave <- TRUE
admin <- TRUE
autorizacion <- FALSE

# Aplicamos la regla de negocio:
# Se permite el acceso si tiene clave Y (es admin O tiene autorización)
acceso <- clave & (admin | autorizacion)

# Mostramos el resultado
print(acceso)

# Cambiamos el valor de clave para probar otro caso
clave <- FALSE

# Recalculamos el acceso
acceso <- clave & (admin | autorizacion)

# Mostramos el nuevo resultado
print(acceso)

# Como usamos AND (&), si una de las condiciones es FALSE,
# todo el resultado será FALSE, sin importar las demás condiciones


# Desafío 3: Lógica de Predicados y Big Data

# Creamos un vector con el estado de 100 servidores (95 activos y 5 caídos)
estado_servidores <- c(rep(TRUE, 95), rep(FALSE, 5))

# Cuantificador universal: verifica si TODOS están activos
all(estado_servidores)

# Resultado: FALSE, porque no todos los servidores cumplen la condición

# Cuantificador existencial: verifica si existe al menos uno que NO esté activo
any(!estado_servidores)

# Resultado: TRUE, porque hay al menos un servidor caído

# Explicación:
# La función all() devuelve TRUE solo si todos los valores son TRUE.
# Si al menos uno no la cumple, el resultado será FALSE.