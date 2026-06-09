# =========================================================
# 00_auxiliares.R
# Funciones auxiliares para trabajar con tablas 2x2
# =========================================================

# Convencion usada en todo el proyecto:
#            Pred +
#            Pred -
# Real +   TP   FN
# Real -   FP   TN

#' Comprobar tabla 2x2
#'
#' Comprueba que la entrada es una matriz o tabla de dimension 2x2,
#' sin valores perdidos y sin frecuencias negativas.
#'
#' @param tab Matriz o tabla 2x2.
#'
#' @return TRUE de forma invisible si la tabla es valida.
#'
#' @export
check_2x2 <- function(tab) {
  if (!is.matrix(tab) && !is.table(tab)) {
    stop("La entrada debe ser una matriz o tabla 2x2.")
  }

  if (!all(dim(tab) == c(2, 2))) {
    stop("La entrada debe tener dimension 2x2.")
  }

  if (any(is.na(tab))) {
    stop("La tabla contiene valores NA.")
  }

  if (any(tab < 0)) {
    stop("La tabla no puede contener frecuencias negativas.")
  }

  invisible(TRUE)
}

#' Division segura
#'
#' Realiza una division controlando denominadores nulos o valores perdidos.
#'
#' @param num Numerador.
#' @param den Denominador.
#'
#' @return Resultado de la division o NA si no esta definida.
#'
#' @export
safe_divide <- function(num, den) {
  if (is.na(num) || is.na(den) || den == 0) {
    return(NA_real_)
  }

  num / den
}

#' Extraer celdas de una tabla 2x2
#'
#' Extrae TP, FN, FP y TN de una tabla 2x2 siguiendo la convencion del paquete.
#'
#' @param tab Matriz o tabla 2x2.
#'
#' @return Lista con TP, FN, FP y TN.
#'
#' @examples
#' tab <- matrix(c(50, 10,
#'                 5, 35),
#'               nrow = 2,
#'               byrow = TRUE)
#' get_cells(tab)
#'
#' @export
get_cells <- function(tab) {
  check_2x2(tab)

  list(
    TP = as.numeric(tab[1, 1]),
    FN = as.numeric(tab[1, 2]),
    FP = as.numeric(tab[2, 1]),
    TN = as.numeric(tab[2, 2])
  )
}

#' Calcular tamano muestral total
#'
#' Calcula el numero total de observaciones de una tabla 2x2.
#'
#' @param tab Matriz o tabla 2x2.
#'
#' @return Numero total de observaciones.
#'
#' @export
get_N <- function(tab) {
  cells <- get_cells(tab)
  cells$TP + cells$FN + cells$FP + cells$TN
}

#' Convertir datos binarios a tabla 2x2
#'
#' Construye una tabla 2x2 a partir de dos vectores: valores reales y predichos.
#'
#' @param real Vector con valores reales.
#' @param pred Vector con valores predichos.
#' @param positive Valor que se considera clase positiva.
#'
#' @return Matriz 2x2 con la convencion TP, FN, FP, TN.
#'
#' @examples
#' real <- c(1, 1, 0, 0)
#' pred <- c(1, 0, 1, 0)
#' df_to_2x2(real, pred)
#'
#' @export
df_to_2x2 <- function(real, pred, positive = 1) {
  if (length(real) != length(pred)) {
    stop("real y pred deben tener la misma longitud.")
  }

  if (any(is.na(real)) || any(is.na(pred))) {
    stop("real y pred no deben contener NA.")
  }

  real_bin <- ifelse(real == positive, 1, 0)
  pred_bin <- ifelse(pred == positive, 1, 0)

  TP <- sum(real_bin == 1 & pred_bin == 1)
  FN <- sum(real_bin == 1 & pred_bin == 0)
  FP <- sum(real_bin == 0 & pred_bin == 1)
  TN <- sum(real_bin == 0 & pred_bin == 0)

  matrix(
    c(TP, FN,
      FP, TN),
    nrow = 2,
    byrow = TRUE
  )
}

#' Estimar error estandar mediante bootstrap
#'
#' Estima el error estandar de una métrica calculada sobre una tabla 2x2
#' mediante remuestreo bootstrap.
#'
#' @param tab Matriz o tabla 2x2.
#' @param stat_fun Funcion que recibe una tabla 2x2 y devuelve un estadistico numerico.
#' @param B Numero de remuestreos bootstrap.
#' @param seed Semilla para reproducibilidad.
#'
#' @return Error estandar bootstrap.
#'
#' @export
bootstrap_se_2x2 <- function(tab, stat_fun, B = 1000, seed = 123) {
  check_2x2(tab)

  if (!is.function(stat_fun)) {
    stop("stat_fun debe ser una funcion.")
  }

  set.seed(seed)

  cells <- get_cells(tab)

  real <- c(
    rep(1, cells$TP),
    rep(1, cells$FN),
    rep(0, cells$FP),
    rep(0, cells$TN)
  )

  pred <- c(
    rep(1, cells$TP),
    rep(0, cells$FN),
    rep(1, cells$FP),
    rep(0, cells$TN)
  )

  datos <- data.frame(real = real, pred = pred)
  n <- nrow(datos)

  if (n == 0) {
    return(NA_real_)
  }

  valores <- numeric(B)

  for (b in seq_len(B)) {
    idx <- sample.int(n, size = n, replace = TRUE)
    boot_data <- datos[idx, ]

    boot_tab <- df_to_2x2(
      real = boot_data$real,
      pred = boot_data$pred,
      positive = 1
    )

    valores[b] <- stat_fun(boot_tab)
  }

  sd(valores, na.rm = TRUE)
}
