# =========================================================
# 03_metricas_clinicas_y_asociacion.R
# Métricas clínicas y de asociación/correlación
# =========================================================

# Convención:
#            Pred+   Pred-
# Real+        TP      FN
# Real-        FP      TN

#' Calcular el likelihood ratio positivo
#'
#' Calcula el likelihood ratio positivo (LR+), definido como
#' sensibilidad / (1 - especificidad).
#'
#' @param tab Matriz o tabla 2x2 con frecuencias observadas.
#' @param se_method Método para calcular el error estándar: "bootstrap" o "none".
#' @param B Número de remuestreos bootstrap.
#' @param seed Semilla para reproducibilidad.
#'
#' @return Lista con statistic y se.
#'
#' @examples
#' tab <- matrix(c(50, 10,
#'                 5, 35),
#'               nrow = 2,
#'               byrow = TRUE)
#' indice_lr_pos(tab, B = 100)
#'
#' @export
indice_lr_pos <- function(tab,
                          se_method = c("bootstrap", "none"),
                          B = 1000,
                          seed = 123) {
  se_method <- match.arg(se_method)

  sens <- indice_sensibilidad(tab, se_method = "none")$statistic
  spec <- indice_especificidad(tab, se_method = "none")$statistic

  statistic <- safe_divide(sens, 1 - spec)

  se <- switch(
    se_method,
    bootstrap = {
      bootstrap_se_2x2(
        tab = tab,
        stat_fun = function(x) {
          sens_x <- indice_sensibilidad(x, se_method = "none")$statistic
          spec_x <- indice_especificidad(x, se_method = "none")$statistic
          safe_divide(sens_x, 1 - spec_x)
        },
        B = B,
        seed = seed
      )
    },
    none = NA_real_
  )

  list(statistic = statistic, se = se)
}

#' Calcular el likelihood ratio negativo
#'
#' Calcula el likelihood ratio negativo (LR-), definido como
#' (1 - sensibilidad) / especificidad.
#'
#' @param tab Matriz o tabla 2x2 con frecuencias observadas.
#' @param se_method Método para calcular el error estándar: "bootstrap" o "none".
#' @param B Número de remuestreos bootstrap.
#' @param seed Semilla para reproducibilidad.
#'
#' @return Lista con statistic y se.
#'
#' @examples
#' tab <- matrix(c(50, 10,
#'                 5, 35),
#'               nrow = 2,
#'               byrow = TRUE)
#' indice_lr_neg(tab, B = 100)
#'
#' @export
indice_lr_neg <- function(tab,
                          se_method = c("bootstrap", "none"),
                          B = 1000,
                          seed = 123) {
  se_method <- match.arg(se_method)

  sens <- indice_sensibilidad(tab, se_method = "none")$statistic
  spec <- indice_especificidad(tab, se_method = "none")$statistic

  statistic <- safe_divide(1 - sens, spec)

  se <- switch(
    se_method,
    bootstrap = {
      bootstrap_se_2x2(
        tab = tab,
        stat_fun = function(x) {
          sens_x <- indice_sensibilidad(x, se_method = "none")$statistic
          spec_x <- indice_especificidad(x, se_method = "none")$statistic
          safe_divide(1 - sens_x, spec_x)
        },
        B = B,
        seed = seed
      )
    },
    none = NA_real_
  )

  list(statistic = statistic, se = se)
}

#' Calcular el diagnostic odds ratio
#'
#' Calcula el diagnostic odds ratio (DOR), definido como
#' (TP * TN) / (FP * FN).
#'
#' @param tab Matriz o tabla 2x2 con frecuencias observadas.
#' @param se_method Método para calcular el error estándar: "bootstrap" o "none".
#' @param B Número de remuestreos bootstrap.
#' @param seed Semilla para reproducibilidad.
#'
#' @return Lista con statistic y se.
#'
#' @examples
#' tab <- matrix(c(50, 10,
#'                 5, 35),
#'               nrow = 2,
#'               byrow = TRUE)
#' indice_dor(tab, B = 100)
#'
#' @export
indice_dor <- function(tab,
                       se_method = c("bootstrap", "none"),
                       B = 1000,
                       seed = 123) {
  se_method <- match.arg(se_method)

  cells <- get_cells(tab)
  TP <- cells$TP
  TN <- cells$TN
  FP <- cells$FP
  FN <- cells$FN

  statistic <- safe_divide(TP * TN, FP * FN)

  se <- switch(
    se_method,
    bootstrap = {
      bootstrap_se_2x2(
        tab = tab,
        stat_fun = function(x) {
          cx <- get_cells(x)
          safe_divide(cx$TP * cx$TN, cx$FP * cx$FN)
        },
        B = B,
        seed = seed
      )
    },
    none = NA_real_
  )

  list(statistic = statistic, se = se)
}

#' Calcular el coeficiente de correlación de Matthews
#'
#' Calcula el Matthews Correlation Coefficient (MCC), una medida de asociación
#' entre la clasificación del método y el estándar de referencia. Utiliza las
#' cuatro celdas de la tabla 2x2.
#'
#' @param tab Matriz o tabla 2x2 con frecuencias observadas.
#' @param se_method Método para calcular el error estándar: "bootstrap" o "none".
#' @param B Número de remuestreos bootstrap.
#' @param seed Semilla para reproducibilidad.
#'
#' @return Lista con statistic y se.
#'
#' @examples
#' tab <- matrix(c(50, 10,
#'                 5, 35),
#'               nrow = 2,
#'               byrow = TRUE)
#' indice_mcc(tab, B = 100)
#'
#' @export
indice_mcc <- function(tab,
                       se_method = c("bootstrap", "none"),
                       B = 1000,
                       seed = 123) {
  se_method <- match.arg(se_method)

  cells <- get_cells(tab)
  TP <- cells$TP
  FN <- cells$FN
  FP <- cells$FP
  TN <- cells$TN

  numerador <- TP * TN - FP * FN
  denominador <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))

  statistic <- safe_divide(numerador, denominador)

  se <- switch(
    se_method,
    bootstrap = {
      bootstrap_se_2x2(
        tab = tab,
        stat_fun = function(x) {
          cx <- get_cells(x)

          num_x <- cx$TP * cx$TN - cx$FP * cx$FN
          den_x <- sqrt(
            (cx$TP + cx$FP) *
              (cx$TP + cx$FN) *
              (cx$TN + cx$FP) *
              (cx$TN + cx$FN)
          )

          safe_divide(num_x, den_x)
        },
        B = B,
        seed = seed
      )
    },
    none = NA_real_
  )

  list(statistic = statistic, se = se)
}

#' Calcular Yule Q
#'
#' Calcula la medida de asociación Yule Q, definida como
#' (TP * TN - FP * FN) / (TP * TN + FP * FN).
#'
#' @param tab Matriz o tabla 2x2 con frecuencias observadas.
#' @param se_method Método para calcular el error estándar: "bootstrap" o "none".
#' @param B Número de remuestreos bootstrap.
#' @param seed Semilla para reproducibilidad.
#'
#' @return Lista con statistic y se.
#'
#' @examples
#' tab <- matrix(c(50, 10,
#'                 5, 35),
#'               nrow = 2,
#'               byrow = TRUE)
#' indice_yule_q(tab, B = 100)
#'
#' @export
indice_yule_q <- function(tab,
                          se_method = c("bootstrap", "none"),
                          B = 1000,
                          seed = 123) {
  se_method <- match.arg(se_method)

  cells <- get_cells(tab)
  TP <- cells$TP
  TN <- cells$TN
  FP <- cells$FP
  FN <- cells$FN

  ad <- TP * TN
  bc <- FP * FN

  statistic <- safe_divide(ad - bc, ad + bc)

  se <- switch(
    se_method,
    bootstrap = {
      bootstrap_se_2x2(
        tab = tab,
        stat_fun = function(x) {
          cx <- get_cells(x)
          ad_x <- cx$TP * cx$TN
          bc_x <- cx$FP * cx$FN
          safe_divide(ad_x - bc_x, ad_x + bc_x)
        },
        B = B,
        seed = seed
      )
    },
    none = NA_real_
  )

  list(statistic = statistic, se = se)
}

#' Calcular Yule Y
#'
#' Calcula la medida de asociación Yule Y, definida como
#' (sqrt(TP * TN) - sqrt(FP * FN)) /
#' (sqrt(TP * TN) + sqrt(FP * FN)).
#'
#' @param tab Matriz o tabla 2x2 con frecuencias observadas.
#' @param se_method Método para calcular el error estándar: "bootstrap" o "none".
#' @param B Número de remuestreos bootstrap.
#' @param seed Semilla para reproducibilidad.
#'
#' @return Lista con statistic y se.
#'
#' @examples
#' tab <- matrix(c(50, 10,
#'                 5, 35),
#'               nrow = 2,
#'               byrow = TRUE)
#' indice_yule_y(tab, B = 100)
#'
#' @export
indice_yule_y <- function(tab,
                          se_method = c("bootstrap", "none"),
                          B = 1000,
                          seed = 123) {
  se_method <- match.arg(se_method)

  cells <- get_cells(tab)
  TP <- cells$TP
  TN <- cells$TN
  FP <- cells$FP
  FN <- cells$FN

  ad <- TP * TN
  bc <- FP * FN

  statistic <- safe_divide(sqrt(ad) - sqrt(bc), sqrt(ad) + sqrt(bc))

  se <- switch(
    se_method,
    bootstrap = {
      bootstrap_se_2x2(
        tab = tab,
        stat_fun = function(x) {
          cx <- get_cells(x)
          ad_x <- cx$TP * cx$TN
          bc_x <- cx$FP * cx$FN
          safe_divide(sqrt(ad_x) - sqrt(bc_x), sqrt(ad_x) + sqrt(bc_x))
        },
        B = B,
        seed = seed
      )
    },
    none = NA_real_
  )

  list(statistic = statistic, se = se)
}
