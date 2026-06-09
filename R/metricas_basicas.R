# =========================================================
# 01_metricas_basicas.R
# Métricas básicas derivadas directamente de la tabla 2x2
# =========================================================

# Convención:
#            Pred+   Pred-
# Real+        TP      FN
# Real-        FP      TN

#' Calcular la sensibilidad
#'
#' Calcula la sensibilidad, también conocida como recall o true positive rate.
#'
#' @param tab Matriz o tabla 2x2 con la convención TP, FN, FP, TN.
#' @param se_method Método para calcular el error estándar: "analytic", "bootstrap" o "none".
#' @param B Número de remuestreos bootstrap.
#' @param seed Semilla para reproducibilidad.
#'
#' @return Una lista con dos elementos: statistic y se.
#'
#' @examples
#' tab <- matrix(c(50, 10,
#'                 5, 35),
#'               nrow = 2,
#'               byrow = TRUE)
#'
#' indice_sensibilidad(tab)
#'
#' @export
indice_sensibilidad <- function(tab,
                                se_method = c("analytic", "bootstrap", "none"),
                                B = 1000,
                                seed = 123) {
  se_method <- match.arg(se_method)

  cells <- get_cells(tab)
  TP <- cells$TP
  FN <- cells$FN

  statistic <- safe_divide(TP, TP + FN)

  se <- switch(
    se_method,
    analytic = {
      if (is.na(statistic)) NA_real_ else sqrt(statistic * (1 - statistic) / (TP + FN))
    },
    bootstrap = {
      bootstrap_se_2x2(
        tab = tab,
        stat_fun = function(x) {
          cx <- get_cells(x)
          safe_divide(cx$TP, cx$TP + cx$FN)
        },
        B = B,
        seed = seed
      )
    },
    none = NA_real_
  )

  list(statistic = statistic, se = se)
}

#' Calcular la especificidad
#'
#' Calcula la especificidad, también conocida como true negative rate.
#'
#' @param tab Matriz o tabla 2x2 con frecuencias observadas.
#' @param se_method Método para calcular el error estándar: "analytic", "bootstrap" o "none".
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
#' indice_especificidad(tab)
#'
#' @export
indice_especificidad <- function(tab,
                                 se_method = c("analytic", "bootstrap", "none"),
                                 B = 1000,
                                 seed = 123) {
  se_method <- match.arg(se_method)

  cells <- get_cells(tab)
  TN <- cells$TN
  FP <- cells$FP

  statistic <- safe_divide(TN, TN + FP)

  se <- switch(
    se_method,
    analytic = {
      if (is.na(statistic)) NA_real_ else sqrt(statistic * (1 - statistic) / (TN + FP))
    },
    bootstrap = {
      bootstrap_se_2x2(
        tab = tab,
        stat_fun = function(x) {
          cx <- get_cells(x)
          safe_divide(cx$TN, cx$TN + cx$FP)
        },
        B = B,
        seed = seed
      )
    },
    none = NA_real_
  )

  list(statistic = statistic, se = se)
}

#' Calcular la tasa de falsos positivos
#'
#' Calcula la false positive rate (FPR), definida como FP/(FP+TN).
#'
#' @param tab Matriz o tabla 2x2 con frecuencias observadas.
#' @param se_method Método para calcular el error estándar: "analytic", "bootstrap" o "none".
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
#' indice_fpr(tab)
#'
#' @export
indice_fpr <- function(tab,
                       se_method = c("analytic", "bootstrap", "none"),
                       B = 1000,
                       seed = 123) {
  se_method <- match.arg(se_method)

  cells <- get_cells(tab)
  FP <- cells$FP
  TN <- cells$TN

  statistic <- safe_divide(FP, FP + TN)

  se <- switch(
    se_method,
    analytic = {
      if (is.na(statistic)) NA_real_ else sqrt(statistic * (1 - statistic) / (FP + TN))
    },
    bootstrap = {
      bootstrap_se_2x2(
        tab = tab,
        stat_fun = function(x) {
          cx <- get_cells(x)
          safe_divide(cx$FP, cx$FP + cx$TN)
        },
        B = B,
        seed = seed
      )
    },
    none = NA_real_
  )

  list(statistic = statistic, se = se)
}

#' Calcular la tasa de falsos negativos
#'
#' Calcula la false negative rate (FNR), definida como FN/(FN+TP).
#'
#' @param tab Matriz o tabla 2x2 con frecuencias observadas.
#' @param se_method Método para calcular el error estándar: "analytic", "bootstrap" o "none".
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
#' indice_fnr(tab)
#'
#' @export
indice_fnr <- function(tab,
                       se_method = c("analytic", "bootstrap", "none"),
                       B = 1000,
                       seed = 123) {
  se_method <- match.arg(se_method)

  cells <- get_cells(tab)
  FN <- cells$FN
  TP <- cells$TP

  statistic <- safe_divide(FN, FN + TP)

  se <- switch(
    se_method,
    analytic = {
      if (is.na(statistic)) NA_real_ else sqrt(statistic * (1 - statistic) / (FN + TP))
    },
    bootstrap = {
      bootstrap_se_2x2(
        tab = tab,
        stat_fun = function(x) {
          cx <- get_cells(x)
          safe_divide(cx$FN, cx$FN + cx$TP)
        },
        B = B,
        seed = seed
      )
    },
    none = NA_real_
  )

  list(statistic = statistic, se = se)
}

#' Calcular el valor predictivo positivo
#'
#' Calcula el valor predictivo positivo (PPV), también conocido como precision.
#'
#' @param tab Matriz o tabla 2x2 con frecuencias observadas.
#' @param se_method Método para calcular el error estándar: "analytic", "bootstrap" o "none".
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
#' indice_ppv(tab)
#'
#' @export
indice_ppv <- function(tab,
                       se_method = c("analytic", "bootstrap", "none"),
                       B = 1000,
                       seed = 123) {
  se_method <- match.arg(se_method)

  cells <- get_cells(tab)
  TP <- cells$TP
  FP <- cells$FP

  statistic <- safe_divide(TP, TP + FP)

  se <- switch(
    se_method,
    analytic = {
      if (is.na(statistic)) NA_real_ else sqrt(statistic * (1 - statistic) / (TP + FP))
    },
    bootstrap = {
      bootstrap_se_2x2(
        tab = tab,
        stat_fun = function(x) {
          cx <- get_cells(x)
          safe_divide(cx$TP, cx$TP + cx$FP)
        },
        B = B,
        seed = seed
      )
    },
    none = NA_real_
  )

  list(statistic = statistic, se = se)
}

#' Calcular el valor predictivo negativo
#'
#' Calcula el valor predictivo negativo (NPV).
#'
#' @param tab Matriz o tabla 2x2 con frecuencias observadas.
#' @param se_method Método para calcular el error estándar: "analytic", "bootstrap" o "none".
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
#' indice_npv(tab)
#'
#' @export
indice_npv <- function(tab,
                       se_method = c("analytic", "bootstrap", "none"),
                       B = 1000,
                       seed = 123) {
  se_method <- match.arg(se_method)

  cells <- get_cells(tab)
  TN <- cells$TN
  FN <- cells$FN

  statistic <- safe_divide(TN, TN + FN)

  se <- switch(
    se_method,
    analytic = {
      if (is.na(statistic)) NA_real_ else sqrt(statistic * (1 - statistic) / (TN + FN))
    },
    bootstrap = {
      bootstrap_se_2x2(
        tab = tab,
        stat_fun = function(x) {
          cx <- get_cells(x)
          safe_divide(cx$TN, cx$TN + cx$FN)
        },
        B = B,
        seed = seed
      )
    },
    none = NA_real_
  )

  list(statistic = statistic, se = se)
}

#' Calcular la exactitud global
#'
#' Calcula la accuracy, definida como (TP+TN)/N.
#'
#' @param tab Matriz o tabla 2x2 con frecuencias observadas.
#' @param se_method Método para calcular el error estándar: "analytic", "bootstrap" o "none".
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
#' indice_accuracy(tab)
#'
#' @export
indice_accuracy <- function(tab,
                            se_method = c("analytic", "bootstrap", "none"),
                            B = 1000,
                            seed = 123) {
  se_method <- match.arg(se_method)

  cells <- get_cells(tab)
  N <- get_N(tab)

  statistic <- safe_divide(cells$TP + cells$TN, N)

  se <- switch(
    se_method,
    analytic = {
      if (is.na(statistic)) NA_real_ else sqrt(statistic * (1 - statistic) / N)
    },
    bootstrap = {
      bootstrap_se_2x2(
        tab = tab,
        stat_fun = function(x) {
          cx <- get_cells(x)
          Nx <- get_N(x)
          safe_divide(cx$TP + cx$TN, Nx)
        },
        B = B,
        seed = seed
      )
    },
    none = NA_real_
  )

  list(statistic = statistic, se = se)
}

#' Calcular la tasa de error
#'
#' Calcula la error rate o misclassification rate, definida como (FP+FN)/N.
#'
#' @param tab Matriz o tabla 2x2 con frecuencias observadas.
#' @param se_method Método para calcular el error estándar: "analytic", "bootstrap" o "none".
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
#' indice_error_rate(tab)
#'
#' @export
indice_error_rate <- function(tab,
                              se_method = c("analytic", "bootstrap", "none"),
                              B = 1000,
                              seed = 123) {
  se_method <- match.arg(se_method)

  cells <- get_cells(tab)
  N <- get_N(tab)

  statistic <- safe_divide(cells$FP + cells$FN, N)

  se <- switch(
    se_method,
    analytic = {
      if (is.na(statistic)) NA_real_ else sqrt(statistic * (1 - statistic) / N)
    },
    bootstrap = {
      bootstrap_se_2x2(
        tab = tab,
        stat_fun = function(x) {
          cx <- get_cells(x)
          Nx <- get_N(x)
          safe_divide(cx$FP + cx$FN, Nx)
        },
        B = B,
        seed = seed
      )
    },
    none = NA_real_
  )

  list(statistic = statistic, se = se)
}
