# =========================================================
# 02_metricas_derivadas.R
# Métricas derivadas de probabilidades básicas
# =========================================================

# Convención:
#            Pred+   Pred-
# Real+        TP      FN
# Real-        FP      TN

#' Calcular la balanced accuracy
#'
#' Calcula la balanced accuracy, definida como la media entre la sensibilidad
#' y la especificidad.
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
#' indice_balanced_accuracy(tab)
#'
#' @export
indice_balanced_accuracy <- function(tab,
                                     se_method = c("analytic", "bootstrap", "none"),
                                     B = 1000,
                                     seed = 123) {
  se_method <- match.arg(se_method)
  
  sens <- indice_sensibilidad(tab, se_method = "none")$statistic
  spec <- indice_especificidad(tab, se_method = "none")$statistic
  
  statistic <- mean(c(sens, spec), na.rm = FALSE)
  
  se <- switch(
    se_method,
    analytic = {
      sens_a <- indice_sensibilidad(tab, se_method = "analytic")
      spec_a <- indice_especificidad(tab, se_method = "analytic")
      
      if (any(is.na(c(sens_a$se, spec_a$se)))) {
        NA_real_
      } else {
        sqrt((sens_a$se^2 + spec_a$se^2) / 4)
      }
    },
    bootstrap = {
      bootstrap_se_2x2(
        tab = tab,
        stat_fun = function(x) {
          sens_x <- indice_sensibilidad(x, se_method = "none")$statistic
          spec_x <- indice_especificidad(x, se_method = "none")$statistic
          mean(c(sens_x, spec_x), na.rm = FALSE)
        },
        B = B,
        seed = seed
      )
    },
    none = NA_real_
  )
  
  list(statistic = statistic, se = se)
}

#' Calcular el índice de Youden
#'
#' Calcula el índice de Youden, definido como sensibilidad + especificidad - 1.
#' También se conoce como informedness o bookmaker informedness.
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
#' indice_youden(tab)
#'
#' @export
indice_youden <- function(tab,
                          se_method = c("analytic", "bootstrap", "none"),
                          B = 1000,
                          seed = 123) {
  se_method <- match.arg(se_method)
  
  sens <- indice_sensibilidad(tab, se_method = "none")$statistic
  spec <- indice_especificidad(tab, se_method = "none")$statistic
  
  statistic <- sens + spec - 1
  
  se <- switch(
    se_method,
    analytic = {
      sens_a <- indice_sensibilidad(tab, se_method = "analytic")
      spec_a <- indice_especificidad(tab, se_method = "analytic")
      
      if (any(is.na(c(sens_a$se, spec_a$se)))) {
        NA_real_
      } else {
        sqrt(sens_a$se^2 + spec_a$se^2)
      }
    },
    bootstrap = {
      bootstrap_se_2x2(
        tab = tab,
        stat_fun = function(x) {
          sens_x <- indice_sensibilidad(x, se_method = "none")$statistic
          spec_x <- indice_especificidad(x, se_method = "none")$statistic
          sens_x + spec_x - 1
        },
        B = B,
        seed = seed
      )
    },
    none = NA_real_
  )
  
  list(statistic = statistic, se = se)
}

#' Calcular la markedness
#'
#' Calcula la markedness, definida como PPV + NPV - 1.
#' Es una medida dual del índice de Youden.
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
#' indice_markedness(tab)
#'
#' @export
indice_markedness <- function(tab,
                              se_method = c("analytic", "bootstrap", "none"),
                              B = 1000,
                              seed = 123) {
  se_method <- match.arg(se_method)
  
  ppv <- indice_ppv(tab, se_method = "none")$statistic
  npv <- indice_npv(tab, se_method = "none")$statistic
  
  statistic <- ppv + npv - 1
  
  se <- switch(
    se_method,
    analytic = {
      ppv_a <- indice_ppv(tab, se_method = "analytic")
      npv_a <- indice_npv(tab, se_method = "analytic")
      
      if (any(is.na(c(ppv_a$se, npv_a$se)))) {
        NA_real_
      } else {
        sqrt(ppv_a$se^2 + npv_a$se^2)
      }
    },
    bootstrap = {
      bootstrap_se_2x2(
        tab = tab,
        stat_fun = function(x) {
          ppv_x <- indice_ppv(x, se_method = "none")$statistic
          npv_x <- indice_npv(x, se_method = "none")$statistic
          ppv_x + npv_x - 1
        },
        B = B,
        seed = seed
      )
    },
    none = NA_real_
  )
  
  list(statistic = statistic, se = se)
}

#' Calcular el F1-score
#'
#' Calcula el F1-score, definido como la media armónica entre precision
#' y recall. En términos de la tabla 2x2 puede escribirse como
#' 2TP/(2TP+FP+FN).
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
#' indice_f1(tab, B = 100)
#'
#' @export
indice_f1 <- function(tab,
                      se_method = c("bootstrap", "none"),
                      B = 1000,
                      seed = 123) {
  se_method <- match.arg(se_method)
  
  cells <- get_cells(tab)
  TP <- cells$TP
  FP <- cells$FP
  FN <- cells$FN
  
  statistic <- safe_divide(2 * TP, 2 * TP + FP + FN)
  
  se <- switch(
    se_method,
    bootstrap = {
      bootstrap_se_2x2(
        tab = tab,
        stat_fun = function(x) {
          cx <- get_cells(x)
          safe_divide(2 * cx$TP, 2 * cx$TP + cx$FP + cx$FN)
        },
        B = B,
        seed = seed
      )
    },
    none = NA_real_
  )
  
  list(statistic = statistic, se = se)
}