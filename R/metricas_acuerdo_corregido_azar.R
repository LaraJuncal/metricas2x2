# =========================================================
# 04_metricas_acuerdo_corregido_azar.R
# Métricas de acuerdo corregidas por azar
# =========================================================

# Convención:
#            Pred+   Pred-
# Real+        TP      FN
# Real-        FP      TN

#' Calcular el coeficiente Kappa de Cohen
#'
#' Calcula el coeficiente Kappa de Cohen, una medida de acuerdo corregida
#' por azar entre el método evaluado y el estándar de referencia.
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
#' indice_kappa(tab, B = 100)
#'
#' @export
indice_kappa <- function(tab,
                         se_method = c("bootstrap", "none"),
                         B = 1000,
                         seed = 123) {

  se_method <- match.arg(se_method)

  cells <- get_cells(tab)

  TP <- cells$TP
  FN <- cells$FN
  FP <- cells$FP
  TN <- cells$TN

  N <- get_N(tab)

  pa <- safe_divide(TP + TN, N)

  pe <- safe_divide(
    (TP + FN) * (TP + FP) +
      (TN + FP) * (TN + FN),
    N^2
  )

  statistic <- safe_divide(pa - pe, 1 - pe)

  se <- switch(
    se_method,

    bootstrap = {

      bootstrap_se_2x2(

        tab,

        stat_fun = function(x){

          cx <- get_cells(x)

          Nx <- get_N(x)

          pa_x <- safe_divide(
            cx$TP + cx$TN,
            Nx
          )

          pe_x <- safe_divide(
            (cx$TP + cx$FN)*(cx$TP + cx$FP)+
              (cx$TN + cx$FP)*(cx$TN + cx$FN),
            Nx^2
          )

          safe_divide(
            pa_x - pe_x,
            1 - pe_x
          )

        },

        B = B,
        seed = seed

      )

    },

    none = NA_real_

  )

  list(
    statistic = statistic,
    se = se
  )

}


#' Calcular Scott Pi
#'
#' Calcula Scott Pi, una medida de acuerdo corregida por azar basada en una
#' estimación común de la prevalencia marginal.
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
#' indice_scott_pi(tab, B = 100)
#'
#' @export
indice_scott_pi <- function(tab,
                            se_method = c("bootstrap","none"),
                            B=1000,
                            seed=123){

  se_method <- match.arg(se_method)

  cells <- get_cells(tab)

  TP <- cells$TP
  FN <- cells$FN
  FP <- cells$FP
  TN <- cells$TN

  N <- get_N(tab)

  pa <- safe_divide(
    TP+TN,
    N
  )

  p <- safe_divide(
    2*TP+FP+FN,
    2*N
  )

  pe <- p^2 + (1-p)^2

  statistic <- safe_divide(
    pa-pe,
    1-pe
  )

  se <- switch(

    se_method,

    bootstrap={

      bootstrap_se_2x2(

        tab,

        stat_fun=function(x){

          cx<-get_cells(x)

          Nx<-get_N(x)

          pa_x<-safe_divide(
            cx$TP+cx$TN,
            Nx
          )

          p_x<-safe_divide(
            2*cx$TP+cx$FP+cx$FN,
            2*Nx
          )

          pe_x<-p_x^2+(1-p_x)^2

          safe_divide(
            pa_x-pe_x,
            1-pe_x
          )

        },

        B=B,
        seed=seed

      )

    },

    none=NA_real_

  )

  list(
    statistic=statistic,
    se=se
  )

}


#' Calcular Bennett S
#'
#' Calcula Bennett S, también relacionado con Brennan-Prediger y PABAK
#' en el caso de dos categorías.
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
#' indice_bennett_s(tab, B = 100)
#'
#' @export
indice_bennett_s <- function(tab,
                             se_method=c("bootstrap","none"),
                             B=1000,
                             seed=123){

  se_method <- match.arg(se_method)

  pa <- indice_accuracy(
    tab,
    se_method="none"
  )$statistic

  statistic <- 2*pa-1

  se <- switch(

    se_method,

    bootstrap={

      bootstrap_se_2x2(

        tab,

        stat_fun=function(x){

          pa_x<-
            indice_accuracy(
              x,
              se_method="none"
            )$statistic

          2*pa_x-1

        },

        B=B,
        seed=seed

      )

    },

    none=NA_real_

  )

  list(
    statistic=statistic,
    se=se
  )

}


#' Calcular AC1 de Gwet
#'
#' Calcula AC1 de Gwet, una medida de acuerdo corregida por azar alternativa
#' al Kappa de Cohen.
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
#' indice_ac1(tab, B = 100)
#'
#' @export
indice_ac1 <- function(tab,
                       se_method=c("bootstrap","none"),
                       B=1000,
                       seed=123){

  se_method <- match.arg(se_method)

  cells <- get_cells(tab)

  TP <- cells$TP
  FN <- cells$FN
  FP <- cells$FP
  TN <- cells$TN

  N <- get_N(tab)

  pa <- safe_divide(
    TP+TN,
    N
  )

  p <- safe_divide(
    2*TP+FP+FN,
    2*N
  )

  pe <- 2*p*(1-p)

  statistic <- safe_divide(
    pa-pe,
    1-pe
  )

  se <- switch(

    se_method,

    bootstrap={

      bootstrap_se_2x2(

        tab,

        stat_fun=function(x){

          cx<-get_cells(x)

          Nx<-get_N(x)

          pa_x<-safe_divide(
            cx$TP+cx$TN,
            Nx
          )

          p_x<-safe_divide(
            2*cx$TP+cx$FP+cx$FN,
            2*Nx
          )

          pe_x<-2*p_x*(1-p_x)

          safe_divide(
            pa_x-pe_x,
            1-pe_x
          )

        },

        B=B,
        seed=seed

      )

    },

    none=NA_real_

  )

  list(
    statistic=statistic,
    se=se
  )

}


#' Calcular el índice Delta
#'
#' Calcula el índice Delta en su aproximación asintótica para una tabla 2x2.
#'
#' Con la convención utilizada en el paquete:
#'
#' \deqn{
#' \Delta = \frac{TP + TN - 2\sqrt{FN \cdot FP}}{N}
#' }
#'
#' donde N es el total de observaciones de la tabla.
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
#' indice_delta(tab, B = 100)
#'
#' @export
indice_delta <- function(tab,
                         se_method = c("bootstrap", "none"),
                         B = 1000,
                         seed = 123) {
  
  se_method <- match.arg(se_method)
  
  cells <- get_cells(tab)
  
  TP <- cells$TP
  FN <- cells$FN
  FP <- cells$FP
  TN <- cells$TN
  
  N <- get_N(tab)
  
  statistic <- safe_divide(
    TP + TN - 2 * sqrt(FN * FP),
    N
  )
  
  se <- switch(
    se_method,
    
    bootstrap = {
      bootstrap_se_2x2(
        tab,
        stat_fun = function(x) {
          
          cx <- get_cells(x)
          Nx <- get_N(x)
          
          safe_divide(
            cx$TP + cx$TN - 2 * sqrt(cx$FN * cx$FP),
            Nx
          )
        },
        B = B,
        seed = seed
      )
    },
    
    none = NA_real_
  )
  
  list(
    statistic = statistic,
    se = se
  )
}
