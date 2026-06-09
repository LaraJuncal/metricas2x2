#devtools::install_github("LaraJuncal/metricas2x2", ref = "main")
library(metricas2x2)

#install.packages(c("ggplot2", "tidyr", "dplyr"))


# ============================================================
# simulacion_cap6.R
# Simulación para el Capítulo 6 del TFM
# Lara Juncal Blanco - Máster Estadística Aplicada UGR
# ============================================================
# Asegúrate de tener el paquete cargado:
# devtools::load_all("ruta/a/metricas2x2")
# o bien: devtools::install_github("LaraJuncal/metricas2x2")
#         library(metricas2x2)
# ============================================================
library(ggplot2)
library(tidyr)
library(dplyr)

# ------------------------------------------------------------
# 1. DEFINICIÓN DE ESCENARIOS
# ------------------------------------------------------------

# Tres perfiles de clasificador
clasificadores <- list(
  "Bueno (Sens=0.90, Spec=0.90)"    = c(sens = 0.90, spec = 0.90),
  "Mediocre (Sens=0.70, Spec=0.70)" = c(sens = 0.70, spec = 0.70),
  "Sesgado (Sens=0.95, Spec=0.60)"  = c(sens = 0.95, spec = 0.60)
)

# Tres niveles de prevalencia
prevalencias <- c(0.05, 0.20, 0.50)

# Tamaño muestral fijo
N <- 1000

# ------------------------------------------------------------
# 2. FUNCIÓN PARA CONSTRUIR LA TABLA 2x2
# ------------------------------------------------------------
# A partir de sens, spec y prevalencia construimos la tabla
# de frecuencias esperadas para un N dado.

construir_tabla <- function(sens, spec, prev, N) {
  P  <- round(N * prev)          # total positivos reales
  Nn <- N - P                    # total negativos reales

  TP <- round(sens * P)
  FN <- P - TP
  TN <- round(spec * Nn)
  FP <- Nn - TN

  matrix(c(TP, FN,
           FP, TN),
         nrow = 2, byrow = TRUE)
}

# ------------------------------------------------------------
# 3. CÁLCULO DE MÉTRICAS EN LOS 9 ESCENARIOS
# ------------------------------------------------------------

metricas_interes <- c(
  "accuracy", "balanced_accuracy", "ppv", "npv",
  "f1", "mcc", "kappa", "delta"
)

resultados <- data.frame()

for (nombre_clas in names(clasificadores)) {
  sens <- clasificadores[[nombre_clas]]["sens"]
  spec <- clasificadores[[nombre_clas]]["spec"]

  for (prev in prevalencias) {

    tab <- construir_tabla(sens, spec, prev, N)

    # Calcular todas las métricas con el paquete
    res <- calcular_metricas_tabla(tab, B = 1000, seed = 42)

    # Filtrar las métricas de interés
    res_filtrado <- res[res$metrica %in% metricas_interes, ]

    # Añadir columnas de contexto
    res_filtrado$clasificador  <- nombre_clas
    res_filtrado$prevalencia   <- prev
    res_filtrado$TP <- tab[1, 1]
    res_filtrado$FN <- tab[1, 2]
    res_filtrado$FP <- tab[2, 1]
    res_filtrado$TN <- tab[2, 2]

    resultados <- rbind(resultados, res_filtrado)
  }
}

# ------------------------------------------------------------
# 4. TABLA RESUMEN (para incluir en el TFM)
# ------------------------------------------------------------

tabla_ancha <- resultados %>%
  select(clasificador, prevalencia, metrica, statistic) %>%
  mutate(
    statistic = round(statistic, 3),
    prevalencia = paste0(prevalencia * 100, "%")
  ) %>%
  pivot_wider(names_from = metrica, values_from = statistic) %>%
  arrange(clasificador, prevalencia)

# Reordenar columnas
tabla_ancha <- tabla_ancha %>%
  select(clasificador, prevalencia,
         accuracy, balanced_accuracy, ppv, npv,
         f1, mcc, kappa, delta)

print(tabla_ancha, width = Inf)

# Exportar como CSV para copiar los valores al TFM
write.csv(tabla_ancha, "resultados_simulacion.csv",
          row.names = FALSE)

# ------------------------------------------------------------
# 5. GRÁFICO 1: Accuracy y Balanced Accuracy vs Prevalencia
# ------------------------------------------------------------

datos_graf1 <- resultados %>%
  filter(metrica %in% c("accuracy", "balanced_accuracy")) %>%
  mutate(
    metrica = recode(metrica,
                     "accuracy"          = "Accuracy",
                     "balanced_accuracy" = "Balanced Accuracy"),
    clasificador_label = case_when(
      grepl("Bueno",    clasificador) ~ "Bueno",
      grepl("Mediocre", clasificador) ~ "Mediocre",
      grepl("Sesgado",  clasificador) ~ "Sesgado"
    ),
    clasificador_label = factor(clasificador_label,
                                levels = c("Bueno", "Mediocre", "Sesgado"))
  )

graf1 <- ggplot(datos_graf1,
                aes(x = prevalencia, y = statistic,
                    color = metrica, shape = metrica)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  facet_wrap(~ clasificador_label, nrow = 1) +
  scale_x_continuous(
    breaks = c(0.05, 0.20, 0.50),
    labels = c("5%", "20%", "50%")
  ) +
  scale_color_manual(values = c("Accuracy"          = "#2166ac",
                                "Balanced Accuracy" = "#d6604d")) +
  labs(
    title   = NULL,
    x       = "Prevalencia",
    y       = "Valor de la métrica",
    color   = NULL,
    shape   = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "#edf2f7"),
    strip.text = element_text(face = "bold")
  ) +
  ylim(0, 1)

ggsave("grafico1_accuracy_vs_prevalencia.pdf",
       graf1, width = 8, height = 4)
ggsave("grafico1_accuracy_vs_prevalencia.png",
       graf1, width = 8, height = 4, dpi = 300)

print(graf1)

# ------------------------------------------------------------
# 6. GRÁFICO 2: MCC, Kappa y Delta vs Prevalencia
# ------------------------------------------------------------

datos_graf2 <- resultados %>%
  filter(metrica %in% c("mcc", "kappa", "delta")) %>%
  mutate(
    metrica = recode(metrica,
                     "mcc"   = "MCC",
                     "kappa" = "Kappa de Cohen",
                     "delta" = "Delta"),
    metrica = factor(metrica,
                     levels = c("MCC", "Kappa de Cohen", "Delta")),
    clasificador_label = case_when(
      grepl("Bueno",    clasificador) ~ "Bueno",
      grepl("Mediocre", clasificador) ~ "Mediocre",
      grepl("Sesgado",  clasificador) ~ "Sesgado"
    ),
    clasificador_label = factor(clasificador_label,
                                levels = c("Bueno", "Mediocre", "Sesgado"))
  )

graf2 <- ggplot(datos_graf2,
                aes(x = prevalencia, y = statistic,
                    color = metrica, shape = metrica)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  facet_wrap(~ clasificador_label, nrow = 1) +
  scale_x_continuous(
    breaks = c(0.05, 0.20, 0.50),
    labels = c("5%", "20%", "50%")
  ) +
  scale_color_manual(values = c("MCC"            = "#1a9641",
                                "Kappa de Cohen" = "#d6604d",
                                "Delta"          = "#4393c3")) +
  labs(
    title = NULL,
    x     = "Prevalencia",
    y     = "Valor de la métrica",
    color = NULL,
    shape = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "#edf2f7"),
    strip.text       = element_text(face = "bold")
  )

ggsave("grafico2_mcc_kappa_delta_vs_prevalencia.pdf",
       graf2, width = 8, height = 4)
ggsave("grafico2_mcc_kappa_delta_vs_prevalencia.png",
       graf2, width = 8, height = 4, dpi = 300)

print(graf2)

# ------------------------------------------------------------
# 7. GRÁFICO 3 (opcional): PPV y NPV vs Prevalencia
# ------------------------------------------------------------
# Este gráfico ilustra bien cómo los valores predictivos
# cambian drásticamente con la prevalencia, a diferencia
# de la sensibilidad y la especificidad.

datos_graf3 <- resultados %>%
  filter(metrica %in% c("ppv", "npv")) %>%
  mutate(
    metrica = recode(metrica,
                     "ppv" = "VPP (Precision)",
                     "npv" = "VPN"),
    clasificador_label = case_when(
      grepl("Bueno",    clasificador) ~ "Bueno",
      grepl("Mediocre", clasificador) ~ "Mediocre",
      grepl("Sesgado",  clasificador) ~ "Sesgado"
    ),
    clasificador_label = factor(clasificador_label,
                                levels = c("Bueno", "Mediocre", "Sesgado"))
  )

graf3 <- ggplot(datos_graf3,
                aes(x = prevalencia, y = statistic,
                    color = metrica, shape = metrica)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  facet_wrap(~ clasificador_label, nrow = 1) +
  scale_x_continuous(
    breaks = c(0.05, 0.20, 0.50),
    labels = c("5%", "20%", "50%")
  ) +
  scale_color_manual(values = c("VPP (Precision)" = "#762a83",
                                "VPN"             = "#1b7837")) +
  labs(
    title = NULL,
    x     = "Prevalencia",
    y     = "Valor de la métrica",
    color = NULL,
    shape = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "#edf2f7"),
    strip.text       = element_text(face = "bold")
  ) +
  ylim(0, 1)

ggsave("grafico3_ppv_npv_vs_prevalencia.pdf",
       graf3, width = 8, height = 4)
ggsave("grafico3_ppv_npv_vs_prevalencia.png",
       graf3, width = 8, height = 4, dpi = 300)

print(graf3)

# ------------------------------------------------------------
# 8. MENSAJE FINAL
# ------------------------------------------------------------

cat("\n=== SIMULACIÓN COMPLETADA ===\n")
cat("Archivos generados:\n")
cat("  - resultados_simulacion.csv\n")
cat("  - grafico1_accuracy_vs_prevalencia.pdf / .png\n")
cat("  - grafico2_mcc_kappa_delta_vs_prevalencia.pdf / .png\n")
cat("  - grafico3_ppv_npv_vs_prevalencia.pdf / .png\n")
cat("\nCopia el contenido de resultados_simulacion.csv\n")
cat("y pégalo aquí para redactar el capítulo.\n")
