# metricas2x2

Paquete en R para el cálculo de métricas de clasificación binaria a partir de tablas de contingencia 2x2, incluyendo medidas diagnósticas, métricas derivadas, medidas de asociación y medidas de acuerdo corregidas por azar.

Además, el proyecto incluye una aplicación interactiva desarrollada con Shiny para explorar los resultados de forma visual.

---

# Objetivo

Este paquete se ha desarrollado como parte del Trabajo Fin de Máster:

**Evaluación de modelos de clasificación binaria en diagnóstico médico: revisión de métricas y desarrollo de una herramienta interactiva en R**

El objetivo es disponer de una implementación homogénea de métricas utilizadas en clasificación binaria, incorporando tanto la estimación puntual como el error estándar cuando sea posible.

---

# Convención utilizada

La tabla 2x2 sigue la siguiente convención:

|                | Método + | Método - |
|----------------|----------|----------|
| Estándar +     | TP       | FN       |
| Estándar -     | FP       | TN       |

Donde:

- TP → Verdaderos positivos
- FN → Falsos negativos
- FP → Falsos positivos
- TN → Verdaderos negativos

---

# Estructura del proyecto

```

metricas2x2/
│
├── R/
│ ├── metricas_basicas.R
│ ├── metricas_derivadas.R
│ ├── metricas_clinicas_y_asociacion.R
│ ├── metricas_acuerdo_corregido_azar.R
│ ├── metricas_score_based.R
│ ├── utilidades_globales.R
│ └── auxiliares.R
│
├── shiny/
│ └── app.R
│
├── examples/
│ └── ejemplo_uso.R
│
├── man/
├── README.md
└── DESCRIPTION

```

Descripción:

- **R/** → Implementación de las funciones del paquete.
- **shiny/** → Aplicación interactiva.
- **examples/** → Ejemplos de uso.
- **man/** → Documentación generada automáticamente.

---

# Instalación

Clonar el repositorio:

```r
install.packages("devtools")

devtools::install_github(
  "LaraJuncal/metricas2x2"
)
```

O instalar desde local:

```r
devtools::install()
```

---

# Ejemplo de uso

```r
library(metricas2x2)

tab <- matrix(
c(
50,10,
5,35
),
nrow = 2,
byrow = TRUE
)

resultado <- calcular_metricas_tabla(
tab,
B = 300
)

resultado
```

---

# Aplicación Shiny

La aplicación se encuentra en:

```text
shiny/app.R
```

Para ejecutarla desde RStudio:

```r
shiny::runApp("shiny")
```

o abrir:

```text
shiny/app.R
```

y pulsar:

```text
Run App
```

La aplicación permite:

- Introducir una tabla 2x2.
- Calcular métricas automáticamente.
- Obtener errores estándar bootstrap.
- Consultar visualmente la convención utilizada.

---

# Métricas implementadas

## Métricas básicas

- Sensibilidad
- Especificidad
- FPR
- FNR
- PPV
- NPV
- Accuracy

## Métricas derivadas

- Error rate
- Balanced accuracy
- Youden
- Markedness
- F1

## Asociación clínica

- LR+
- LR−
- DOR
- MCC
- Yule Q
- Yule Y

## Acuerdo corregido por azar

- Kappa
- Scott Pi
- Bennett S
- AC1
- Delta

---

# Validación

Las implementaciones se están contrastando con:

- cálculo manual,
- literatura metodológica,
- paquetes de referencia en R,
- herramientas externas de validación.

---

# Autor

Lara Juncal Blanco

Máster en Estadística Aplicada  
Universidad de Granada