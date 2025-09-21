## ─────────────────────────────────────────────────────────────────────
## 1. Pacchetti per manipolazione e struttura dei dati
## ─────────────────────────────────────────────────────────────────────
library(here)
library(rio)
library(tidyr)
library(dplyr)
library(tibble)
library(modelr)
library(matrixStats)
library(janitor)
library(conflicted)
library(sessioninfo)

conflict_prefer("var", "stats")
conflict_prefer("sd", "stats")
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("chisq.test", "stats")

## ─────────────────────────────────────────────────────────────────────
## 2. Pacchetti per analisi bayesiana
## ─────────────────────────────────────────────────────────────────────
library(brms)
library(rstan)
library(loo)
library(posterior)
library(priorsense)
library(reliabilitydiag)

conflict_prefer("mad", "posterior")
conflict_prefer("rhat", "posterior")
conflict_prefer("ess_bulk", "posterior")
conflict_prefer("ess_tail", "posterior")

options(
  brms.backend = "cmdstanr",
  mc.cores = parallel::detectCores(logical = FALSE),
  posterior.num_args = list(digits = 3)
)

## ─────────────────────────────────────────────────────────────────────
## 3. Pacchetti per visualizzazione
## ─────────────────────────────────────────────────────────────────────
library(ggplot2)
library(bayesplot)
library(tidybayes)
library(ggdist)
library(patchwork)

## ─────────────────────────────────────────────────────────────────────
## 4. PALETTE MODERNA COLORBLIND-SAFE
## ─────────────────────────────────────────────────────────────────────

# Palette principale basata su Paul Tol (colorblind-safe)
modern_palette <- list(
  # Colori di base
  white = "#ffffff",
  off_white = "#fafafa",
  text_dark = "#2c3e50",
  text_medium = "#34495e",
  text_light = "#7f8c8d",

  # Palette dati colorblind-safe
  blue = "#4477AA",
  cyan = "#66CCEE",
  green = "#228833",
  yellow = "#CCBB44",
  red = "#EE6677",
  purple = "#AA3377",
  grey = "#BBBBBB",

  # Bordi e elementi UI
  border = "#e1e8ed",
  border_medium = "#bdc3c7",
  grid = "#ecf0f1"
)

# Vettore per scale discrete
palette_discrete <- c(
  modern_palette$blue,
  modern_palette$red,
  modern_palette$green,
  modern_palette$yellow,
  modern_palette$cyan,
  modern_palette$purple,
  modern_palette$grey
)

## ─────────────────────────────────────────────────────────────────────
## 5. TIPOGRAFIA - SANS SERIF
## ─────────────────────────────────────────────────────────────────────

# Localizza il miglior sans serif disponibile
locate_sans_family <- function() {
  sf <- systemfonts::system_fonts()

  prefer <- c(
    "Source Sans 3",
    "Source Sans Pro",
    "Inter",
    "Roboto",
    "Helvetica Neue",
    "Segoe UI",
    "Arial"
  )

  fams <- unique(sf$family)

  for (p in prefer) {
    hit <- fams[grepl(paste0("^", p, "$"), fams, ignore.case = TRUE)]
    if (length(hit) > 0) return(hit[1])
  }

  "sans"
}

modern_sans <- locate_sans_family()
message("Font sans serif selezionato per i grafici: ", modern_sans)

## ─────────────────────────────────────────────────────────────────────
## 6. CONFIGURAZIONE TEMI - Solo bayesplot default con sans
## ─────────────────────────────────────────────────────────────────────

# Applica tema bayesplot default per tutto
ggplot2::theme_set(bayesplot::theme_default(
  base_family = modern_sans,
  base_size = 14
))

bayesplot::bayesplot_theme_set(bayesplot::theme_default(
  base_family = modern_sans,
  base_size = 15
))

# Schema colori bayesplot moderno
bayesplot::color_scheme_set("blue")

## ─────────────────────────────────────────────────────────────────────
## 7. SCALE COLORI MODERNE
## ─────────────────────────────────────────────────────────────────────

scale_color_modern <- function(..., na.value = "#CCCCCC", drop = FALSE) {
  ggplot2::scale_color_manual(
    values = palette_discrete,
    ...,
    na.value = na.value,
    drop = drop
  )
}

scale_fill_modern <- function(..., na.value = "#CCCCCC", drop = FALSE) {
  ggplot2::scale_fill_manual(
    values = palette_discrete,
    ...,
    na.value = na.value,
    drop = drop
  )
}

# Scale continue
scale_color_viridis_modern <- function(...) {
  ggplot2::scale_color_viridis_c(option = "plasma", ...)
}

scale_fill_viridis_modern <- function(...) {
  ggplot2::scale_fill_viridis_c(option = "plasma", ...)
}

# Divergente
scale_color_divergent <- function(...) {
  ggplot2::scale_color_gradient2(
    low = modern_palette$blue,
    mid = modern_palette$white,
    high = modern_palette$red,
    midpoint = 0,
    ...
  )
}

scale_fill_divergent <- function(...) {
  ggplot2::scale_fill_gradient2(
    low = modern_palette$blue,
    mid = modern_palette$white,
    high = modern_palette$red,
    midpoint = 0,
    ...
  )
}

## ─────────────────────────────────────────────────────────────────────
## 8. DEFAULTS GEOMETRICI
## ─────────────────────────────────────────────────────────────────────

update_geom_defaults(
  "point",
  list(
    size = 2.2,
    alpha = 0.8,
    stroke = 0.3,
    color = modern_palette$blue
  )
)

update_geom_defaults(
  "line",
  list(
    linewidth = 0.8,
    color = modern_palette$blue,
    alpha = 0.9
  )
)

update_geom_defaults(
  "text",
  list(
    family = modern_sans,
    color = modern_palette$text_dark,
    size = 3.5
  )
)

update_geom_defaults(
  "bar",
  list(
    fill = modern_palette$blue,
    color = modern_palette$white,
    alpha = 0.8,
    linewidth = 0.2
  )
)

## ─────────────────────────────────────────────────────────────────────
## 9. KNITR - Sfondo bianco esplicito
## ─────────────────────────────────────────────────────────────────────

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  message = FALSE,
  warning = FALSE,
  echo = TRUE,
  eval = TRUE,
  error = FALSE,
  dev = "ragg_png",
  dpi = 200,
  out.width = "85%",
  fig.align = "center",
  fig.asp = 0.618,
  fig.width = 7,
  fig.height = 4.33,
  dev.args = list(
    background = "white" # FONDAMENTALE: sfondo bianco
  ),
  R.options = list(
    digits = 3,
    width = 80,
    scipen = 4
  )
)

## ─────────────────────────────────────────────────────────────────────
## 10. TABELLE
## ─────────────────────────────────────────────────────────────────────

library(tinytable)

options(
  tinytable_format_num_fmt = "significant_cell",
  tinytable_format_digits = 3,
  tinytable_tt_digits = 3,
  tinytable_theme = "void"
)

tabella_moderna <- function(data, caption = NULL, note = NULL) {
  tt(data) %>%
    style_tt(
      family = modern_sans,
      fontsize = "0.9em",
      background = list("tinytable_header" = modern_palette$off_white),
      color = list(
        "tinytable_header" = modern_palette$text_dark,
        "tinytable_body" = modern_palette$text_medium
      ),
      line_color = modern_palette$border,
      line_width = 0.5
    ) %>%
    format_tt(
      digits = 3,
      num_fmt = function(x) format(x, decimal.mark = ",", big.mark = ".")
    ) %>%
    {
      if (!is.null(caption)) {
        group_tt(., j = list(caption = 1:ncol(data)), caption = caption)
      } else .
    } %>%
    {
      if (!is.null(note)) {
        footnote(., note, i = nrow(data))
      } else .
    }
}

## ─────────────────────────────────────────────────────────────────────
## 11. FUNZIONI HELPER
## ─────────────────────────────────────────────────────────────────────

# Modificatori tema semplici
nessuna_griglia <- theme(panel.grid = element_blank())
griglia_sottile_x <- theme(panel.grid.major.y = element_blank())
griglia_sottile_y <- theme(panel.grid.major.x = element_blank())

legenda_in_alto <- theme(legend.position = "top")
legenda_destra <- theme(legend.position = "right")

# Formattazione italiana
formato_italiano <- function(accuracy = 0.01, scale = 1) {
  scales::label_number(
    accuracy = accuracy,
    scale = scale,
    decimal.mark = ",",
    big.mark = "."
  )
}

formato_percentuale_it <- function(accuracy = 1) {
  scales::label_percent(
    accuracy = accuracy,
    decimal.mark = ",",
    suffix = "%"
  )
}

## ─────────────────────────────────────────────────────────────────────
## 12. CONFIGURAZIONI FINALI
## ─────────────────────────────────────────────────────────────────────

library(pillar)
options(
  pillar.negative = FALSE,
  pillar.subtle = FALSE,
  pillar.bold = TRUE,
  pillar.width = Inf,
  width = 80,
  scipen = 4,
  digits = 3,
  show.signif.stars = FALSE
)

conflicts_prefer(ggplot2::theme_void)
