# _common.R — caricato automaticamente da ogni .qmd
# Versione ottimizzata con palette qualitativa e helper didattici

suppressPackageStartupMessages({
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
  library(brms)
  library(rstan)
  library(loo)
  library(posterior)
  library(priorsense)
  library(reliabilitydiag)
  library(ggplot2)
  library(bayesplot)
  library(tidybayes)
  library(ggdist)
  library(patchwork)
  library(systemfonts)
  library(withr)
  library(tinytable)
  library(ragg)
})

set.seed(1234)
RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion")

# Conflitti
conflict_prefer("var", "stats")
conflict_prefer("sd", "stats")
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("chisq.test", "stats")
conflict_prefer("mad", "posterior")
conflict_prefer("rhat", "posterior")
conflict_prefer("ess_bulk", "posterior")
conflict_prefer("ess_tail", "posterior")
conflict_prefer("theme_void", "ggplot2")
conflict_prefer("extract", "tidyr")
conflict_prefer("theme_default", "bayesplot")

options(
  brms.backend = "cmdstanr",
  mc.cores = max(1L, parallel::detectCores(logical = FALSE)),
  pillar.bold = TRUE,
  pillar.subtle = FALSE,
  pillar.width = Inf,
  width = 80,
  scipen = 4,
  digits = 3,
  show.signif.stars = FALSE
)
rstan::rstan_options(auto_write = TRUE)

# --------- Palette coerente con SCSS ---------
PRIMARY <- "#39729E"
TEXT_DARK <- "#1b1f23"
TEXT_MED <- "#2b3137"
TEXT_LIGHT <- "#6C6C6C"
BORDER <- "#e1e4e8"
GRID <- "#f1f3f5"

modern_palette <- list(
  white = "#ffffff",
  off_white = "#fafafa",
  text_dark = TEXT_DARK,
  text_medium = TEXT_MED,
  text_light = TEXT_LIGHT,
  black = "#111111",
  grey1 = "#222222",
  grey2 = "#444444",
  grey3 = "#666666",
  grey4 = "#888888",
  grey5 = "#aaaaaa",
  grey6 = "#cccccc",
  border = BORDER,
  grid = GRID,
  accent = PRIMARY,
  blue = PRIMARY,
  red = "#b25252"
)

# Palette discreta (scala di grigi)
palette_discrete <- c(
  modern_palette$grey1,
  modern_palette$grey2,
  modern_palette$grey3,
  modern_palette$grey4,
  modern_palette$grey5,
  modern_palette$grey6
)

# --------- NUOVO: Palette qualitativa colorblind-friendly ---------
# Basata su Paul Tol's vibrant scheme
palette_qualitative <- c(
  "#E69F00", # Arancione
  "#56B4E9", # Azzurro
  "#009E73", # Verde
  "#F0E442", # Giallo
  "#0072B2", # Blu
  "#D55E00", # Rosso-arancio
  "#CC79A7", # Rosa
  "#999999" # Grigio
)

# Palette per confronti binari (es. controllo vs trattamento)
palette_binary <- c(
  control = modern_palette$grey3,
  treatment = PRIMARY,
  before = modern_palette$grey4,
  after = PRIMARY,
  pre = modern_palette$grey4,
  post = PRIMARY
)

# -------- Font di sistema --------
locate_sans_family <- function() {
  prefer <- c(
    "Helvetica",
    "Source Sans 3",
    "Inter",
    "Source Sans Pro",
    "Roboto",
    "Helvetica Neue",
    "Segoe UI",
    "Arial"
  )
  fams <- unique(systemfonts::system_fonts()$family)
  hit <- Filter(
    function(p) any(grepl(paste0("^", p, "$"), fams, ignore.case = TRUE)),
    prefer
  )
  if (length(hit)) return(hit[[1]])
  "Helvetica"
}
modern_sans <- locate_sans_family()
message("Font sans-serif: ", modern_sans)

# -------- Tema ggplot basato su theme_minimal() --------
apply_visual_theme <- function(base_size = 15) {
  theme_set(
    theme_minimal(base_family = modern_sans, base_size = base_size) +
      theme(
        # Background trasparente per integrazione HTML
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),

        # Griglia sottile
        panel.grid.major = element_line(colour = "#eaeaea", linewidth = 0.4),
        panel.grid.minor = element_line(colour = "#f3f3f3", linewidth = 0.2),

        # Testo
        axis.title = element_text(colour = modern_palette$text_medium),
        axis.text = element_text(colour = modern_palette$text_dark),

        # Strip per facet
        strip.background = element_rect(
          fill = "white",
          colour = modern_palette$border
        ),
        strip.text = element_text(
          face = "bold",
          colour = modern_palette$text_medium
        ),

        # Titoli e caption con spacing migliorato
        plot.title = element_text(
          face = "bold",
          colour = modern_palette$text_dark,
          margin = margin(b = 8)
        ),
        plot.subtitle = element_text(
          colour = modern_palette$text_medium,
          size = rel(0.88),
          margin = margin(b = 10)
        ),
        plot.caption = element_text(
          colour = modern_palette$text_light,
          hjust = 0,
          margin = margin(t = 10)
        ),

        # Margini ariosi
        plot.margin = margin(12, 12, 12, 12)
      )
  )

  # Tema bayesplot per grafici diagnostici
  bayesplot::bayesplot_theme_set(
    bayesplot::theme_default(
      base_family = modern_sans,
      base_size = base_size + 1
    )
  )
  bayesplot::color_scheme_set("blue")

  invisible(TRUE)
}
apply_visual_theme()

# -------- Defaults per geoms --------
update_geom_defaults(
  "point",
  list(size = 2.2, stroke = 0.3, colour = modern_palette$grey3, alpha = 0.9)
)
update_geom_defaults(
  "line",
  list(linewidth = 0.9, colour = modern_palette$grey3, alpha = 0.95)
)
update_geom_defaults(
  "text",
  list(family = modern_sans, colour = modern_palette$text_dark, size = 3.6)
)
update_geom_defaults("label", list(family = modern_sans))
update_geom_defaults("bar", list(linewidth = 0.2, colour = NA))
update_geom_defaults("area", list(fill = modern_palette$grey6, alpha = 0.6))

# -------- Scale helpers --------

# Scala di grigi (originale)
scale_color_modern <- function(..., na.value = "#CCCCCC", drop = FALSE)
  scale_color_manual(
    values = palette_discrete,
    ...,
    na.value = na.value,
    drop = drop
  )

scale_fill_modern <- function(..., na.value = "#CCCCCC", drop = FALSE)
  scale_fill_manual(
    values = palette_discrete,
    ...,
    na.value = na.value,
    drop = drop
  )

# NUOVO: Palette qualitativa per categorie multiple
scale_color_qualitative <- function(..., na.value = "#CCCCCC", drop = FALSE)
  scale_color_manual(
    values = palette_qualitative,
    ...,
    na.value = na.value,
    drop = drop
  )

scale_fill_qualitative <- function(..., na.value = "#CCCCCC", drop = FALSE)
  scale_fill_manual(
    values = palette_qualitative,
    ...,
    na.value = na.value,
    drop = drop
  )

# NUOVO: Palette binaria per confronti (es. controllo vs trattamento)
scale_color_binary <- function(..., na.value = "#CCCCCC")
  scale_color_manual(
    values = palette_binary,
    ...,
    na.value = na.value
  )

scale_fill_binary <- function(..., na.value = "#CCCCCC")
  scale_fill_manual(
    values = palette_binary,
    ...,
    na.value = na.value
  )

# Viridis e divergenti (originali)
scale_color_viridis_modern <- function(...)
  scale_color_viridis_c(option = "plasma", ...)

scale_fill_viridis_modern <- function(...)
  scale_fill_viridis_c(option = "plasma", ...)

scale_color_divergent <- function(...)
  scale_color_gradient2(
    low = modern_palette$grey5,
    mid = "#f7f7f7",
    high = modern_palette$grey1,
    midpoint = 0,
    ...
  )

scale_fill_divergent <- function(...)
  scale_fill_gradient2(
    low = modern_palette$grey5,
    mid = "#f7f7f7",
    high = modern_palette$grey1,
    midpoint = 0,
    ...
  )

# Accent (primario)
scale_color_accent <- function(...)
  scale_color_manual(values = c(modern_palette$accent), ...)

scale_fill_accent <- function(...)
  scale_fill_manual(values = c(modern_palette$accent), ...)

scale_color_primary_then_grey <- function(n_primary = 1, ...) {
  vals <- c(rep(PRIMARY, n_primary), palette_discrete)
  scale_color_manual(values = vals, ...)
}

# -------- Chunk defaults con knitr --------
knitr::opts_chunk$set(
  # Device
  dev = "ragg_png",
  fig.ext = "png",
  dev.args = list(background = "transparent"),

  # Risoluzione (OTTIMIZZATO per web)
  dpi = 150, # Sufficiente per web
  fig.retina = 2, # Effettivo 300 DPI su display retina

  # Dimensioni
  fig.width = 7,
  fig.asp = 0.618, # Golden ratio
  fig.height = 4.33, # 7 * 0.618
  out.width = "85%",
  fig.align = "center",
  fig.show = "hold",

  # Comportamento
  comment = "#>",
  collapse = TRUE,
  message = FALSE,
  warning = FALSE,
  echo = TRUE,
  eval = TRUE,
  error = FALSE,

  # Cache (default off, attivare nei chunk pesanti)
  cache = FALSE,
  cache.lazy = FALSE
)

# -------- tinytable --------
options(
  tinytable_format_num_fmt = "significant_cell",
  tinytable_format_digits = 3,
  tinytable_tt_digits = 3,
  tinytable_theme = "void",
  tinytable_css = paste(
    ":root{--tt-border:",
    BORDER,
    "; --tt-primary:",
    PRIMARY,
    ";}",
    "table.tt{border:1px solid var(--tt-border)}",
    "table.tt thead th{border-bottom:2px solid var(--tt-border);",
    "color:",
    TEXT_MED,
    "; font-weight:600}",
    "table.tt td, table.tt th{border-bottom:1px solid var(--tt-border)}",
    "table.tt caption{color:",
    TEXT_LIGHT,
    ";}"
  )
)

# -------- Helper tema --------
nessuna_griglia <- theme(panel.grid = element_blank())
griglia_sottile_x <- theme(panel.grid.major.y = element_blank())
griglia_sottile_y <- theme(panel.grid.major.x = element_blank())
legenda_in_alto <- theme(legend.position = "top")
legenda_destra <- theme(legend.position = "right")
legenda_nascosta <- theme(legend.position = "none")

formato_italiano <- function(accuracy = 0.01, scale = 1)
  scales::label_number(
    accuracy = accuracy,
    scale = scale,
    decimal.mark = ",",
    big.mark = "."
  )

formato_percentuale_it <- function(accuracy = 1)
  scales::label_percent(accuracy = accuracy, decimal.mark = ",", suffix = "%")

# -------- Annotazioni con colore primario --------
geom_hline_primary <- function(yintercept, ...) {
  geom_hline(
    yintercept = yintercept,
    linewidth = 0.8,
    colour = PRIMARY,
    alpha = 0.9,
    ...
  )
}

geom_vline_primary <- function(xintercept, ...) {
  geom_vline(
    xintercept = xintercept,
    linewidth = 0.8,
    colour = PRIMARY,
    alpha = 0.9,
    ...
  )
}

annotate_primary <- function(...) {
  annotate(..., colour = PRIMARY)
}

# -------- NUOVO: Helper per grafici didattici comuni --------

# Istogramma con densità sovrapposta
geom_hist_density <- function(binwidth = NULL, bins = 30, ...) {
  list(
    geom_histogram(
      aes(y = after_stat(density)),
      binwidth = binwidth,
      bins = bins,
      fill = modern_palette$grey5,
      color = modern_palette$grey3,
      alpha = 0.7,
      ...
    ),
    geom_density(
      linewidth = 1,
      color = PRIMARY,
      ...
    )
  )
}

# QQ plot per normalità
plot_qq_normal <- function(data, var, title = "QQ Plot") {
  ggplot(data, aes(sample = {{ var }})) +
    stat_qq(color = modern_palette$grey3, size = 2, alpha = 0.7) +
    stat_qq_line(color = PRIMARY, linewidth = 1.2) +
    labs(
      title = title,
      x = "Quantili teorici (Normale)",
      y = "Quantili osservati"
    )
}

# Prior vs Posterior (pattern comune in bayesiano)
plot_prior_posterior <- function(
  prior_samples,
  posterior_samples,
  parameter_name = "θ",
  title = "Aggiornamento Bayesiano"
) {
  df <- bind_rows(
    tibble(value = prior_samples, type = "Prior"),
    tibble(value = posterior_samples, type = "Posterior")
  )

  ggplot(df, aes(x = value, fill = type, color = type)) +
    geom_density(alpha = 0.4, linewidth = 1) +
    scale_fill_manual(
      values = c(
        Prior = modern_palette$grey4,
        Posterior = PRIMARY
      )
    ) +
    scale_color_manual(
      values = c(
        Prior = modern_palette$grey3,
        Posterior = PRIMARY
      )
    ) +
    labs(
      title = title,
      x = parameter_name,
      y = "Densità",
      fill = NULL,
      color = NULL
    ) +
    legenda_in_alto
}

# Intervallo di credibilità con distribuzione
plot_credible_interval <- function(
  samples,
  prob = 0.95,
  parameter_name = "θ",
  title = "Distribuzione Posteriori"
) {
  alpha <- 1 - prob
  lower <- quantile(samples, alpha / 2)
  upper <- quantile(samples, 1 - alpha / 2)
  median_val <- median(samples)

  df <- tibble(value = samples)

  ggplot(df, aes(x = value)) +
    geom_density(fill = modern_palette$grey5, alpha = 0.5, linewidth = 0) +
    geom_vline(
      xintercept = median_val,
      color = PRIMARY,
      linewidth = 1.2,
      linetype = "dashed"
    ) +
    geom_vline(
      xintercept = c(lower, upper),
      color = PRIMARY,
      linewidth = 0.8
    ) +
    annotate(
      "rect",
      xmin = lower,
      xmax = upper,
      ymin = -Inf,
      ymax = Inf,
      fill = PRIMARY,
      alpha = 0.15
    ) +
    labs(
      title = title,
      x = parameter_name,
      y = "Densità",
      caption = sprintf(
        "Intervallo di Credibilità %.0f%%: [%.3f, %.3f] | Mediana: %.3f",
        prob * 100,
        lower,
        upper,
        median_val
      )
    )
}

# Confronto distribuzioni (es. normale vs t)
plot_distribution_comparison <- function(
  x_range,
  distributions_list,
  title = "Confronto Distribuzioni"
) {
  # distributions_list è una lista nominata con funzioni densità
  # Esempio: list("Normale" = dnorm, "t(3)" = function(x) dt(x, df=3))

  df <- map_dfr(names(distributions_list), function(name) {
    tibble(
      x = x_range,
      density = distributions_list[[name]](x_range),
      distribution = name
    )
  })

  ggplot(df, aes(x = x, y = density, color = distribution)) +
    geom_line(linewidth = 1.2) +
    scale_color_qualitative() +
    labs(
      title = title,
      x = "x",
      y = "Densità",
      color = "Distribuzione"
    ) +
    legenda_in_alto
}
