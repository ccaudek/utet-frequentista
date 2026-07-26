#!/usr/bin/env Rscript
# =============================================================
#  Controllo dei link interni di un sito Quarto reso.
#
#  Verifica due cose per ogni link relativo:
#    1. il file di destinazione esiste;
#    2. se il link ha un frammento (#ancora), quell'ancora
#       esiste davvero nella pagina di destinazione.
#
#  Portabile: la cartella di output viene dedotta da _quarto.yml,
#  quindi lo stesso file si copia identico in tutti i repository
#  del progetto. Va eseguito dalla radice del progetto.
#
#  Esce con stato 1 se trova problemi, così può essere usato
#  come cancello prima della pubblicazione.
# =============================================================

# ---- individuazione della cartella di output ----------------

site <- Sys.getenv("QUARTO_OUTPUT_DIR", "")

if (!nzchar(site) && file.exists("_quarto.yml")) {
  y <- readLines("_quarto.yml", warn = FALSE)
  m <- grep("^\\s*output-dir\\s*:", y, value = TRUE)
  if (length(m)) site <- trimws(gsub("[\"']", "", sub("^[^:]*:", "", m[1])))
}

if (!nzchar(site) || !dir.exists(site)) {
  for (cand in c("docs", "_book", "_site")) {
    if (dir.exists(cand)) { site <- cand; break }
  }
}

if (!nzchar(site) || !dir.exists(site)) {
  stop("Cartella di output non trovata. Hai reso il sito? ",
       "Puoi forzarla con QUARTO_OUTPUT_DIR=nome Rscript ...")
}

files <- list.files(site, pattern = "\\.html$", recursive = TRUE,
                    full.names = TRUE)
if (!length(files)) stop("Nessun file HTML in '", site, "'.")

# ---- utilità ------------------------------------------------

cache <- new.env(parent = emptyenv())

ids_di <- function(path) {
  if (exists(path, envir = cache)) return(get(path, envir = cache))
  h <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  m <- regmatches(h, gregexpr('id="[^"]+"', h))[[1]]
  v <- unique(sub('"$', "", sub('^id="', "", m)))
  assign(path, v, envir = cache)
  v
}

# risolve ".." e "." senza richiedere che il file esista
normalizza <- function(p) {
  parti <- strsplit(p, "/", fixed = TRUE)[[1]]
  out <- character(0)
  for (x in parti) {
    if (x == "" || x == ".") next
    if (x == "..") { if (length(out)) out <- out[-length(out)] }
    else out <- c(out, x)
  }
  paste(out, collapse = "/")
}

problemi <- data.frame(pagina = character(), link = character(),
                       causa = character(), stringsAsFactors = FALSE)

# ---- scansione ----------------------------------------------

for (f in files) {
  h <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  hrefs <- regmatches(h, gregexpr('href="[^"]*"', h))[[1]]
  hrefs <- unique(sub('"$', "", sub('^href="', "", hrefs)))

  # esclude i placeholder presenti negli script JavaScript generati da Quarto,
  # per esempio: a[href="${href}"].
  # Non sono collegamenti HTML reali e produrrebbero falsi positivi.
  hrefs <- hrefs[!grepl("\\$\\{[^}]+\\}", hrefs)]

  # esclude tutto ciò che non è un link relativo interno
  hrefs <- hrefs[!grepl("^(https?:|mailto:|tel:|javascript:|data:|//)", hrefs)]
  hrefs <- hrefs[nzchar(hrefs) & hrefs != "#"]

  rel_f <- sub(paste0("^", site, "/"), "", f)

  for (a in hrefs) {
    pezzi <- strsplit(a, "#", fixed = TRUE)[[1]]
    perc  <- sub("\\?.*$", "", if (length(pezzi)) pezzi[1] else "")
    frag  <- utils::URLdecode(paste(pezzi[-1], collapse = "#"))

    # link alla stessa pagina
    if (!nzchar(perc)) {
      target <- f
    } else {
      base   <- if (startsWith(perc, "/")) site else dirname(f)
      target <- normalizza(file.path(base, utils::URLdecode(perc)))
      if (dir.exists(target) || endsWith(perc, "/"))
        target <- file.path(target, "index.html")
    }

    if (!file.exists(target)) {
      problemi <- rbind(problemi, data.frame(
        pagina = rel_f, link = a, causa = "destinazione assente",
        stringsAsFactors = FALSE))
      next
    }

    if (nzchar(frag) && grepl("\\.html$", target) &&
        !frag %in% ids_di(target)) {
      problemi <- rbind(problemi, data.frame(
        pagina = rel_f, link = a, causa = "ancora assente",
        stringsAsFactors = FALSE))
    }
  }
}

# ---- esito --------------------------------------------------

if (nrow(problemi)) {
  cat("\n*** LINK INTERNI ROTTI:", nrow(problemi), "occorrenze in '",
      site, "' ***\n\n")
  agg <- aggregate(pagina ~ link + causa, problemi, length)
  names(agg)[3] <- "occorrenze"
  agg <- agg[order(agg$causa, -agg$occorrenze), ]
  print(agg, row.names = FALSE)

  cat("\nEsempio di pagina in cui compare ciascun link:\n")
  primo <- problemi[!duplicated(problemi$link), c("link", "pagina")]
  print(head(primo, 20), row.names = FALSE)
  cat("\n")
  quit(status = 1)
}

cat("Link interni: nessun problema (", length(files), "pagine in '",
    site, "').\n")
