#!/usr/bin/env Rscript
# =============================================================
#  Audit dei capitoli contro il template canonico.
#  Vedi TEMPLATE-CAPITOLO.md per il canone di riferimento.
#
#  Legge i sorgenti .qmd (non l'HTML reso) e riporta:
#    1. quali blocchi canonici mancano, capitolo per capitolo;
#    2. i blocchi presenti ma fuori ordine;
#    3. i callout con tipo o titolo anomalo;
#    4. i rimandi @sec-/@fig-/@tbl- che non corrispondono ad
#       alcuna etichetta definita nel progetto;
#    5. quale capitolo è più vicino al canone (candidato come
#       modello da cui copiare).
#
#  Solo base R, nessuna dipendenza. Da eseguire dalla radice.
#  Esce sempre con stato 0: è una diagnosi, non un cancello.
#  Con --strict esce con 1 se ci sono rimandi irrisolti.
# =============================================================

args   <- commandArgs(trailingOnly = TRUE)
strict <- "--strict" %in% args

radice <- if (dir.exists("chapters")) "chapters" else "."
files  <- list.files(radice, pattern = "\\.qmd$", recursive = TRUE,
                     full.names = TRUE)
files  <- files[!grepl("/_", files)]   # esclude i partial
if (!length(files)) stop("Nessun .qmd trovato sotto '", radice, "'.")

# ---- lettura strutturata di un .qmd -------------------------
# Restituisce il testo con: front matter rimosso, corpo dei chunk
# rimosso (tranne le righe di opzione #|), e la profondità dei div.

leggi <- function(path) {
  L <- readLines(path, warn = FALSE, encoding = "UTF-8")

  if (length(L) && grepl("^---\\s*$", L[1])) {
    chiusure <- which(grepl("^---\\s*$", L))
    if (length(chiusure) >= 2) L[1:chiusure[2]] <- ""
  }

  fence  <- grepl("^\\s*```", L)
  dentro <- (cumsum(fence) %% 2) == 1
  in_code <- dentro & !fence

  testo <- L
  testo[in_code & !grepl("^\\s*#\\|", L)] <- ""
  testo[fence] <- ""

  # profondità dei div ::: (per non contare i titoli dentro i callout)
  apre  <- grepl("^:::+\\s*(\\{|[A-Za-z])", L) & !in_code
  chiude <- grepl("^:::+\\s*$", L) & !in_code
  prof <- cumsum(apre) - cumsum(chiude)
  prof <- prof - as.integer(apre)   # la riga di apertura sta ancora a 0

  list(raw = L, testo = testo, prof = prof, in_code = in_code)
}

# ---- estrazione dei titoli di sezione -----------------------

titoli <- function(d) {
  idx <- grep("^#{1,6}\\s+\\S", d$testo)
  idx <- idx[d$prof[idx] == 0]          # solo fuori dai callout
  if (!length(idx)) return(NULL)
  grezzi <- d$testo[idx]
  data.frame(
    riga    = idx,
    livello = nchar(sub("^(#+).*$", "\\1", grezzi)),
    testo   = trimws(sub("\\{[^}]*\\}\\s*$", "",
                         sub("^#+\\s+", "", grezzi))),
    attrib  = ifelse(grepl("\\{[^}]*\\}\\s*$", grezzi),
                     sub("^.*(\\{[^}]*\\})\\s*$", "\\1", grezzi), ""),
    stringsAsFactors = FALSE
  )
}

# ---- estrazione dei callout ---------------------------------

callout <- function(d) {
  idx <- grep("^:::+\\s*\\{?\\s*\\.callout-", d$raw)
  idx <- idx[!d$in_code[idx]]
  if (!length(idx)) return(data.frame(tipo = character(), titolo = character(),
                                      collapse = logical(), riga = integer()))
  tipo <- sub("^.*\\.callout-([a-z]+).*$", "\\1", d$raw[idx])
  tit  <- rep("", length(idx))
  ha_t <- grepl('title\\s*=\\s*"', d$raw[idx])
  tit[ha_t] <- sub('^.*title\\s*=\\s*"([^"]*)".*$', "\\1", d$raw[idx][ha_t])
  for (k in which(!ha_t)) {            # titolo come heading sulla riga dopo
    for (j in (idx[k] + 1):min(idx[k] + 3, length(d$raw))) {
      if (grepl("^#{1,6}\\s+\\S", d$raw[j])) {
        tit[k] <- trimws(sub("^#+\\s+", "", d$raw[j])); break
      }
      if (nzchar(trimws(d$raw[j]))) break
    }
  }
  data.frame(tipo = tipo, titolo = tit,
             collapse = grepl("collapse", d$raw[idx]),
             riga = idx, stringsAsFactors = FALSE)
}

# ---- il canone ----------------------------------------------
# nome | come riconoscerlo | dove (sezione o callout) | obbligatorio

canone <- list(
  list(n = "Sintesi",      p = "in sintesi|da saper fare|obiettiv", w = "callout", req = TRUE),
  list(n = "Introduzione", p = "^introduzione",                     w = "sez",     req = TRUE),
  list(n = "Prerequisiti", p = "prerequisit",                       w = "callout", req = TRUE),
  list(n = "Panoramica",   p = "panoramica",                        w = "sez",     req = TRUE),
  list(n = "Setup",        p = "preparazione del notebook|setup",   w = "callout", req = TRUE),
  list(n = "Conclusioni",  p = "riflessioni conclusive",            w = "sez",     req = TRUE),
  list(n = "Esercizi",     p = "^esercizi",                         w = "sez",     req = TRUE),
  list(n = "Soluzioni",    p = "soluzion",                          w = "callout", req = TRUE),
  list(n = "PuntiChiave",  p = "punti chiave",                      w = "callout", req = TRUE),
  list(n = "SessionInfo",  p = "ambiente di sviluppo|sessioninfo",  w = "callout", req = TRUE),
  list(n = "Bibliografia", p = "^bibliografia",                     w = "sez",     req = FALSE)
)
ordine_atteso <- c("Introduzione", "Panoramica", "Conclusioni",
                   "Esercizi", "Bibliografia")

# deroghe: le appendici non hanno percorso didattico
esenti_appendice <- c("Panoramica", "Esercizi", "Soluzioni", "Conclusioni")

# ---- scansione ----------------------------------------------

etichette <- character(0)
rimandi   <- data.frame(file = character(), ref = character(),
                        stringsAsFactors = FALSE)
righe     <- list()
anomalie  <- data.frame(file = character(), problema = character(),
                        dettaglio = character(), stringsAsFactors = FALSE)

pref <- "sec|fig|tbl|eq|thm|lem|cor|prp|def|exm|exr"

for (f in files) {
  d  <- leggi(f)
  h  <- titoli(d)
  cl <- callout(d)
  app <- grepl("appendix", f)
  nome <- sub("\\.qmd$", "", basename(f))

  # etichette definite
  et <- regmatches(d$testo, gregexpr(
    paste0("\\{#(", pref, ")-[^} \t]+"), d$testo))
  et <- sub("^\\{#", "", unlist(et))
  et2 <- regmatches(d$testo, gregexpr(
    paste0("#\\|\\s*label:\\s*(", pref, ")-\\S+"), d$testo))
  et2 <- sub("^#\\|\\s*label:\\s*", "", unlist(et2))
  etichette <- c(etichette, et, et2)

  # rimandi usati
  rf <- regmatches(d$testo, gregexpr(
    paste0("@(", pref, ")-[A-Za-z0-9_-]+"), d$testo))
  rf <- sub("^@", "", unlist(rf))
  if (length(rf)) rimandi <- rbind(rimandi,
    data.frame(file = nome, ref = unique(rf), stringsAsFactors = FALSE))

  # presenza dei blocchi
  stato <- setNames(rep("-", length(canone)), sapply(canone, `[[`, "n"))
  pos   <- setNames(rep(NA_integer_, length(canone)), names(stato))
  for (b in canone) {
    fonte <- if (b$w == "sez") (if (is.null(h)) character(0) else h$testo)
             else cl$titolo
    hit <- grep(b$p, fonte, ignore.case = TRUE)
    if (length(hit)) {
      stato[b$n] <- "si"
      pos[b$n] <- if (b$w == "sez") h$riga[hit[1]] else cl$riga[hit[1]]
    } else if (app && b$n %in% esenti_appendice) {
      stato[b$n] <- "na"
    } else if (!b$req) {
      stato[b$n] <- "opz"
    }
  }
  righe[[nome]] <- c(list(file = nome), as.list(stato))

  # ordine dei blocchi presenti
  seq_pres <- ordine_atteso[!is.na(pos[ordine_atteso])]
  p <- pos[seq_pres]
  if (length(p) > 1 && is.unsorted(p)) {
    anomalie <- rbind(anomalie, data.frame(file = nome,
      problema = "ordine dei blocchi",
      dettaglio = paste(seq_pres[order(p)], collapse = " < "),
      stringsAsFactors = FALSE))
  }

  # callout: tipo e collapse
  bad <- cl[grepl("preparazione del notebook|setup", cl$titolo, ignore.case = TRUE) &
            cl$tipo != "note", ]
  if (nrow(bad)) anomalie <- rbind(anomalie, data.frame(file = nome,
    problema = "callout di setup con tipo non canonico",
    dettaglio = paste0("callout-", bad$tipo[1]), stringsAsFactors = FALSE))

  lunghi <- cl[grepl("soluzion|punti chiave|ambiente di sviluppo|sessioninfo",
                     cl$titolo, ignore.case = TRUE) & !cl$collapse, ]
  if (nrow(lunghi)) anomalie <- rbind(anomalie, data.frame(file = nome,
    problema = "callout consultabile senza collapse",
    dettaglio = paste(lunghi$titolo, collapse = "; "), stringsAsFactors = FALSE))

  # etichetta sul titolo di capitolo
  if (!is.null(h) && any(h$livello == 1)) {
    h1 <- h[h$livello == 1, ][1, ]
    if (!grepl("#sec-", h1$attrib)) anomalie <- rbind(anomalie,
      data.frame(file = nome, problema = "titolo di capitolo senza etichetta",
                 dettaglio = h1$testo, stringsAsFactors = FALSE))
  }
}

# ---- referto ------------------------------------------------

mat <- do.call(rbind, lapply(righe, function(r) as.data.frame(r,
                stringsAsFactors = FALSE)))
rownames(mat) <- NULL

cat("\n=========== CONFORMITA' AL TEMPLATE ===========\n")
cat("si = presente | - = ASSENTE | opz = facoltativo | na = non pertinente\n\n")
print(mat, row.names = FALSE)

conta <- rowSums(mat[, -1] == "si")
mancanti <- rowSums(mat[, -1] == "-")
cat("\nBlocchi mancanti per capitolo:\n")
riep <- data.frame(file = mat$file, presenti = conta, mancanti = mancanti)
print(riep[order(riep$mancanti, -riep$presenti), ], row.names = FALSE)

migliore <- riep$file[order(riep$mancanti, -riep$presenti)][1]
cat("\nCapitolo piu' vicino al canone (candidato come modello):",
    migliore, "\n")

if (nrow(anomalie)) {
  cat("\n=========== ANOMALIE ===========\n\n")
  print(anomalie[order(anomalie$problema, anomalie$file), ], row.names = FALSE)
}

etichette <- unique(etichette)
irrisolti <- rimandi[!(rimandi$ref %in% etichette), ]
if (nrow(irrisolti)) {
  cat("\n=========== RIMANDI IRRISOLTI ===========\n")
  cat("Riferimenti @... che non corrispondono a nessuna etichetta del progetto.\n\n")
  agg <- aggregate(file ~ ref, irrisolti, function(x) paste(unique(x), collapse = ", "))
  names(agg) <- c("rimando", "usato in")
  print(agg, row.names = FALSE)
} else {
  cat("\nRimandi: tutti risolti (", length(etichette), "etichette definite ).\n")
}

cat("\n")
if (strict && nrow(irrisolti)) quit(status = 1)
