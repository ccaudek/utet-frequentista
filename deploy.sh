#!/usr/bin/env bash
# =============================================================
#  Deploy del sito Quarto UTET Frequentista.
#
#  Flusso:
#    1. verifica repository, configurazione e strumenti;
#    2. pulizia completa dell'output precedente;
#    3. rendering Quarto;
#    4. verifica delle pagine attese;
#    5. controllo dei link interni;
#    6. commit/push dei sorgenti e dell'output;
#    7. pubblicazione su gh-pages.
# =============================================================
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ROOT"

MSG="${1:-aggiornamento del sito utet-frequentista}"

EXPECTED_SLUG="utet-frequentista"
EXPECTED_SITE_URL="https://ccaudek.github.io/utet-frequentista/"
EXPECTED_REPO_FRAGMENT="ccaudek/utet-frequentista"

YML="_quarto.yml"
INDEX="index.qmd"
LINK_CHECK="R/check_link.R"

fail() {
  printf 'ERRORE: %s\n' "$*" >&2
  exit 1
}

need_cmd() {
  command -v "$1" >/dev/null 2>&1 ||
    fail "comando richiesto non trovato: $1"
}

read_yaml_scalar() {
  local key="$1"
  awk -v key="$key" '
    $0 ~ "^[[:space:]]*" key "[[:space:]]*:" {
      sub("^[^:]*:[[:space:]]*", "", $0)
      gsub(/[\047\042]/, "", $0)
      sub(/[[:space:]]+#.*$/, "", $0)
      gsub(/^[[:space:]]+|[[:space:]]+$/, "", $0)
      print
      exit
    }
  ' "$YML"
}

printf '→ verifica ambiente e identità del progetto\n'

need_cmd quarto
need_cmd Rscript
need_cmd git
need_cmd ghp-import
need_cmd awk
need_cmd find

test -f "$YML" || fail "manca $YML"
test -f "$INDEX" || fail "manca $INDEX"
test -f "$LINK_CHECK" || fail "manca $LINK_CHECK"

git rev-parse --is-inside-work-tree >/dev/null 2>&1 ||
  fail "la directory non è un repository Git"

SITE_URL="$(read_yaml_scalar site-url)"
REPO_URL="$(read_yaml_scalar repo-url)"
OUT="$(read_yaml_scalar output-dir)"
OUT="${OUT:-docs}"

case "$OUT" in
  ""|"/"|"."|".."|../*|/*)
    fail "output-dir non sicura: '$OUT'"
    ;;
esac

[[ "$SITE_URL" == "$EXPECTED_SITE_URL" ]] ||
  fail "site-url inatteso: '${SITE_URL:-assente}'"

[[ "$REPO_URL" == *"$EXPECTED_REPO_FRAGMENT"* ]] ||
  fail "repo-url non coerente con $EXPECTED_SLUG: '${REPO_URL:-assente}'"

ORIGIN_URL="$(git remote get-url origin 2>/dev/null || true)"
[[ -n "$ORIGIN_URL" ]] ||
  fail "remote Git 'origin' non configurato"

[[ "$ORIGIN_URL" == *"$EXPECTED_REPO_FRAGMENT"* ]] ||
  fail "remote origin non coerente con $EXPECTED_SLUG: '$ORIGIN_URL'"

grep -Eqi 'inferenza[[:space:]]+frequentista|modulo[[:space:]]+sull.inferenza[[:space:]]+frequentista' "$INDEX" ||
  fail "index.qmd non sembra appartenere al sito frequentista"

# Il riferimento a utet-prob nel link-external-filter è intenzionale:
# non va trattato come errore di identità del sito.

printf '→ pulizia completa di %s\n' "$OUT"
rm -rf -- "$OUT"

printf '→ render\n'
quarto render --clean

printf '→ verifica output\n'
test -d "$OUT" ||
  fail "Quarto non ha creato la directory $OUT"

PAGINE_ATTESE=(
  "$OUT/index.html"
  "$OUT/chapters/frequentist_inference/introduction_frequentist_inference.html"
  "$OUT/chapters/frequentist_inference/01_galton.html"
  "$OUT/chapters/frequentist_inference/02_stime_parametri.html"
  "$OUT/chapters/frequentist_inference/03_conf_interv.html"
  "$OUT/chapters/frequentist_inference/04_test_ipotesi.html"
  "$OUT/chapters/frequentist_inference/05_sample_size.html"
  "$OUT/chapters/frequentist_inference/06_two_ind_samples.html"
  "$OUT/chapters/epilogo/epilogo.html"
)

for pagina in "${PAGINE_ATTESE[@]}"; do
  test -f "$pagina" ||
    fail "pagina attesa non generata: $pagina"
done

HTML_COUNT="$(find "$OUT" -type f -name '*.html' | wc -l | tr -d '[:space:]')"
[[ "$HTML_COUNT" =~ ^[0-9]+$ ]] ||
  fail "impossibile contare le pagine HTML"

(( HTML_COUNT >= ${#PAGINE_ATTESE[@]} )) ||
  fail "output incompleto: trovate $HTML_COUNT pagine HTML"

grep -qi "inferenza frequentista" "$OUT/index.html" ||
  fail "la homepage generata non contiene il titolo del sito"

printf '→ controllo link interni\n'
Rscript "$LINK_CHECK"

printf '→ commit dei sorgenti e dell’output\n'
git add -A

if git diff --cached --quiet; then
  printf '  (niente da committare)\n'
else
  git commit -m "$MSG"
fi

git push

printf '→ pubblicazione di %s su gh-pages\n' "$OUT"
ghp-import -n -p -f "$OUT"

printf 'Fatto. Pubblicato %s.\n' "$EXPECTED_SLUG"
