# ============================================================
#  Valores de mercado Transfermarkt 2025-26  (TFG) -- SOLO DESCARGA
#  Descarga las 5 grandes ligas (temporada 2025-26, con ascendidos)
#  y guarda valores_transfermarkt.csv para que lo analices ANTES
#  de cruzarlo con el master.
#
#  REQUISITO (hazlo una vez en la CONSOLA y reinicia R):
#     install.packages("remotes")
#     remotes::install_github("JaseZiv/worldfootballR", upgrade = "never")
#     # luego: Session > Restart R   (no necesitas Rtools)
#  Despues abre este fichero y pulsa "Source".
# ============================================================

## 0) Paquetes -------------------------------------------------
need <- c("worldfootballR","dplyr","stringr","readr")
for (p in need) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressPackageStartupMessages({
  library(worldfootballR); library(dplyr); library(stringr); library(readr)
})
cat("worldfootballR version:", as.character(packageVersion("worldfootballR")), "\n\n")
options(scipen = 999)   # numeros completos, sin notacion cientifica (1.5e+07 -> 15000000)

## 1) Descargar las 5 ligas 2025-26 (URL directa = incluye ascendidos)
TEMPORADA <- 2025
ligas <- list(
  Spain   = "https://www.transfermarkt.com/laliga/startseite/wettbewerb/ES1",
  England = "https://www.transfermarkt.com/premier-league/startseite/wettbewerb/GB1",
  Italy   = "https://www.transfermarkt.com/serie-a/startseite/wettbewerb/IT1",
  Germany = "https://www.transfermarkt.com/bundesliga/startseite/wettbewerb/L1",
  France  = "https://www.transfermarkt.com/ligue-1/startseite/wettbewerb/FR1"
)
get_one <- function(pais){
  cat("  Descargando", pais, "2025-26 ...\n")
  out <- tryCatch(
    tm_player_market_values(country_name = "", start_year = TEMPORADA,
                            league_url = ligas[[pais]]),
    error = function(e){ cat("   ERROR en", pais, ":", conditionMessage(e), "\n"); NULL })
  if (!is.null(out)) out$liga <- pais
  Sys.sleep(3)
  out
}
valores_raw <- bind_rows(lapply(names(ligas), get_one))
cat("\nFilas totales descargadas:", nrow(valores_raw), "\n")
cat("Columnas que devuelve la funcion:\n"); print(names(valores_raw))

## 2) Estandarizar columnas (tolerante a variaciones) ----------
pick <- function(df, cands, pat = NULL){
  nm <- names(df); h <- nm[tolower(nm) %in% tolower(cands)]
  if (!length(h) && !is.null(pat)) h <- nm[str_detect(tolower(nm), pat)]
  if (length(h)) h[1] else NA_character_
}
parse_val <- function(x){
  if (is.numeric(x)) return(x)
  s <- tolower(str_replace_all(as.character(x), "[\u20ac,\\s]", ""))
  m <- ifelse(str_detect(s, "m"), 1e6, ifelse(str_detect(s, "k"), 1e3, 1))
  suppressWarnings(as.numeric(str_remove_all(s, "[mk]")) * m)
}
cN <- pick(valores_raw, c("player_name","player","name"), "name")
cS <- pick(valores_raw, c("squad","current_club","team","club"), "squad|club|team")
cV <- pick(valores_raw, c("player_market_value_euros","market_value_euros",
                          "value_euros","player_market_value"), "value.*eur|market.*value")
cU <- pick(valores_raw, c("player_url","url"), "url")
cP <- pick(valores_raw, c("player_position","position","pos"), "position")
cA <- pick(valores_raw, c("player_age","age"), "age")
cNt<- pick(valores_raw, c("player_nationality","nationality"), "nation")
if (is.na(cN) || is.na(cV))
  stop("No localizo columnas de nombre/valor. Mira el print de names() de arriba y pasamelo.")

valores <- valores_raw %>% transmute(
  liga        = if ("liga" %in% names(valores_raw)) liga else NA_character_,
  player_name = .data[[cN]],
  squad       = if (!is.na(cS))  .data[[cS]]  else NA_character_,
  position    = if (!is.na(cP))  .data[[cP]]  else NA_character_,
  age         = if (!is.na(cA))  .data[[cA]]  else NA,
  nationality = if (!is.na(cNt)) .data[[cNt]] else NA_character_,
  value_eur   = round(parse_val(.data[[cV]])),
  player_url  = if (!is.na(cU))  .data[[cU]]  else NA_character_,
  tm_id       = if (!is.na(cU))  str_match(.data[[cU]], "spieler/(\\d+)")[,2] else NA_character_
) %>%
  mutate(valor_millones = round(value_eur / 1e6, 2)) %>%
  relocate(valor_millones, .after = value_eur) %>%
  filter(!is.na(player_name), player_name != "")

write_excel_csv(valores, "valores_transfermarkt.csv")

## 3) Resumen para que lo analices -----------------------------
cat("\n================ RESUMEN ================\n")
cat("Jugadores guardados:", nrow(valores), "\n")
print(valores %>% count(liga, name = "jugadores"))
vv <- valores$value_eur
cat(sprintf("\nValor de mercado (M EUR): min %.2f | mediana %.2f | max %.1f | con valor %d\n",
            min(vv, na.rm=TRUE)/1e6, median(vv, na.rm=TRUE)/1e6,
            max(vv, na.rm=TRUE)/1e6, sum(!is.na(vv))))
cat("\nTop 10 por valor:\n")
print(valores %>% arrange(desc(value_eur)) %>% select(player_name, squad, value_eur) %>% head(10))
cat("\nListo -> valores_transfermarkt.csv\n")
