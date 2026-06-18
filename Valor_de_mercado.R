# ============================================================
#  Script 3 | Cruce master <-> valor de mercado (Transfermarkt)
#  Une master_jugadores_campo.csv + valores_transfermarkt.csv en
#  CASCADA: nombre unico -> nombre+club -> diccionario tm_id ->
#  fuzzy/comodin. Guarda master_con_valor.csv (~96% de cobertura).
#  USO: ten los 2 CSV en esta carpeta, abre en RStudio y "Source".
# ============================================================
need <- c("worldfootballR","dplyr","stringr","stringi","readr")
for (p in need) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(stringi); library(readr) })

norm_nm <- function(x) stri_trans_general(
  str_squish(str_replace_all(tolower(as.character(x)), "[-.'\u2019\u00b4]", " ")), "Latin-ASCII")

find1 <- function(pat){
  f <- list.files(getwd(), pattern = pat, recursive = TRUE, full.names = TRUE)
  if (!length(f)) stop(paste("No encuentro", pat, "en la carpeta")); f[1]
}
mas <- read_csv(find1("^master_jugadores_campo\\.csv$"), show_col_types = FALSE)
val <- read_csv(find1("^valores_transfermarkt\\.csv$"),  show_col_types = FALSE)
cat("Master:", nrow(mas), "jugadores | Valores TM:", nrow(val), "\n")

val <- val %>% filter(!is.na(value_eur)) %>%
  mutate(kn = norm_nm(player_name), kc = norm_nm(squad),
         tmid = str_extract(as.character(tm_id), "[0-9]+"))
mas <- mas %>% mutate(kn = norm_nm(Player), kc = norm_nm(Squad))

mas$tm_market_value_eur <- NA_real_; mas$tm_player_name <- NA_character_
mas$tm_squad <- NA_character_;        mas$match_metodo  <- NA_character_

set_hit <- function(mask, v, nm, sq, label){           # rellena solo los que aun no tienen valor
  i <- which(mask & is.na(mas$tm_market_value_eur))
  mas$tm_market_value_eur[i] <<- v[i];  mas$tm_player_name[i] <<- nm[i]
  mas$tm_squad[i]            <<- sq[i];  mas$match_metodo[i]   <<- label
}

# 1) nombre unico ---------------------------------------------------------
uni <- val %>% add_count(kn) %>% filter(n == 1)
vU<-setNames(uni$value_eur,uni$kn); nU<-setNames(uni$player_name,uni$kn); sU<-setNames(uni$squad,uni$kn)
set_hit(mas$kn %in% names(vU), unname(vU[mas$kn]), unname(nU[mas$kn]), unname(sU[mas$kn]), "nombre")

# 2) nombre + club (para nombres repetidos) -------------------------------
dup <- val %>% add_count(kn) %>% filter(n > 1) %>% distinct(kn, kc, .keep_all = TRUE) %>% mutate(key = paste(kn, kc))
vD<-setNames(dup$value_eur,dup$key); nD<-setNames(dup$player_name,dup$key); sD<-setNames(dup$squad,dup$key)
mk <- paste(mas$kn, mas$kc)
set_hit(mk %in% names(vD), unname(vD[mk]), unname(nD[mk]), unname(sD[mk]), "nombre+club")

# 3) diccionario FBref <-> Transfermarkt por tm_id ------------------------
dic <- tryCatch(worldfootballR::player_dictionary_mapping(), error = function(e) NULL)
if (!is.null(dic)) {
  nmc  <- names(dic)[str_detect(tolower(names(dic)), "player")][1]                 # nombre FBref
  urlc <- names(dic)[str_detect(tolower(names(dic)), "tmarkt|transfermarkt")][1]   # url Transfermarkt
  # el diccionario viene en cp1252; saneamos los bytes a UTF-8 (si no, norm_nm casca)
  dic[[nmc]]  <- iconv(as.character(dic[[nmc]]),  from = "latin1", to = "UTF-8")
  dic[[urlc]] <- iconv(as.character(dic[[urlc]]), from = "latin1", to = "UTF-8")
  dic <- dic %>% mutate(kn = norm_nm(.data[[nmc]]), tmid = str_extract(.data[[urlc]], "[0-9]+")) %>%
    filter(!is.na(tmid)) %>% distinct(kn, .keep_all = TRUE)
  vById <- val %>% filter(!is.na(tmid)) %>% distinct(tmid, .keep_all = TRUE) %>%
    select(tmid, value_eur, player_name, squad)
  dv <- dic %>% select(kn, tmid) %>% left_join(vById, by = "tmid") %>% filter(!is.na(value_eur))
  vX<-setNames(dv$value_eur,dv$kn); nX<-setNames(dv$player_name,dv$kn); sX<-setNames(dv$squad,dv$kn)
  set_hit(mas$kn %in% names(vX), unname(vX[mas$kn]), unname(nX[mas$kn]), unname(sX[mas$kn]), "diccionario")
} else cat("(aviso: no se pudo descargar el diccionario; se omite esa capa)\n")

# 4) fuzzy (tokens) + comodin sobre '?' -----------------------------------
vkn   <- unique(val$kn)
vlook <- val %>% distinct(kn, .keep_all = TRUE)
find_one <- function(kn){
  toks <- strsplit(kn, " ")[[1]]
  if (length(toks)) {
    sur <- toks[length(toks)]
    cc <- vkn[vapply(vkn, function(k){ kt <- strsplit(k, " ")[[1]]
    sur %in% kt && (all(toks %in% kt) || all(kt %in% toks)) }, logical(1))]
    cc <- unique(cc); if (length(cc) == 1) return(cc)
  }
  if (grepl("\\?", kn)) {                                  # nombre corrupto: '?' = comodin
    pat <- paste0("^", str_replace_all(kn, "[^a-z0-9 ]", "."), "$")
    cc <- unique(vkn[grepl(pat, vkn)]); if (length(cc) == 1) return(cc)
  }
  NA_character_
}
# guardia de club: solo acepta el match difuso si los clubes comparten un
# token con el mismo prefijo de 4 letras (evita colisiones de apellido, p.ej.
# Michael Murillo del Marsella != "Murillo" del Nottingham Forest)
club_ok <- function(a, b, k = 4){
  pa <- substr(Filter(function(t) nchar(t) >= k, strsplit(norm_nm(a), " ")[[1]]), 1, k)
  pb <- substr(Filter(function(t) nchar(t) >= k, strsplit(norm_nm(b), " ")[[1]]), 1, k)
  length(intersect(pa, pb)) > 0
}
for (i in which(is.na(mas$tm_market_value_eur))) {
  k <- find_one(mas$kn[i])
  if (!is.na(k)) {
    r <- vlook[vlook$kn == k, ][1, ]
    if (club_ok(mas$Squad[i], r$squad)) {
      mas$tm_market_value_eur[i] <- r$value_eur; mas$tm_player_name[i] <- r$player_name
      mas$tm_squad[i] <- r$squad;                mas$match_metodo[i]   <- "fuzzy/comodin"
    }
  }
}

# 5) guardar + validacion -------------------------------------------------
mas$tm_valor_millones <- round(mas$tm_market_value_eur / 1e6, 2)
out <- mas %>% select(-kn, -kc)
write_excel_csv(out, file.path(getwd(), "master_con_valor.csv"))
cov <- mean(!is.na(mas$tm_market_value_eur)) * 100
cat(sprintf("\nCobertura de valor de mercado: %.1f%% (%d/%d)\n",
            cov, sum(!is.na(mas$tm_market_value_eur)), nrow(mas)))
print(table(mas$match_metodo, useNA = "ifany"))
cat("Sin valor:", sum(is.na(mas$tm_market_value_eur)), "\n")
cat("Guardado: master_con_valor.csv\n")
