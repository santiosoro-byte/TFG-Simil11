# =====================================================================
#  TFG - Sistema de recomendacion de jugadores (5 grandes ligas europeas)
#  SCRIPT 1 | Dataset maestro  ·  VERSION AUTONOMA (no hay que editar nada)
# ---------------------------------------------------------------------
#  - Localiza sola la carpeta de datos; si no, abre un dialogo para elegirla.
#  - Integra por jugador las 4 tablas de FBref 2025-26 (Standard, Passing,
#    Defensive, Salaries) de las 5 grandes ligas. Resultado validado:
#    ~1000 jugadores de campo (>=450 min = 5 partidos completos), sin porteros.
#  - SALARIOS: lectura robusta (cp1252 forzado, espacio duro limpiado,
#    importe en EUROS aunque la libra vaya primero, columnas por nombre).
#  USO: abre este fichero en RStudio y pulsa "Source" (o pega todo y ejecuta).
#       Si aparece un dialogo, selecciona la carpeta "Datos-TFG".
# =====================================================================

# install.packages(c("tidyverse", "readxl"))
suppressPackageStartupMessages({ library(tidyverse); library(readxl) })

MIN_MINUTOS <- 450
ligas       <- c("PL", "LaLiga", "SerieA", "Bundes", "L1")

# --- 0. LOCALIZAR LA CARPETA DE DATOS AUTOMATICAMENTE -----------------
locate_base <- function(start){
  cands <- unique(c(start, list.dirs(start, recursive = TRUE)))
  has_all <- function(d){
    subs <- tolower(basename(list.dirs(d, recursive = FALSE)))
    any(grepl("general|standard", subs)) && any(grepl("passing|pass", subs)) &&
      any(grepl("defen", subs)) && any(grepl("salar|wage", subs))
  }
  for (d in cands) if (length(d) && has_all(d)) return(normalizePath(d))
  NULL
}
pick_dir <- function(){
  msg <- "Selecciona la carpeta Datos-TFG"
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
    return(rstudioapi::selectDirectory(caption = msg))
  if (.Platform$OS.type == "windows")
    return(utils::choose.dir(caption = msg))
  if (requireNamespace("tcltk", quietly = TRUE))
    return(tryCatch(tcltk::tk_choose.dir(caption = msg), error = function(e) NA_character_))
  readline("Pega la ruta a la carpeta Datos-TFG y pulsa Enter: ")
}
base_dir <- locate_base(getwd())
if (is.null(base_dir)){
  chosen <- pick_dir()
  ok <- !is.null(chosen) && length(chosen) == 1 && !is.na(chosen) && nzchar(chosen)
  if (!ok) stop("No se ha seleccionado carpeta. Vuelve a ejecutar y elige la carpeta Datos-TFG.")
  base_dir <- locate_base(chosen); if (is.null(base_dir)) base_dir <- chosen
}
out_file <- file.path(base_dir, "master_jugadores_campo.csv")
cat("Carpeta de datos:", base_dir, "\n")

# --- 1. LOCALIZAR SUBCARPETAS Y FICHEROS (robusto a nombres raros) ----
find_category <- function(base, kw){
  ds  <- list.dirs(base, recursive = TRUE)
  hit <- ds[str_detect(tolower(basename(ds)), kw)]
  hit <- hit[!str_detect(tolower(basename(hit)), "team")]
  if (!length(hit)) stop(paste0("No encuentro la carpeta de categoria: ", kw, " en ", base))
  hit[which.min(nchar(hit))]
}
player_subdir <- function(cat){
  subs <- list.dirs(cat, recursive = FALSE)
  subs <- subs[!str_detect(tolower(basename(subs)), "team")]
  if (!length(subs)) cat else subs[1]
}
dir_std <- player_subdir(find_category(base_dir, "general|standard"))
dir_pas <- player_subdir(find_category(base_dir, "passing|pass"))
dir_def <- player_subdir(find_category(base_dir, "defen"))
dir_sal <- player_subdir(find_category(base_dir, "salar|wage"))

.key <- function(fn) str_remove_all(tolower(str_remove_all(fn, regex("players", ignore_case = TRUE))), "[^a-z0-9]")
LIGA_PAT <- c(PL = "pl", LaLiga = "laliga", L1 = "l1", SerieA = "seriea", Bundes = "bundes")
find_file <- function(folder, liga){
  fs  <- list.files(folder, full.names = TRUE, pattern = "\\.(csv|xlsx)$", ignore.case = TRUE)
  hit <- fs[str_detect(.key(basename(fs)), fixed(LIGA_PAT[[liga]]))]
  if (!length(hit)) stop(paste0("No encuentro fichero de ", liga, " en ", folder))
  hit[1]
}

# --- 2. LECTURA ROBUSTA (cp1252 + cabecera en fila variable) ----------
read_raw <- function(path){
  if (!str_detect(tolower(path), "\\.csv$"))
    return(as.data.frame(readxl::read_excel(path, col_names = FALSE, col_types = "text")))
  cand <- unique(c(tryCatch(readr::guess_encoding(path)$encoding, error = function(e) character(0)),
                   "windows-1252", "latin1", "UTF-8"))
  cand <- setdiff(cand, "ASCII"); if (!length(cand)) cand <- "windows-1252"
  res <- NULL
  for (enc in cand){
    df <- tryCatch(suppressWarnings(readr::read_delim(
      path, delim = ";", col_names = FALSE, col_types = cols(.default = "c"),
      locale = locale(encoding = enc), progress = FALSE, show_col_types = FALSE)),
      error = function(e) NULL)
    if (is.null(df)) next
    res <- df
    mal <- any(vapply(df, function(col) any(str_detect(col, "\uFFFD"), na.rm = TRUE), logical(1)))
    if (!mal) return(as.data.frame(df))
  }
  as.data.frame(res)
}
header_row <- function(raw)
  which(apply(raw, 1, function(r) any(str_trim(as.character(r)) == "Player", na.rm = TRUE)))[1]
read_named <- function(path, cols){
  raw <- read_raw(path); h <- header_row(raw)
  df  <- raw[(h + 1):nrow(raw), , drop = FALSE]
  k   <- min(ncol(df), length(cols)); df <- df[, 1:k, drop = FALSE]; colnames(df) <- cols[1:k]
  df %>% filter(!is.na(Player), str_trim(Player) != "", str_trim(Player) != "Player") %>%
    mutate(Player = str_trim(Player), Squad = str_trim(Squad))
}
load_all <- function(folder, cols) map_dfr(ligas, ~ read_named(find_file(folder, .x), cols))

num <- function(x){
  x  <- str_remove_all(str_trim(as.character(x)), "[%\\s\u00a0]")
  th <- str_detect(x, "^-?\\d{1,3}([.,]\\d{3})+$"); th[is.na(th)] <- FALSE   # miles: grupos de 3 (1.043 / 1,043)
  x[th] <- str_remove_all(x[th], "[.,]")
  dc <- str_detect(x, "^-?\\d+,\\d{1,2}$");          dc[is.na(dc)] <- FALSE   # decimal con coma europea (1,05)
  x[dc] <- str_replace(x[dc], ",", ".")
  x  <- str_replace_all(x, ",", "")                                          # coma que quede = miles
  suppressWarnings(as.numeric(x))
}
text_cols <- c("Player","Squad","League","Nation","Pos","Pos_primary")
numify    <- function(df) df %>% mutate(across(-any_of(text_cols), num))

# --- 3. NOMBRES POR TABLA (orden identico en las 5 ligas) -------------
COLS_STD <- c("Rk","Player","Nation","Pos","Squad","Age","Born","MP","Starts","Min","90s",
              "Gls","Ast","G+A","G-PK","PK","PKatt","CrdY","CrdR","xG","npxG","xAG","npxG+xAG","PrgC","PrgP","PrgR",
              "Gls_p90","Ast_p90","G+A_p90","G-PK_p90","G+A-PK_p90","xG_p90","xAG_p90","xG+xAG_p90","npxG_p90","npxG+xAG_p90","Matches")
COLS_PAS <- c("Rk","Player","Nation","Pos","Squad","Age","Born","90s",
              "Cmp_tot","Att_tot","CmpPct_tot","TotDist","PrgDist","Cmp_short","Att_short","CmpPct_short",
              "Cmp_med","Att_med","CmpPct_med","Cmp_long","Att_long","CmpPct_long",
              "Ast","xAG","xA","A-xAG","KP","Pass_final3rd","PPA","CrsPA","PrgP","Matches")
COLS_DEF <- c("Rk","Player","Nation","Pos","Squad","Age","Born","90s",
              "Tkl","TklW","Tkl_Def3rd","Tkl_Mid3rd","Tkl_Att3rd","ChalTkl","ChalAtt","ChalTklPct","ChalLost",
              "Blocks","Blocks_Sh","Blocks_Pass","Int","Tkl+Int","Clr","Err","Matches")

# --- 4. CARGA ---------------------------------------------------------
standard <- map_dfr(ligas, ~ read_named(find_file(dir_std, .x), COLS_STD) %>% mutate(League = .x)) %>%
  mutate(Pos_primary = str_extract(Pos, "^[^,]+"),
         Age         = num(str_extract(Age, "^[0-9]+"))) %>%
  numify() %>%
  # FIX: el Excel guarda algunos minutos de 1000+ como decimal de miles
  # (la celda numerica vale 1.043 = 1043 min). Si Min sale como decimal
  # pequeno, x1000 -> recupera a Mbappe, Luis Milla, Cucho, etc., que de
  # lo contrario caen por debajo del umbral y se pierden del dataset.
  mutate(Min = ifelse(!is.na(Min) & Min < 100 & abs(Min - round(Min)) > 1e-9,
                      round(Min * 1000), Min))
passing <- load_all(dir_pas, COLS_PAS) %>% numify() %>%
  rename_with(~ paste0("pass_", .x), .cols = -all_of(c("Player","Squad")))
defense <- load_all(dir_def, COLS_DEF) %>% numify() %>%
  rename_with(~ paste0("def_",  .x), .cols = -all_of(c("Player","Squad")))

# --- SALARIOS (lectura robusta) --------------------------------------
read_raw_sal <- function(path){      # CSV de salarios: cp1252 FORZADO (el simbolo € se lee mal con autodeteccion)
  if (!str_detect(tolower(path), "\\.csv$"))
    return(as.data.frame(readxl::read_excel(path, col_names = FALSE, col_types = "text")))
  as.data.frame(readr::read_delim(path, delim = ";", col_names = FALSE,
                                  col_types = cols(.default = "c"), locale = locale(encoding = "windows-1252"),
                                  progress = FALSE, show_col_types = FALSE))
}
read_salary <- function(path){
  raw <- read_raw_sal(path)
  raw[] <- lapply(raw, function(col) str_replace_all(as.character(col), "\u00a0", " "))   # espacio duro -> espacio
  hr <- which(apply(raw, 1, function(r) any(str_trim(r) == "Player", na.rm = TRUE)))[1]
  if (is.na(hr)) hr <- which(apply(raw, 1, function(r) any(str_detect(r, "Weekly"), na.rm = TRUE)))[1]
  H <- str_trim(as.character(unlist(raw[hr, ])))
  D <- raw[(hr + 1):nrow(raw), , drop = FALSE]
  if (mean(!is.na(suppressWarnings(as.numeric(D[[1]]))), na.rm = TRUE) > 0.7){   # columna indice (Rk) en los datos
    D <- D[, -1, drop = FALSE]
    if (grepl("^r?k$|rk", tolower(H[1])) || is.na(H[1]) || H[1] == "") H <- H[-1] # ...y su etiqueta en la cabecera
  }
  H <- H[seq_len(ncol(D))]; H[is.na(H) | H == ""] <- paste0("v", which(is.na(H) | H == ""))
  names(D) <- make.unique(H)
  g <- function(pat){ i <- which(str_detect(tolower(names(D)), pat))[1]; if (is.na(i)) rep(NA_character_, nrow(D)) else D[[i]] }
  tibble(Player = g("player"), Squad = g("squad|team"),
         Weekly_raw = g("weekly"), Annual_raw = g("annual")) %>%
    mutate(Player = str_remove_all(str_trim(Player), '["\u00a0]'),
           Squad  = str_remove_all(str_trim(Squad),  '["\u00a0]')) %>%
    filter(!is.na(Player), Player != "", Player != "Player")
}
parse_eur <- function(x){
  x   <- str_replace_all(as.character(x), "\u00a0", " ")
  amt <- str_match(x, "\u20ac\\s*([0-9][0-9.,]*)")[, 2]      # importe en EUROS (aunque la libra vaya primero)
  na  <- is.na(amt); amt[na] <- str_match(x[na], "([0-9][0-9.,]+)")[, 2]
  as.numeric(str_remove_all(amt, "[.,]"))
}
salaries <- map_dfr(ligas, ~ read_salary(find_file(dir_sal, .x))) %>%
  mutate(sal_wage_weekly_eur = parse_eur(Weekly_raw),
         sal_wage_annual_eur = parse_eur(Annual_raw)) %>%
  distinct(Player, Squad, .keep_all = TRUE) %>%
  select(Player, Squad, sal_wage_weekly_eur, sal_wage_annual_eur)

# --- 5. UNION, FILTRO Y LIMPIEZA --------------------------------------
master <- standard %>%
  left_join(passing,  by = c("Player","Squad")) %>%
  left_join(defense,  by = c("Player","Squad")) %>%
  left_join(salaries, by = c("Player","Squad")) %>%
  filter(Min >= MIN_MINUTOS, Pos_primary != "GK") %>%
  relocate(Player, Squad, League, Nation, Pos, Pos_primary, Age) %>%
  select(where(~ !all(is.na(.)))) %>%
  select(-any_of(c("Rk","pass_Rk","def_Rk","pass_Nation","def_Nation",
                   "pass_Pos","def_Pos","pass_Born","def_Born","pass_90s","def_90s"))) %>%
  mutate(across(where(is.character), ~ str_remove_all(.x, '["\u00a0]')),
         Pos         = str_replace_all(str_remove_all(Pos, " "), ",", "-"),
         Pos_primary = str_extract(Pos, "^[^-]+"))

# --- 5b. RESCATE DE SALARIOS por nombre normalizado -------------------
#  Recupera nombres que no casaron exactamente (acentos, apostrofes o
#  cesiones donde el club no coincide). Solo nombres SIN ambiguedad.
norm_nm <- function(x) stringi::stri_trans_general(
  str_squish(str_replace_all(tolower(as.character(x)), "[-.'\u2019\u00b4]", " ")), "Latin-ASCII")
sal_fb <- salaries %>% filter(!is.na(sal_wage_weekly_eur)) %>%
  mutate(k = norm_nm(Player)) %>% add_count(k) %>% filter(n == 1) %>%
  select(k, w_fb = sal_wage_weekly_eur, a_fb = sal_wage_annual_eur)
master <- master %>% mutate(k = norm_nm(Player)) %>%
  left_join(sal_fb, by = "k") %>%
  mutate(sal_wage_weekly_eur = coalesce(sal_wage_weekly_eur, w_fb),
         sal_wage_annual_eur = coalesce(sal_wage_annual_eur, a_fb)) %>%
  select(-k, -w_fb, -a_fb)

# --- 6. VALIDACION ----------------------------------------------------
cat("\n================ VALIDACION ================\n")
cat("Jugadores:", nrow(master), " | columnas:", ncol(master), "\n")
print(table(master$Pos_primary))
cobertura <- function(col) if (col %in% names(master)) round(100 * mean(!is.na(master[[col]])), 1) else NA_real_
cat(sprintf("Cobertura -> passing: %s%%  defensa: %s%%  salario: %s%%\n",
            cobertura("pass_KP"), cobertura("def_Tkl"), cobertura("sal_wage_weekly_eur")))
cat("Max sueldo semanal (EUR, ~600.962 Mbappe):", suppressWarnings(max(master$sal_wage_weekly_eur, na.rm = TRUE)), "\n")
cat("Max xG_p90 (debe rondar 1):", round(suppressWarnings(max(master$xG_p90, na.rm = TRUE)), 3), "\n")
cat("Jugadores con 1000+ min:", sum(master$Min >= 1000, na.rm = TRUE),
    "| Mbappe presente:", any(grepl("Mbapp", master$Player)), "\n")
chr <- master %>% select(where(is.character))
cat("Limpieza -> comas en texto:",
    sum(vapply(chr, function(col) sum(str_detect(col, ","),  na.rm = TRUE), integer(1))),
    "| comillas:",
    sum(vapply(chr, function(col) sum(str_detect(col, '\\"'), na.rm = TRUE), integer(1))), "\n")

# --- 7. GUARDAR -------------------------------------------------------
readr::write_excel_csv(master, out_file)
cat("\nGuardado:", out_file, "\n")
