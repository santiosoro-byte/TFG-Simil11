# ============================================================
#  TFG · Ingeniería del Dato — Limpieza y transformación (R)
#  A partir de master_con_valor.csv: deriva las per-90, documenta
#  la poda de variables, imputa nulos y saca las 4 figuras de
#  antes/después. Genera master_modelo.csv (dataset de modelado).
#  USO: ten master_con_valor.csv en esta carpeta, abre RStudio, Source.
# ============================================================
req <- c("tidyverse", "patchwork")
for (p in req) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressPackageStartupMessages({ library(tidyverse); library(patchwork) })

find1 <- function(pat){ f <- list.files(getwd(), pat, recursive = TRUE, full.names = TRUE)
if (!length(f)) stop(paste("No encuentro", pat)); f[1] }
df <- read_csv(find1("^master_con_valor\\.csv$"), show_col_types = FALSE) %>%
  mutate(Pos_primary = factor(Pos_primary, levels = c("DF","MF","FW")))

# ---- derivar per-90 (incl. redundantes, solo para la figura de poda) ----
df <- df %>% mutate(
  KP_p90     = pass_KP     / `90s`, PPA_p90    = pass_PPA    / `90s`, CrsPA_p90  = pass_CrsPA / `90s`,
  xA_p90     = pass_xA     / `90s`, Cmp_p90    = pass_Cmp_tot / `90s`,
  PrgC_p90   = PrgC        / `90s`, PrgP_p90   = PrgP         / `90s`, PrgR_p90   = PrgR        / `90s`,
  Tkl_p90    = def_Tkl     / `90s`, Int_p90    = def_Int      / `90s`,
  Blocks_p90 = def_Blocks  / `90s`, Clr_p90    = def_Clr      / `90s`,
  CmpPct     = pass_CmpPct_tot,     ChalTklPct = def_ChalTklPct,
  TklW_p90   = def_TklW    / `90s`, TklInt_p90 = `def_Tkl+Int` / `90s`)

estilo <- c("Gls_p90","npxG_p90","Ast_p90","xAG_p90","xA_p90","KP_p90","PPA_p90","CrsPA_p90",
            "PrgC_p90","PrgP_p90","PrgR_p90","Cmp_p90","CmpPct","Tkl_p90","Int_p90","Blocks_p90","Clr_p90","ChalTklPct")
antes  <- c("Gls_p90","npxG_p90","xG_p90","G+A_p90","G-PK_p90","Ast_p90","xAG_p90","xG+xAG_p90","npxG+xAG_p90",
            "xA_p90","KP_p90","PPA_p90","CrsPA_p90","Cmp_p90","CmpPct","PrgC_p90","PrgP_p90","PrgR_p90",
            "Tkl_p90","TklW_p90","TklInt_p90","Int_p90","Blocks_p90","Clr_p90","ChalTklPct")
pal <- c(DF="#1f77b4", MF="#2ca02c", FW="#d62728"); theme_set(theme_bw(base_size = 11))

# ================= 1) PODA DE VARIABLES (antes / después) =================
heatdf <- function(vars){
  C <- cor(df %>% select(all_of(vars)), use = "pairwise.complete.obs")
  as.data.frame(as.table(C)) %>% setNames(c("v1","v2","r")) %>%
    mutate(v1 = factor(v1, levels = vars), v2 = factor(v2, levels = vars)) %>%
    filter(as.integer(v1) >= as.integer(v2))
}
hi <- heatdf(antes) %>% filter(v1 != v2, abs(r) > 0.85)          # parejas redundantes
p_antes <- ggplot(heatdf(antes), aes(v2, fct_rev(v1), fill = r)) +
  geom_tile(color = "white", linewidth = .3) +
  geom_tile(data = hi, color = "black", linewidth = .8, fill = NA) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0, limits = c(-1,1), guide = "none") +
  coord_fixed() + labs(title = paste0("ANTES — ", length(antes), " variables candidatas"), x = NULL, y = NULL) +
  theme(axis.text = element_text(size = 6), axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5), panel.grid = element_blank())
p_desp <- ggplot(heatdf(estilo), aes(v2, fct_rev(v1), fill = r)) +
  geom_tile(color = "white", linewidth = .3) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0, limits = c(-1,1), name = "r") +
  coord_fixed() + labs(title = paste0("DESPUÉS — ", length(estilo), " variables finales"), x = NULL, y = NULL) +
  theme(axis.text = element_text(size = 7), axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5), panel.grid = element_blank())
(p_antes | p_desp) +
  plot_annotation(title = paste0("Poda de variables por correlación |r| > 0,85 — se eliminan ", length(antes) - length(estilo), " redundantes"))
ggsave("fig_poda_correlacion_antes_despues.png", width = 20, height = 9.5, dpi = 130)

# ================= 2) IMPUTACIÓN DE NULOS (antes / después) =================
# imputación por la MEDIANA de la posición (robusta y sin dependencias extra;
# adecuada cuando a un jugador le falta un bloque entero de pase/defensa)
na_antes <- sapply(df[estilo], function(x) sum(is.na(x)))
mi <- df %>% group_by(Pos_primary) %>%
  mutate(across(all_of(estilo), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))) %>% ungroup()
na_desp <- sapply(mi[estilo], function(x) sum(is.na(x)))
tibble(var = factor(estilo, levels = estilo), Antes = na_antes, `Después` = na_desp) %>%
  pivot_longer(c(Antes, `Después`), names_to = "momento", values_to = "nulos") %>%
  mutate(momento = factor(momento, levels = c("Antes","Después"))) %>%
  ggplot(aes(var, nulos, fill = momento)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(Antes = "#d62728", `Después` = "#2ca02c"),
                    labels = c(paste0("Antes (", sum(na_antes), " nulos)"), paste0("Después (", sum(na_desp), " nulos)"))) +
  labs(title = "Imputación de nulos por la mediana de la posición",
       subtitle = "jugadores sin bloque de pase/defensa", x = NULL, y = "nº de nulos", fill = NULL) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
ggsave("fig_imputacion_nulos.png", width = 12, height = 6, dpi = 130)
write_csv(mi, "master_modelo.csv")

# ================= 3) CAMBIO DE VALORES: MINUTOS (antes / después) =================
rec <- df %>% filter(Min >= 1000) %>% transmute(Player, despues = Min, antes = Min/1000) %>% arrange(despues)
rec$y <- seq_len(nrow(rec))
ggplot(rec) +
  geom_segment(aes(antes, y, xend = despues, yend = y), color = "grey70") +
  geom_point(aes(antes, y, color = "Valor guardado (decimal → < 450)"), size = 1.8) +
  geom_point(aes(despues, y, color = "Tras corrección (× 1000)"), size = 1.8) +
  geom_vline(xintercept = 450, linetype = "dashed") +
  scale_x_log10() +
  scale_color_manual(values = c("Valor guardado (decimal → < 450)" = "#d62728", "Tras corrección (× 1000)" = "#2ca02c")) +
  scale_y_continuous(breaks = rec$y, labels = rec$Player) +
  labs(title = "Cambio de valores: minutos guardados como decimal de miles (1.043 = 1043)",
       subtitle = paste0(nrow(rec), " jugadores con 1000+ min corregidos ×1000"),
       x = "minutos (escala log)", y = NULL, color = NULL) +
  theme(axis.text.y = element_text(size = 6), legend.position = "bottom")
ggsave("fig_minutos_antes_despues.png", width = 11, height = 8, dpi = 130)

# ================= 4) COBERTURA DEL CRUCE (cascada) =================
n <- nrow(df)
casc <- tibble(
  capa = factor(c("Nombre único","+ Diccionario","+ Fuzzy / comodín"),
                levels = c("Nombre único","+ Diccionario","+ Fuzzy / comodín")),
  pct  = c(100 * sum(df$match_metodo == "nombre", na.rm = TRUE) / n,
           100 * sum(df$match_metodo %in% c("nombre","nombre+club","diccionario"), na.rm = TRUE) / n,
           100 * sum(!is.na(df$match_metodo)) / n))
ggplot(casc, aes(pct, fct_rev(capa), fill = capa)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -.12, fontface = "bold") +
  scale_fill_manual(values = c("#1f77b4","#ff7f0e","#2ca02c")) +
  coord_cartesian(xlim = c(0, 105)) +
  labs(title = "Cobertura del valor de mercado por capa del cruce",
       subtitle = "de 89 % (solo nombre) a 96,4 % con la cascada completa",
       x = "cobertura acumulada (%)", y = NULL)
ggsave("fig_cobertura_cruce.png", width = 10, height = 4, dpi = 130)

cat("\nLimpieza en R completa: 4 figuras antes/después + master_modelo.csv (", nrow(mi), "x", ncol(mi), ")\n")
