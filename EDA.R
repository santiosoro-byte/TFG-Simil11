# ============================================================
#  TFG · Ingeniería del Dato — Análisis exploratorio (EDA) en R
#  Reproduce en ggplot2 las figuras del EDA a partir del dataset
#  de modelado (master_modelo.csv): heatmap de correlación,
#  distribuciones y boxplots por posición, variables de negocio,
#  completitud, y la tabla de descriptivos.
#  USO: ten master_modelo.csv en esta carpeta, abre en RStudio y "Source".
# ============================================================
req <- c("tidyverse", "patchwork")
for (p in req) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressPackageStartupMessages({ library(tidyverse); library(patchwork) })

find1 <- function(pat){ f <- list.files(getwd(), pat, recursive = TRUE, full.names = TRUE)
if (!length(f)) stop(paste("No encuentro", pat)); f[1] }
df <- read_csv(find1("^master_con_valor\\.csv$"), show_col_types = FALSE)

# Derivar las variables de estilo por-90 (las de pase/defensa vienen en totales;
# Gls_p90, npxG_p90, Ast_p90 y xAG_p90 ya vienen calculadas de FBref)
df <- df %>% mutate(
  KP_p90     = pass_KP      / `90s`, PPA_p90    = pass_PPA    / `90s`, CrsPA_p90  = pass_CrsPA / `90s`,
  xA_p90     = pass_xA      / `90s`, Cmp_p90    = pass_Cmp_tot / `90s`,
  PrgC_p90   = PrgC         / `90s`, PrgP_p90   = PrgP         / `90s`, PrgR_p90   = PrgR        / `90s`,
  Tkl_p90    = def_Tkl      / `90s`, Int_p90    = def_Int      / `90s`,
  Blocks_p90 = def_Blocks   / `90s`, Clr_p90    = def_Clr      / `90s`,
  CmpPct     = pass_CmpPct_tot,      ChalTklPct = def_ChalTklPct)

estilo <- c("Gls_p90","npxG_p90","Ast_p90","xAG_p90","xA_p90","KP_p90","PPA_p90","CrsPA_p90",
            "PrgC_p90","PrgP_p90","PrgR_p90","Cmp_p90","CmpPct","Tkl_p90","Int_p90","Blocks_p90","Clr_p90","ChalTklPct")
df  <- df %>% mutate(Pos_primary = factor(Pos_primary, levels = c("DF","MF","FW")))
pal <- c(DF = "#1f77b4", MF = "#2ca02c", FW = "#d62728")
theme_set(theme_bw(base_size = 11))

# ---- 1) DESCRIPTIVOS POR POSICIÓN -> CSV --------------------------------
desc <- df %>% select(Pos_primary, all_of(estilo)) %>%
  pivot_longer(-Pos_primary, names_to = "variable", values_to = "val") %>%
  group_by(variable, Pos_primary) %>%
  summarise(media = round(mean(val, na.rm = TRUE), 2),
            sd    = round(sd(val,   na.rm = TRUE), 2), .groups = "drop") %>%
  pivot_wider(names_from = Pos_primary, values_from = c(media, sd)) %>%
  arrange(factor(variable, levels = estilo))
write_csv(desc, "eda_descriptivos_por_posicion.csv")

# ---- 2) HEATMAP DE CORRELACIÓN ------------------------------------------
C <- cor(df %>% select(all_of(estilo)), use = "pairwise.complete.obs")
heat <- as.data.frame(as.table(C)) %>% setNames(c("v1","v2","r")) %>%
  mutate(v1 = factor(v1, levels = estilo), v2 = factor(v2, levels = estilo)) %>%
  filter(as.integer(v1) >= as.integer(v2))                       # triángulo inferior
ggplot(heat, aes(v2, fct_rev(v1), fill = r)) +
  geom_tile(color = "white", linewidth = .4) +
  geom_text(aes(label = sprintf("%.2f", r)), size = 2.4) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                       midpoint = 0, limits = c(-1, 1), name = "r de Pearson") +
  coord_fixed() +
  labs(title = "Matriz de correlación de las 18 variables de estilo",
       subtitle = "Jugadores de campo · 5 grandes ligas", x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5), panel.grid = element_blank())
ggsave("eda_heatmap_correlacion.png", width = 11, height = 10, dpi = 130)

# ---- 3) DISTRIBUCIONES POR POSICIÓN -------------------------------------
etq <- c(npxG_p90 = "npxG / 90", xAG_p90 = "xAG / 90", KP_p90 = "Pases clave / 90",
         PrgC_p90 = "Conducciones progr. / 90", PrgP_p90 = "Pases progr. / 90",
         Tkl_p90 = "Entradas / 90", Clr_p90 = "Despejes / 90", CmpPct = "% acierto de pase")
df %>% select(Pos_primary, all_of(names(etq))) %>%
  pivot_longer(-Pos_primary, names_to = "var", values_to = "val") %>%
  mutate(var = factor(etq[var], levels = etq)) %>%
  ggplot(aes(val, fill = Pos_primary, color = Pos_primary)) +
  geom_density(alpha = .25, linewidth = .7) +
  facet_wrap(~ var, scales = "free", ncol = 4) +
  scale_fill_manual(values = pal) + scale_color_manual(values = pal) +
  labs(title = "Distribución de variables de estilo por posición",
       x = NULL, y = "densidad", fill = "Posición", color = "Posición")
ggsave("eda_distribuciones_por_posicion.png", width = 16, height = 8, dpi = 130)

# ---- 4) BOXPLOTS POR POSICIÓN -------------------------------------------
etqb <- c(npxG_p90 = "npxG / 90", xAG_p90 = "xAG / 90", KP_p90 = "Pases clave / 90",
          PrgC_p90 = "Conducciones progr./90", PrgR_p90 = "Recepciones progr./90",
          Tkl_p90 = "Entradas / 90", Clr_p90 = "Despejes / 90", CmpPct = "% acierto pase")
df %>% select(Pos_primary, all_of(names(etqb))) %>%
  pivot_longer(-Pos_primary, names_to = "var", values_to = "val") %>%
  mutate(var = factor(etqb[var], levels = etqb)) %>%
  ggplot(aes(Pos_primary, val, fill = Pos_primary)) +
  geom_boxplot(outlier.size = .5, outlier.alpha = .3) +
  facet_wrap(~ var, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = pal) +
  labs(title = "Distribución por posición (boxplots) — mediana, IQR y atípicos",
       x = NULL, y = NULL, fill = "Posición")
ggsave("eda_boxplots_por_posicion.png", width = 16, height = 8, dpi = 130)

# ---- 5) VARIABLES DE NEGOCIO --------------------------------------------
dn <- df %>% mutate(valor_M = tm_market_value_eur / 1e6, sal_M = sal_wage_annual_eur / 1e6)
g1 <- ggplot(dn, aes(valor_M)) + geom_histogram(bins = 30, fill = "#4477aa") + scale_x_log10() +
  labs(title = "Valor de mercado (M€, log)", x = "M€", y = "nº jugadores")
g2 <- ggplot(dn, aes(sal_M)) + geom_histogram(bins = 30, fill = "#aa7744") + scale_x_log10() +
  labs(title = "Salario anual (M€, log)", x = "M€/año", y = "nº jugadores")
g3 <- ggplot(dn, aes(Age)) + geom_histogram(binwidth = 1, fill = "#66aa77", color = "white") +
  labs(title = "Edad", x = "años", y = "nº jugadores")
g4 <- ggplot(dn, aes(Pos_primary, valor_M, fill = Pos_primary)) + geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = pal, guide = "none") + labs(title = "Valor por posición", x = NULL, y = "M€")
g5 <- ggplot(dn, aes(Pos_primary, sal_M, fill = Pos_primary)) + geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = pal, guide = "none") + labs(title = "Salario por posición", x = NULL, y = "M€/año")
rho <- cor(log10(dn$valor_M), log10(dn$sal_M), use = "complete.obs")
g6 <- ggplot(dn, aes(sal_M, valor_M, color = Pos_primary)) + geom_point(size = 1, alpha = .5) +
  scale_x_log10() + scale_y_log10() + scale_color_manual(values = pal) +
  labs(title = sprintf("Valor vs salario (log-log, r=%.2f)", rho),
       x = "salario (M€)", y = "valor (M€)", color = "Posición")
(g1 | g2 | g3) / (g4 | g5 | g6) +
  plot_annotation(title = "Variables de negocio: valor de mercado, salario y edad")
ggsave("eda_variables_negocio.png", width = 16, height = 9, dpi = 130)

# ---- 6) COMPLETITUD POR FUENTE ------------------------------------------
comp <- tibble(
  fuente = c("Standard\n(FBref)","Passing\n(FBref)","Defensive\n(FBref)","Salario\n(Capology)","Valor\n(Transfermarkt)"),
  pct = c(100,
          100 * mean(!is.na(df$pass_KP)),
          100 * mean(!is.na(df$def_Tkl)),
          100 * mean(!is.na(df$sal_wage_weekly_eur)),
          100 * mean(!is.na(df$tm_market_value_eur)))) %>%
  mutate(fuente = factor(fuente, levels = fuente))
ggplot(comp, aes(fuente, pct, fill = fuente)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -.4, fontface = "bold") +
  scale_fill_manual(values = c("#34568B","#34568B","#34568B","#aa7744","#2ca02c")) +
  coord_cartesian(ylim = c(0, 108)) +
  labs(title = "Completitud del dataset por fuente / bloque de variables", x = NULL, y = "% con dato")
ggsave("eda_completitud_fuentes.png", width = 10, height = 5, dpi = 130)

cat("\nEDA en R completo: 5 figuras PNG + eda_descriptivos_por_posicion.csv\n")