# ============================================================
#  Modelado.R  ·  PASO 1 del Análisis del Dato (clustering y validación)
#  TFG: Sistema de recomendación de futbolistas
#  Entrada : master_con_valor.csv   (salida de Valor_de_mercado.R)
#  Salida  : clusters_asignados.csv  + figuras en /figuras
#  >>> EJECUTA ESTE PRIMERO; luego Recomendador.R. <<<
# ============================================================
# Paquetes necesarios (instalar una vez):
# install.packages(c("tidyverse","cluster","clusterCrit","mclust",
#                     "randomForest","factoextra","patchwork","reshape2"))

library(tidyverse)
library(cluster)      # silhouette
library(clusterCrit)  # Davies-Bouldin, Calinski-Harabasz
library(mclust)       # adjustedRandIndex (acuerdo entre modelos)
library(randomForest) # importancia de variables
library(factoextra)   # visualización de clustering y silueta
library(patchwork)
set.seed(42)

# setwd("C:/Users/cbran/OneDrive/Documentos/Datos-TFG")   # ajustar si hace falta
dir.create("figuras", showWarnings = FALSE)
df <- read.csv("master_con_valor.csv", check.names = FALSE, stringsAsFactors = FALSE)

# Grupo posicional (DF / MF / FW); los porteros ya están excluidos
df$grp <- sub("-.*", "", df$Pos_primary)

# ---------------- 1. Las 18 variables de estilo (por 90 minutos) ----------------
s90 <- df[["90s"]]; s90[s90 == 0] <- NA
F <- tibble(
  Gls_p90    = df[["Gls_p90"]],          npxG_p90 = df[["npxG_p90"]],
  Ast_p90    = df[["Ast_p90"]],          xAG_p90  = df[["xAG_p90"]],
  xA_p90     = df[["pass_xA"]]   / s90,   KP_p90   = df[["pass_KP"]]    / s90,
  PPA_p90    = df[["pass_PPA"]]  / s90,   CrsPA_p90= df[["pass_CrsPA"]] / s90,
  PrgC_p90   = df[["PrgC"]]      / s90,   PrgP_p90 = df[["PrgP"]]       / s90,
  PrgR_p90   = df[["PrgR"]]      / s90,   Cmp_p90  = df[["pass_Cmp_tot"]] / s90,
  CmpPct     = df[["pass_CmpPct_tot"]],
  Tkl_p90    = df[["def_Tkl"]]   / s90,   Int_p90  = df[["def_Int"]]    / s90,
  Blocks_p90 = df[["def_Blocks"]]/ s90,   Clr_p90  = df[["def_Clr"]]    / s90,
  ChalTklPct = df[["def_ChalTklPct"]]
)
V <- names(F)
F$grp <- df$grp

# Imputación por la mediana de la posición (replica Limpieza.R)
F <- F %>% group_by(grp) %>%
  mutate(across(all_of(V), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  ungroup()

positions <- c("DF", "MF", "FW")
nombres   <- c(DF = "Defensas", MF = "Mediocentros", FW = "Delanteros")
asign <- df %>%
  transmute(Player, Squad, grp,
            cluster = NA_integer_, rol = NA_character_)
Zlist <- list()

# Nombres de rol por posicion: (rol de contencion, rol creativo/de proyeccion)
ROLES <- list(DF = c("Central de contención", "Defensa de proyección"),
              MF = c("Mediocentro de contención", "Mediocentro de creación"),
              FW = c("Delantero finalizador", "Atacante asociativo"))
CREATIVAS <- c("PrgR_p90", "xA_p90", "KP_p90", "PPA_p90")  # definen el cluster creativo

# ---------------- 2. Clustering y validación por posición ----------------
for (p in positions) {
  idx <- which(df$grp == p)
  Z   <- scale(as.matrix(F[idx, V]))      # tipificación z dentro de la posición
  Zlist[[p]] <- Z
  
  cat("\n==================  ", nombres[p], "  (n =", length(idx), ")\n")
  
  # 2.1 Auditoría multimétrica para elegir k (silueta, codo, DB, CH)
  D <- dist(Z)
  for (k in 2:8) {
    km   <- kmeans(Z, centers = k, nstart = 20)
    sil  <- mean(silhouette(km$cluster, D)[, 3])
    crit <- intCriteria(Z, as.integer(km$cluster),
                        c("Davies_Bouldin", "Calinski_Harabasz"))
    cat(sprintf("  k=%d  silueta=%.3f  DB=%.3f  CH=%.0f  inercia=%.0f\n",
                k, sil, crit$davies_bouldin, crit$calinski_harabasz, km$tot.withinss))
  }
  
  # 2.2 Solución final con k = 2 (óptimo según la auditoría)
  km <- kmeans(Z, centers = 2, nstart = 50)
  asign$cluster[idx] <- km$cluster
  
  # Etiquetar el rol: el cluster con mayor media en las variables creativas es el
  # rol creativo/de proyeccion; el otro, el de contencion (mismo criterio que el pipeline)
  mu_cre <- sapply(1:2, function(c) mean(colMeans(Z[km$cluster == c, CREATIVAS, drop = FALSE])))
  crea   <- which.max(mu_cre)
  asign$rol[idx] <- ifelse(km$cluster == crea, ROLES[[p]][2], ROLES[[p]][1])
  
  # 2.3 Perfil medio de cada cluster (en z)
  prof <- aggregate(Z, by = list(cluster = km$cluster), FUN = mean)
  cat("  Perfil medio por cluster (z):\n"); print(round(prof, 2))
  
  # 2.4 Validación con clustering jerárquico (Ward) -> índice de Rand ajustado
  hc  <- cutree(hclust(D, method = "ward.D2"), k = 2)
  cat(sprintf("  ARI K-Means vs jerárquico = %.3f\n", adjustedRandIndex(km$cluster, hc)))
  
  # 2.5 PCA: varianza explicada y cargas de PC1
  pc <- prcomp(Z)
  ve <- pc$sdev^2 / sum(pc$sdev^2) * 100
  cat(sprintf("  PCA: PC1=%.1f%%  PC2=%.1f%%  (2 PC=%.1f%%, 5 PC=%.1f%%)\n",
              ve[1], ve[2], sum(ve[1:2]), sum(ve[1:5])))
  cargas <- sort(pc$rotation[, 1], decreasing = TRUE)
  cat("  Cargas PC1 (top 4):", paste(names(head(cargas, 4)), collapse = ", "), "\n")
  
  # 2.6 Importancia de variables (Random Forest sobre la etiqueta de cluster)
  rf  <- randomForest(x = Z, y = as.factor(km$cluster), ntree = 300, importance = TRUE)
  imp <- sort(importance(rf, type = 2)[, 1], decreasing = TRUE)
  cat("  Importancia (top 4):", paste(sprintf("%s=%.3f", names(head(imp,4)),
                                              head(imp,4)/sum(imp)), collapse = ", "), "\n")
}
write.csv(asign, "clusters_asignados.csv", row.names = FALSE)

# ---------------- 3. Figuras ----------------
# 3.1 Selección de k (silueta + codo) por posición
plots <- list()
for (p in positions) {
  Z <- Zlist[[p]]; D <- dist(Z); sil <- c(); ine <- c()
  for (k in 2:8) {
    km <- kmeans(Z, centers = k, nstart = 20)
    sil <- c(sil, mean(silhouette(km$cluster, D)[, 3])); ine <- c(ine, km$tot.withinss)
  }
  d <- tibble(k = 2:8, silueta = sil, inercia = ine)
  plots[[paste0(p,"s")]] <- ggplot(d, aes(k, silueta)) + geom_line(color="#2E5496") +
    geom_point(color="#2E5496") + geom_vline(xintercept=2, linetype="dashed", color="#C0504D") +
    labs(title = paste(nombres[p], "- silueta"), x="k", y="silueta") + theme_minimal()
  plots[[paste0(p,"i")]] <- ggplot(d, aes(k, inercia)) + geom_line(color="#1F3864") +
    geom_point(color="#1F3864") + geom_vline(xintercept=2, linetype="dashed", color="#C0504D") +
    labs(title = paste(nombres[p], "- codo"), x="k", y="inercia") + theme_minimal()
}
ggsave("figuras/fig_ad_seleccion_k.png",
       (plots$DFs|plots$MFs|plots$FWs)/(plots$DFi|plots$MFi|plots$FWi),
       width = 13, height = 7, dpi = 140)

# 3.2 Dendrograma (Mediocentros)
png("figuras/fig_ad_dendrograma.png", width = 1400, height = 600, res = 130)
plot(hclust(dist(Zlist$MF), method = "ward.D2"), labels = FALSE,
     main = "Dendrograma (Ward) - Mediocentros", xlab = "", sub = "")
dev.off()

# 3.3 PCA por posición (coloreado por cluster)
for (p in positions) {
  km <- kmeans(Zlist[[p]], centers = 2, nstart = 50)
  g <- fviz_cluster(list(data = Zlist[[p]], cluster = km$cluster),
                    geom = "point", ellipse.type = "norm",
                    main = paste("PCA -", nombres[p])) + theme_minimal()
  ggsave(paste0("figuras/fig_ad_pca_", p, ".png"), g, width = 5, height = 4, dpi = 140)
}

# 3.4 Heatmap de perfiles (6 roles x 18 variables)
rol <- c("Contención","Proyección","Contención","Creación","Asociativo","Finalizador")
M <- c(); fil <- c(); r <- 1
for (p in positions) {
  km <- kmeans(Zlist[[p]], centers = 2, nstart = 50)
  pr <- aggregate(Zlist[[p]], by = list(km$cluster), FUN = mean)[, -1]
  for (c in 1:2) { M <- rbind(M, as.numeric(pr[c, ])); fil <- c(fil, paste(p, rol[r])); r <- r+1 }
}
colnames(M) <- V; rownames(M) <- fil
library(reshape2)
mm <- melt(M)
ggplot(mm, aes(Var2, Var1, fill = value)) + geom_tile() +
  geom_text(aes(label = sprintf("%.1f", value)), size = 2) +
  scale_fill_gradient2(low="#2E5496", mid="white", high="#C0504D", limits=c(-1.2,1.2)) +
  labs(title="Perfil medio de cada rol (z)", x=NULL, y=NULL, fill="z") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
ggsave("figuras/fig_ad_perfiles.png", width = 13, height = 4.6, dpi = 140)

cat("\nListo. Asignaciones en clusters_asignados.csv y figuras en /figuras\n")
