# ============================================================
#  Recomendador.R  ·  PASO 2 del Analisis del Dato
#  Sistema de recomendación por similitud
#  Modelo: perfil de 18 métricas en PERCENTIL dentro de la posición
#          (el de la telaraña) + distancia euclídea
#          + calibración POR ROL:  similitud = 99 - 59 * sqrt(F)
#          Recomendación dentro del MISMO ROL.
#  >>> EJECUTA ANTES Modelado.R (genera clusters_asignados.csv). <<<
#  Entrada : master_con_valor.csv  +  clusters_asignados.csv
#  Si faltara algun rol, el script lo deriva solo (misma receta que Modelado.R).
#  Paquetes: tidyverse, fmsb
# ============================================================
# install.packages(c("tidyverse","fmsb"))
library(tidyverse)
library(fmsb)
set.seed(42)
# setwd("C:/Users/cbran/OneDrive/Documentos/Datos-TFG")

df <- read.csv("master_con_valor.csv", check.names = FALSE, stringsAsFactors = FALSE)
df$grp <- sub("-.*", "", df$Pos_primary)
# si el master no trae valor/salario, se crean vacios para que el script corra igual
if (!"tm_valor_millones"   %in% names(df)) df$tm_valor_millones   <- NA_real_
if (!"sal_wage_annual_eur" %in% names(df)) df$sal_wage_annual_eur <- NA_real_

# ---- 18 variables de estilo (identico a Modelado.R) ----
s90 <- df[["90s"]]; s90[s90 == 0] <- NA
F <- tibble(
  Gls_p90=df[["Gls_p90"]], npxG_p90=df[["npxG_p90"]], Ast_p90=df[["Ast_p90"]], xAG_p90=df[["xAG_p90"]],
  xA_p90=df[["pass_xA"]]/s90, KP_p90=df[["pass_KP"]]/s90, PPA_p90=df[["pass_PPA"]]/s90, CrsPA_p90=df[["pass_CrsPA"]]/s90,
  PrgC_p90=df[["PrgC"]]/s90, PrgP_p90=df[["PrgP"]]/s90, PrgR_p90=df[["PrgR"]]/s90, Cmp_p90=df[["pass_Cmp_tot"]]/s90,
  CmpPct=df[["pass_CmpPct_tot"]], Tkl_p90=df[["def_Tkl"]]/s90, Int_p90=df[["def_Int"]]/s90,
  Blocks_p90=df[["def_Blocks"]]/s90, Clr_p90=df[["def_Clr"]]/s90, ChalTklPct=df[["def_ChalTklPct"]])
V <- names(F); F$grp <- df$grp
F <- F %>% group_by(grp) %>% mutate(across(all_of(V), ~ifelse(is.na(.), median(.,na.rm=TRUE), .))) %>% ungroup()

# ---- ROLES: del clusters_asignados.csv si trae 'rol'; si no, derivarlos aqui ----
ROLES <- list(DF=c("Central de contención","Defensa de proyección"),
              MF=c("Mediocentro de contención","Mediocentro de creación"),
              FW=c("Delantero finalizador","Atacante asociativo"))
CRE <- c("PrgR_p90","xA_p90","KP_p90","PPA_p90")   # definen el cluster creativo
derivar_roles <- function() {
  rol <- rep(NA_character_, nrow(df))
  for (p in c("DF","MF","FW")) {
    idx <- which(df$grp == p); Z <- scale(as.matrix(F[idx, V]))
    km  <- kmeans(Z, centers = 2, nstart = 50)
    mu  <- sapply(1:2, function(c) mean(colMeans(Z[km$cluster == c, CRE, drop = FALSE])))
    crea <- which.max(mu)
    rol[idx] <- ifelse(km$cluster == crea, ROLES[[p]][2], ROLES[[p]][1])
  }
  rol
}
# Se usan los roles que guardo Modelado.R en clusters_asignados.csv (asi el
# recomendador es coherente con el clustering y con la memoria). Red de seguridad:
# si falta el fichero, o algun jugador del master no esta en el (p. ej. tras
# regenerar el master), esos roles se derivan aqui con la MISMA receta -> nunca
# queda un rol vacio y la calibracion por rol no puede fallar.
df$rol <- NA_character_
if (file.exists("clusters_asignados.csv")) {
  asign <- read.csv("clusters_asignados.csv", stringsAsFactors = FALSE)
  if ("rol" %in% names(asign)) df$rol <- asign$rol[match(df$Player, asign$Player)]
}
if (all(is.na(df$rol))) {
  message("Aviso: no hay clusters_asignados.csv utilizable. Ejecuta antes Modelado.R. Derivando los roles aqui.")
  df$rol <- derivar_roles()
} else if (any(is.na(df$rol))) {
  faltan <- is.na(df$rol)
  message(sprintf("Aviso: %d jugador(es) sin rol en clusters_asignados.csv; se derivan aqui.", sum(faltan)))
  df$rol[faltan] <- derivar_roles()[faltan]
}

# ---- representacion percentil (telarana) y calibracion por rol ----
PCT <- F %>% group_by(grp) %>% mutate(across(all_of(V), ~ rank(.) / n() * 100)) %>% ungroup()
M   <- as.matrix(PCT[, V]); rownames(M) <- df$Player
CDF <- list()
for (r in unique(na.omit(df$rol))) { idx <- which(df$rol == r); CDF[[r]] <- ecdf(as.numeric(dist(M[idx, ]))) }
similitud <- function(i, j) {
  d <- sqrt(sum((M[i, ] - M[j, ])^2)); max(0, 99 - 59 * sqrt(CDF[[ df$rol[i] ]](d)))
}

# ---- 3) RECOMENDADOR dentro del ROL + filtros de negocio ----
recomendar <- function(nombre, n = 5, val_max = Inf, sal_max = Inf, edad_max = Inf) {
  i <- grep(nombre, df$Player, ignore.case = TRUE)[1]
  if (is.na(i)) { cat("No encuentro a", nombre, "\n"); return(invisible(NULL)) }
  cand <- which(df$rol == df$rol[i] & seq_len(nrow(df)) != i)
  if (is.finite(val_max))  cand <- cand[!is.na(df$tm_valor_millones[cand]) & df$tm_valor_millones[cand] <= val_max]
  if (is.finite(sal_max))  cand <- cand[!is.na(df$sal_wage_annual_eur[cand]) & df$sal_wage_annual_eur[cand]/1e6 <= sal_max]
  if (is.finite(edad_max)) { edad <- as.numeric(sub("-.*","",df$Age[cand])); cand <- cand[!is.na(edad) & edad <= edad_max] }
  if (length(cand) == 0) { cat("Sin candidatos que cumplan los filtros.\n"); return(invisible(NULL)) }
  sim <- vapply(cand, function(j) similitud(i, j), numeric(1))
  out <- data.frame(Player=df$Player[cand], Equipo=df$Squad[cand], similitud=round(sim,0), valor_M=df$tm_valor_millones[cand])
  head(out[order(-out$similitud), ], n)
}
cat("Similares a Vinicius:\n");              print(recomendar("Vinicius"))
cat("\nSimilares a Saliba:\n");              print(recomendar("Saliba"))
cat("\nSimilares a Haaland por <= 40 M:\n"); print(recomendar("Haaland", val_max = 40))

# ---- 4) Comparacion de metricas sobre el perfil (Tabla 9) ----
vecinos <- function(nombre, metrica="euclidea", n=5) {
  i <- grep(nombre, df$Player, ignore.case=TRUE)[1]; if (is.na(i)) return(NULL)
  cand <- which(df$rol==df$rol[i] & seq_len(nrow(df))!=i)
  a <- M[i, ]; B <- M[cand,,drop=FALSE]
  d <- switch(metrica,
              euclidea  = sqrt(rowSums((B - matrix(a,nrow(B),length(V),byrow=TRUE))^2)),
              manhattan = rowSums(abs(B - matrix(a,nrow(B),length(V),byrow=TRUE))),
              coseno    = 1 - (B %*% a)/(sqrt(rowSums(B^2))*sqrt(sum(a^2))))
  df$Player[cand][order(d)][1:n]
}
cat("\nVecinos de Bellingham segun la metrica:\n")
for (m in c("euclidea","coseno","manhattan")) cat(" ", m, ":", paste(vecinos("Bellingham", m), collapse=", "), "\n")

# ---- 5) VALIDACION: recuperacion del rol con k vecinos DENTRO DE LA POSICION (Tabla 14) ----
cat("\nRecuperacion del rol (k-NN dentro de la posicion, dejando uno fuera):\n")
acc_tot <- c()
for (p in c("DF","MF","FW")) {
  idx <- which(df$grp==p); rr <- df$rol[idx]; D <- as.matrix(dist(M[idx,])); diag(D) <- Inf
  for (k in c(5,10)) {
    pred <- sapply(seq_along(idx), function(t){ nn<-order(D[t,])[1:k]; names(which.max(table(rr[nn]))) })
    if (k==5) acc_tot <- c(acc_tot, pred==rr)
    cat(sprintf("  %s k=%d: %.1f%%\n", p, k, mean(pred==rr)*100))
  }
}
cat(sprintf("  Conjunto (6 roles, k=5): %.1f%%\n", mean(acc_tot)*100))

# ---- 6) SUSTITUIBILIDAD: cuasi-clones del rol con similitud >= 90 % (Tabla 13) ----
n90 <- function(nombre){ i<-grep(nombre,df$Player,ignore.case=TRUE)[1]; if(is.na(i)) return(NA_integer_)
cand<-which(df$rol==df$rol[i] & seq_len(nrow(df))!=i); if(length(cand)==0) return(0L)
sum(vapply(cand, function(j) similitud(i,j), numeric(1)) >= 90) }
cat("\nCuasi-clones del rol (similitud >= 90 %):\n")
for (nm in c("Pedri","Haaland","Mbapp","Vinicius","Caicedo","Saliba","Joe Rodon")) cat(sprintf("  %-14s %s\n", nm, n90(nm)))

# ---- 7) TELARANAS: perfiles en percentil ----
dir.create("figuras", showWarnings = FALSE)
perfil <- function(nombre){ i<-grep(nombre,df$Player,ignore.case=TRUE)[1]; as.numeric(M[i,]) }
telarana <- function(nombres, fichero, titulo, colores){
  datos <- as.data.frame(t(sapply(nombres, perfil))); colnames(datos) <- V
  datos <- rbind(rep(100,length(V)), rep(0,length(V)), datos)
  png(fichero, width=1100, height=1100, res=150)
  radarchart(datos, pcol=colores, pfcol=scales::alpha(colores,0.2), plwd=2, title=titulo,
             vlcex=0.7, axistype=1, caxislabels=c("0","25","50","75","100"))
  legend("topright", legend=nombres, col=colores, lwd=2, bty="n", cex=0.9); dev.off()
}
telarana(c("Vinicius Júnior","Lamine Yamal","Pedro Neto"), "figuras/fig_ad_telarana_similares.png",
         "Alta coincidencia de estilo", c("#2E5496","#C0504D","#2E7D32"))
telarana(c("Vinicius Júnior","Erling Haaland"), "figuras/fig_ad_telarana_contraste.png",
         "Perfiles distintos (asociativo vs finalizador)", c("#2E5496","#7030A0"))
cat("\nListo. Telaranas guardadas en /figuras\n")
