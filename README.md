Símil11 — Sistema de recomendación de futbolistas por similitud de estilo

Trabajo de Fin de Grado (Grado en Business Analytics y ADE). Recomendador de
futbolistas por parecido de estilo de juego en las cinco grandes ligas europeas,
a partir de datos públicos de rendimiento, valor de mercado y salario de la
temporada 2025-26.

Herramienta online: https://USUARIO.github.io/REPO/

Qué hace


Agrupa a 1.021 jugadores de campo en seis roles funcionales mediante clustering
(K-Means, dos roles por posición).
Recomienda, dentro de cada rol, los jugadores de perfil más parecido a uno dado,
con un porcentaje de similitud calibrado.
Permite filtrar las alternativas por valor de mercado, salario y edad.


Datos


Rendimiento: FBref (datos de StatsBomb).
Valor de mercado: Transfermarkt (vía el paquete worldfootballR).
Salario: Capology (a través de FBref).


El conjunto ya procesado está en data/. Los datos de origen son públicos; aquí
se incluye el resultado del pipeline para poder reproducir el análisis.

Estructura del repositorio


index.html — herramienta web (Símil11), autocontenida.
R/ — scripts de R del proyecto.
data/ — conjunto de datos procesado (master_con_valor.csv, master_modelo.csv).
figuras/ — figuras generadas por los scripts.


Cómo reproducirlo

Requiere R. Paquetes principales: tidyverse, cluster, clusterCrit, mclust,
randomForest, factoextra, patchwork, reshape2, fmsb, worldfootballR
(más los que utilice el ETL).

Orden de ejecución:


Ingeniería del Dato (ETL): lectura de las fuentes, descarga del valor de
mercado, cruce, limpieza y análisis exploratorio. Generan master_con_valor.csv
y master_modelo.csv.
Modelado.R — clustering y validación. Genera clusters_asignados.csv.
Recomendador.R — recomendaciones, validación y figuras.


Autor

Santiago Osoro Brandeiro
