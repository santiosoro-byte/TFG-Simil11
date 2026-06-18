Símil11 — Sistema de recomendación de futbolistas por similitud de estilo

Trabajo de Fin de Grado (Grado en Business Analytics y ADE). Recomendador de
futbolistas por parecido de estilo de juego en las cinco grandes ligas europeas,
a partir de datos públicos de rendimiento, valor de mercado y salario de la
temporada 2025-26.

Herramienta online: https://github.com/santiosoro-byte/TFG-Simil11

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


Temporada 2025-26. Los datos de origen son públicos; en el repositorio se incluyen
los conjuntos ya procesados para poder reproducir el análisis.

Contenido del repositorio


index.html — herramienta web (Símil11), autocontenida; se abre en el navegador.
Scripts de R del proyecto (ver orden de ejecución más abajo).
master_con_valor.csv — estadísticas de rendimiento y valor de mercado cruzados;
es la entrada del análisis.
master_modelo.csv — conjunto final del pipeline, con las 18 variables de estilo
ya calculadas.


Cómo ejecutarlo

Requiere R (versión 4.x). Instala los paquetes una sola vez:

rinstall.packages(c("tidyverse", "cluster", "clusterCrit", "mclust",
                   "randomForest", "factoextra", "patchwork", "reshape2",
                   "fmsb", "worldfootballR"))

Descarga el repositorio en una carpeta y ejecuta R desde ella (o usa
setwd("ruta/a/la/carpeta")), de modo que los scripts encuentren los CSV, que
leen y escriben en el directorio de trabajo.

Orden de ejecución:


Dataset.R — lee e integra las cuatro tablas de FBref de las cinco ligas.
Descarga_valores.R — descarga los valores de mercado de Transfermarkt.
Valor_de_mercado.R — cruza el valor con los jugadores y genera master_con_valor.csv.
Limpieza.R — limpieza, corrección de errores e imputación.
EDA.R — análisis exploratorio y figuras.
Modelado.R — clustering y validación; genera clusters_asignados.csv.
Recomendador.R — recomendaciones, validación de roles y diagramas de telaraña.


La herramienta web (index.html) es autocontenida: lleva los datos dentro y no
necesita ejecutar nada para funcionar.
Autor

Santiago Osoro Brandeiro
Autor

Santiago Osoro Brandeiro
