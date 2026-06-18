Símil11 — Sistema de recomendación de futbolistas por similitud de estilo

Trabajo de Fin de Grado (Grado en Business Analytics y ADE). Recomendador de
futbolistas por parecido de estilo de juego en las cinco grandes ligas europeas,
a partir de datos públicos de rendimiento, valor de mercado y salario de la
temporada 2025-26.

Herramienta online: https://santiosoro-byte.github.io/TFG-Simil11/

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
los conjuntos ya procesados para poder reproducir el análisis sin tener que volver
a descargarlo todo.

Contenido del repositorio


index.html — herramienta web (Símil11), autocontenida; se abre en el navegador.
Siete scripts de R: cinco para la construcción y depuración del conjunto de datos
y dos para el análisis (ver orden más abajo).
master_con_valor.csv — conjunto con las estadísticas, el valor de mercado, el
salario y la edad ya cruzados; es la entrada del análisis.
master_modelo.csv — conjunto final, con las 18 variables de estilo ya calculadas.


Cómo ejecutarlo

Requiere R (versión 4.x). Instala los paquetes una sola vez:

rinstall.packages(c("tidyverse", "cluster", "clusterCrit", "mclust",
                   "randomForest", "factoextra", "patchwork", "reshape2",
                   "fmsb", "remotes"))
remotes::install_github("JaseZiv/worldfootballR", upgrade = "never")

Descarga el repositorio en una carpeta y ejecuta R desde ella (o usa
setwd("ruta/a/la/carpeta")), de modo que cada script encuentre los CSV, que se
leen y se escriben en el directorio de trabajo.

Orden de ejecución del pipeline completo:


Dataset.R — integra las cuatro tablas de FBref (Standard, Passing, Defensive y
Salaries) de las cinco ligas, filtra a los jugadores de campo con 450 minutos o
más y genera master_jugadores_campo.csv.
Descarga_valores.R — descarga los valores de mercado de Transfermarkt y genera
valores_transfermarkt.csv.
Valor_de_mercado.R — cruza los dos ficheros anteriores en cascada (nombre,
nombre y club, diccionario y coincidencia difusa) y genera master_con_valor.csv.
Limpieza.R — deriva las variables por noventa minutos, poda las redundantes por
correlación, imputa los valores ausentes y genera master_modelo.csv, junto con
las figuras de antes y después.
EDA.R — produce las figuras del análisis exploratorio y la tabla de descriptivos
por posición.
Modelado.R — clustering por posición y validación; genera clusters_asignados.csv.
Recomendador.R — recomendaciones por rol, validación y diagramas de telaraña.


En el repositorio están los dos conjuntos ya procesados, master_con_valor.csv y
master_modelo.csv. Con el primero se reproduce directamente el análisis, que es el
núcleo del trabajo: limpieza, exploración, clustering y recomendador (pasos 4 a 7).
Los tres primeros scripts documentan cómo se construyó ese fichero a partir de las
fuentes originales; para volver a ejecutarlos hacen falta las tablas descargadas de
FBref y conexión con Transfermarkt.

La herramienta web (index.html) es autocontenida: lleva los datos dentro y no
necesita ejecutar nada para funcionar.

Autor

Santiago Osoro Brandeiro
