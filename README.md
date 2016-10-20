# Instrucciones de scripts para el análisis de ENVE 2014 en R

## Análisis elaborado para presentación en ASC2016

*Patricio R. Estévez Soto*  

## Versión 0.1

Estas instrucciones detallan brevemente el procedimiento necesario para correr exitosamente los scripts diseñados para el análisis de la ENVE 2014 en R.

Dado que los microdatos de la ENVE son de acceso controlado, este script fue enviado a la Dirección de Microdatos del INEGI, quien lo ejecuta en nombre del investigador.

La ejecución consta principalmente de dos partes, la **Instalación de paquetes requeridos**, y la **Ejecución del script**.

La instalación de paquetes solo se requiere realizar una vez. La ejecución del script puede realizarse las veces que sean necesarias si se encuentran errores.

## Instalación de paquetes requeridos

Los paquetes requeridos por este proyecto son los siguientes:

- **foreign**: Importa archivos .dbf a R
- **ggplot2**: Genera gráficos más atractivos que R básico
- **Cairo**: Permite guardar los gráficos como archivos png
- **xtable**: Genera tablas sencillas en formato LaTeX
- **texreg**: Genera tablas complejas en formato LaTeX
- **lmtest**: Realiza pruebas de hipótesis al comparar modelos
- **classInt**: Genera intervalos para quantiles.
- **knitr**: Permite correr el script entero para e incorporar los resultados en un arhcivo de texto con formato markdown (extensión .md).
- **lme4**: Paquete para correr modelos multinivel.
- **car**: Para realizar pruebas de hipótesis.
- **pscl**: Pruebas de hipótesis
- **sjstats**: Calcula la ICC para modelos de lme4
- **sjPlot**: Elabora gráficas para modelos lme4
- **rmarkdown**: Utilizado en combinación con knitr
- **arm**: Requerido por sjPlot


Algunos de estos paquetes pueden ya estar instalados en nuestra computadora. Aquellos que no estén instalados pueden instalarse mediante los siguientes comandos:

```
install.packages("foreign")
install.packages("ggplot2")
install.packages("Cairo")
install.packages("knitr")
install.packages("lme4")
install.packages("classInt")
install.packages("car")
install.packages("texreg")
install.packages("xtable")
install.packages("lmtest")
install.packages("pscl")
install.packages("sjstats")
install.packages("sjPlot")
install.packages("rmarkdown")
install.packages("arm")
```

Para verificar si los paquetes se encuentran instalados puede correr lo siguientes comandos:

```
packages <- c("foreign",
              "ggplot2",
              "Cairo",
              "knitr",
              "lme4",
              "classInt",
              "car",
              "texreg",
              "xtable",
              "lmtest",
              "pscl",
              "sjstats",
              "sjPlot",
              "rmarkdown",
              "arm")


packages %in% rownames(installed.packages())
```

Si todas las respuestas son positivas (`TRUE`), todos los paquetes requeridos están instalados.

## Ejecución del script

El script está diseñado para correr sin requerir ningún tipo de intervención para su ejecución. Una vez puesto en marcha, el script generará el reporte y los archivos relevantes en la carpeta del proyecto para revisión.

Además de la instalación de los paquetes necesarios, descrita en la sección anterior, la única intervención necesaria es colocar los archivos .dbf que contienen los datos de la ENVE en el folder del proyecto y asignar el *working directory* en R al folder del proyecto.

### Archivo .dbf ENVE 2014

Copie los archivos **"TR_ENVE_CUES_2014.dbf"** y **TR_ENVE_DELITOS2014.dbf** al folder de este proyecto.

### Establecer working directory en R

Establezca el *working directory* en R para que apunte al folder del proyecto.

Por ejemplo:
```
getwd() # Devuelve cuál es el working directory actual.

setwd("~/R/ENVE_ASC2016") # Establece el working directory en el folder del proyecto
```

Sólo se requiere que el *path* al folder del proyecto sea especificado.

### Ejecutar el script

El proyecto está contenido en un archivo [R Markdown](http://rmarkdown.rstudio.com) que permite elaborar reportes automáticos de análisis realizados en R. Cuando el script termina, guarda los resultados en una archivo de texto con formato markdown (extensión `.md`) que puede ser abierto en cualquier editor de texto en cualquier computadora (Notepad, etc.).

Para correr el análisis, ejecute los siguientes comandos en la consola de R:

```
require(knitr)
knit("ASC2016_Extortion_Report.Rmd")
```

La ejecución es tardada, pero depende del equipo en el que se corra; es normal que parezca que no hace nada.

## Resultados

El script automáticamente realizará los análisis requeridos y guardará los resultados en un archivo llamado **ASC2016_Extortion_Report.md** en la carpeta del proyecto. Asimismo, el programa guardará en una subcarpeta llamada `figure/` las gráficas elaboradas en el análisis.

## Errores

Los scripts han sido probados con un archivo .dbf del mismo nombre y estructura (pero datos falsos) que el archivo .dbf de la ENVE 2014. Ha sido probado en plataformas Mac, Windows y Ubuntu Linux y ha corrido sin errores.

El script está diseñado para correr hasta el final aún y cuando se generen erroes.

## Análisis

El script de análisis está dividido en dos secciones. La primera realiza una batería de análisis exploratorios (EDA) para describir la distribución de los datos, así como para explorar las relaciones bivariadas entre la variable dependiente, y las variables independientes seleccionadas.

La segunda sección realiza modelos estadísticos logit en versión uninivel y multinivel.

## Licencia

The MIT License (MIT)

Copyright (c) 2016 Patricio Rodrigo Estévez Soto

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
