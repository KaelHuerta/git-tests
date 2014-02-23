
Big data :: Lecture 3
========================================================
author: Adolfo De Unánue T.
date: 18 de Febrero, 2014
font-import: http://fonts.googleapis.com/css?family=Risque
font-family: 'Risque'
css: ../css/itam_big_data.css


```{r,echo=FALSE,warning=FALSE, message=FALSE,error=FALSE}
require(ggplot2)
require(stringr)
require(lubridate)
require(plyr)
require(reshape2)
require(dplyr)
```

dplyr
=======================================================
type: exclaim


dplyr
=======================================================

- Siguiente generación de `plyr`
  - Aunque solo trabaja con `data.frames` (de ahí la `d`)

- Misma interfaz, no importa dónde están guardados los datos (`data.frame` , `data.table` o base de datos)

- Soporta las siguientes bases de datos: `SQLite`, `PostgreSQL`, `Amazon RedShift`, `MySQL`, `Google Bigquery`

dplyr
=======================================================

- Heritage Health Prize: [Datos para jugar](https://www.heritagehealthprize.com/c/hhp/data?HHP_release1.zip)

```{r}
library(dplyr)
claims <- read.table("HHP/HHP_release1/Claims_Y1.csv", sep=",", header=TRUE, colClasses=c(rep("factor", 7), "integer", "character", "character", "factor", "character"), comment.char="")
members <- read.table("HHP/HHP_release1/Members_Y1.csv", sep=",", header=TRUE, colClasses=rep("factor", 3), comment.char="")
dih <- read.table("HHP/HHP_release1/DayInHospital_Y2.csv", sep=",", colClasses=c("factor", "integer"), header=TRUE, comment.char="")
dim(claims)
dim(members)
dim(dih)
```


dplyr
=======================================================

```{r}
claims_tbl <- tbl_df(claims)
members_tbl <- tbl_df(members)
dih_tbl <- tbl_df(dih)
```
dplyr
=======================================================

```{r}
claims_tbl
members_tbl
dih_tbl
```

dplyr
=======================================================

```{r}
names(dih_tbl)[1] <- "MemberID" # Arreglando el nombre
saveRDS(object=claims_tbl, "claims.rds")
saveRDS(object=members_tbl, "members.rds")
saveRDS(object=dih_tbl, "dih.rds")
```



dplyr
=======================================================

- Para usar las funciones de `dplyr` es bueno saber lo siguiente

  - El primer argumento siempre es el `data.frame`
  - Los argumentos subsecuentes describen que hacer con el `data.frame`
  - Se puede referir a columnas sin usar `$`
  - El resultado es un `data.frame` nuevo



dplyr: Verbos
=======================================================

- `dplyr` usa los siguientes **verbos**:

  - `select()` subconjunto de columnas.
  - `filter()` subconjunto de renglones.
  - `mutate()` agregar nuevas columnas
  - `summarise()` reduce cada grupo a estadísticas.
  - `arrange()` reordena los renglones

dplyr: Verbos
======================================================
```{r}
filter(claims_tbl, specialty == 'Internal', placesvc=='Urgent Care' | placesvc == 'Ambulance')
```


dplyr: Verbos
=======================================================

```{r}
arrange(claims_tbl, specialty, placesvc, desc(paydelay))
```


dplyr: Verbos
=======================================================
```{r}
select(claims_tbl, specialty:paydelay)
```

dplyr: Verbos
=======================================================
```{r}
select(claims_tbl, -(specialty:paydelay))
```


dplyr: Verbos
=======================================================

- *Helper functions*

```{r}
toNumeric <- function(col) {
  sapply(strsplit(gsub("[^[:digit:]]+", " ", col, perl=TRUE), " ", fixed=TRUE), function(x) mean(as.numeric(x)))
}
```

```{r}
toDays <- function(col) {
  col = toNumeric(col)
  col.weeks <- grepl("week", col, fixed=TRUE)
  col[col.weeks] <- col[col.weeks]*7
  col[is.nan(col)] <- 0
  return(col)
}
```
```{r}
# Niveles
LoS.levels <- c("", "1 day", sprintf("%d days", 2:6), "1-2 weeks", "2-4 weeks", "4-8 weeks", "8-12 weeks", "12-26 weeks", "26+ weeks")

dsfs.levels <- c("0-1 month", sprintf("%d-%d months", 1:11, 2:12))
```

dplyr: Verbos
=======================================================
```{r}
claims_tbl <- mutate(claims_tbl,
                     LengthOfStay = factor(
                       str_replace_all(LengthOfStay, "- ", "-"),
                       levels=LoS.levels,
                       labels=c("0 days", LoS.levels[-1]), ordered = TRUE
                       ),
                     LenghtOfStayInDays = toDays(LengthOfStay),
                     dsfs.months=toNumeric(dsfs),
                     dsfs = factor(str_replace_all(dsfs, "- ", "-"), levels=dsfs.levels, ordered = TRUE),
                     CharlsonIndex = factor(toNumeric(CharlsonIndex), ordered=TRUE)
                     )
saveRDS(object=claims_tbl, file="claims_clean.rds")
```

dplyr: Verbos
=======================================================

```{r}
summarise(claims_tbl, mean.paydelay=mean(paydelay, na.rm=TRUE))
```

dplyr: Operaciones por grupo
========================================================

```{r}
specialties <- group_by(claims_tbl, specialty)
payment.delays.per.specialty <- summarise(specialties, count=n(), mean.paydelay=mean(paydelay, na.rm=TRUE))
filter(payment.delays.per.specialty, count > 100000, mean.paydelay > 45)
```

dplyr: Operaciones por grupo
========================================================

Si no queremos guardar los resultados intermedios usamos el operador `%.%`

```{r}
claims_tbl %.%
group_by(specialty) %.%
summarise(count=n(), mean.paydelay=mean(paydelay, na.rm=TRUE)) %.%
filter(count > 100000, mean.paydelay > 45)
```


dplyr: do()
========================================================

- `do()` es equivalente a `dlply()` de `plyr`

```{r}
# Esto es un modelo muy sencillo, no tiene nada de gracia
do(group_by(claims_tbl, specialty), failwith(NULL, lm), formula = paydelay ~ LenghtOfStayInDays)
```


dplyr: Verbos binarios
==========================================================

- Reciben dos `data.frame` como parámetros

  * `inner_join(x, y)`: `x + y`

  * `left_join(x, y)`: todo `x` + lo que haga *match* en `y` (todas las columnas de `x` y `y`)

  * `semi_join(x, y)`: todo `x` con *match* en `y` (sólo las columnas de `x`)

  * `anti_join(x, y)`: todo `x` que **no** haga *match* con `y` (sólo las columnas de `x`)

dplyr: Verbos binarios
==========================================================
```{r}
claims_tbl <- inner_join(claims_tbl, members_tbl)
claims_tbl
saveRDS(object=claims_tbl, file="claims_clean.rds")
```

dplyr: Guardar a base de datos
===========================================================

```{r}
# Preparamos la base de datos
my_db <- src_sqlite("claims.sqlite3", create=TRUE)
# Lo guarda en una base de datos SQLite
# Observa que no es un objeto tbl, si no un data.frame
#claims_sqlite <- copy_to(my_db, claims, temporary = FALSE, indexes = list(c("MemberID", "ProviderID", "vendor", "specialty", "placesvc"))) # Recuerda que esto está sucio
# En la vida real, sería mejor cargar externamente el data.frame
# Si ya está cargada en la base de datos
claims_sqlite <- tbl(my_db, sql("select * from claims"))
claims_sqlite # Sólo ejecuta el query para traer los primeros 10 registros
```

dplyr: Guardar a base de datos
===========================================================

- Cuando estamos trabajando con bases de datos y `dplyr`:
  - Nunca *jala* información a `R` a menos que se pida explícitamente
  - Retrasa todo el trabajo y lo hace en un solo paso
  
- Es decir `dplyr` con base de datos es `lazy`
  - Más adelante veremos que esto es muy parecido a `Apache Pig`
  
dplyr: Guardar a base de datos
=============================================================
Ejemplo de *Lazyness*

```{r}
c1 <- claims_sqlite %.%
  filter(paydelay > 20) %.%
  select(specialty, placesvc, paydelay, dsfs) %.%
  arrange(desc(specialty), paydelay)

c1 # Hasta aquí se ejecuta el SQL

```

dplyr: Guardar a base de datos
=============================================================

- Viendo que pasó ...
```{r}
c1$query
explain(c1)
```

Esto se puede establecer por default con `options(dplyr.show_sql = TRUE)`
y `options(dplyr.explain_sql = TRUE)`
  - Ejecuta el código de `c1`, luego de activar estas opciones en tu sesión de `R`.


dplyr: Guardar a base de datos
=============================================================

Notas:

* `PostgreSQL` y `Bigquery` lo veremos en las clases correspondientes

dplyr: Guardar a base de datos
=============================================================

- Si queremos forzar el `query`, tenemos 2 funciones principalmente

- `collect()` ejecuta el `query` y trae los resultados a `R`.
- `compute()` ejecuta el `query` y guarda los resultados en una tabla temporal en la base de datos.


dplyr: Window functions
=======================================================

- Una `window function` es una variación de una función de agregación (como `sum()` o `mean()`)
- Una función de agregación, toma `n` entradas y regresa 1 resultado.
- Un `window function` recibe `n` y regresa `n`
- Los veremos en `PostgreSQL` y regresaremos a `dplyr` ahí.

dplyr: Window functions
=======================================================

Notas:
  - `sqlite` no tiene `window functions`, así que `group_by` sólo tiene sentido con `summarise`

Bigmemory
=======================================================
type: exclaim

Bigmemory
=======================================================

* Crea, almacena, accesa y manipula matrices masivas (Multi gigabyte data).

* Incluye un framework en `C++` para desarrollar nuevos algoritmos o herramientas.

* Los datos se colocan en memoria compartida.
  - Esto permite compartirlas entre diferentes sesiones de `R`.

* Pueden ser respaldados a disco.

* Sólo matrices, no `data.frames`.
  - Todos los datos deben de ser del mismo tipo

Bigmemory
=======================================================

* Compatible con funciones estadísticas estándar.

* Resumiendo, datos numéricos que caben en la memoria de la computadora...

Bigmemory y sus amigos
=======================================================

```{r}
library(bigmemory)
library(biglm)
library(biganalytics)
library(bigtabulate)
```


Ejemplo: HMDA
=======================================================

* 2009 Home Mortgage Disclosure Act (HMDA) Loan Application Register (LAR) Data

* Se encuentra en [Data.gov](http://www.ffiec.gov/hmdarawdata/LAR/National/2009HMDALAR%20-%20National.zip)

* El diccionario de datos, se encuentra en la misma carpeta que los datos.

Ejemplo: HMDA
=======================================================
Nombres de las columnas

```{r}
    nombres.cols <- c(
    "as.of.year","respondent.id","agency.code","loan.type","property.type","loan.purpose",
    "occupancy","loan.amount.Ks","preapproval","action.type","msa.md","state.code","county.code",
    "census.tract.number","applicant.ethnicity","co.applicant.ethnicity","applicant.race.1",
    "applicant.race.2","applicant.race.3","applicant.race.4","applicant.race.5","co.applicant.race.1",
    "co.applicant.race.2","co.applicant.race.3","co.applicant.race.4","co.applicant.race.5",
    "applicant.sex","co.applicant.sex","applicant.income.Ks","purchaser.type","denial.reason.1",
    "denial.reason.2","denial.reason.3","rate.spread","hoepa.status","lien.status","edit.status",
    "sequence.number","population","minority.population.perc","hud.median.family.income",
    "tract.to.msa.md .income.perc","number.of.owner.occupied.units","number.of.1.to.4.family.units",
    "application.date.indicator")
```

Ejemplo: HMDA
=======================================================
```
system.time(hmda <- read.big.matrix("hmda/2009HMDALAR-National.CSV",
    sep=",", header=FALSE, type="integer", col.names=nombres.cols,
    backingfile="2009-hmda.bin",
    descriptor="2009-hmda.desc"))
    
## user system elapsed
## 245.380 4.209 278.198
```

Ejemplo: HMDA
=======================================================

```{r}
hmda.desc <- dget("2009-hmda.desc")
hmda <- attach.big.matrix(hmda.desc)
```

Ejemplo: HMDA
=======================================================

```{r}
dim(hmda)
gc()
```

Ejemplo: HMDA
=======================================================
* `colmean, colmin, colmax, colrange, colvar, colsum, colprod, colna`

```{r}
colmean(hmda,cols="population", na.rm=TRUE)
colrange(hmda,cols="applicant.income.Ks", na.rm=TRUE)
```

Ejemplo: HMDA
=======================================================

```{r}
system.time(median(hmda[, "applicant.income.Ks"], na.rm=TRUE)) # Soporta las funciones de R base
system.time(mean(hmda[, "applicant.income.Ks"], na.rm=TRUE)) # Soporta las funciones de R base
system.time(colmean(hmda,cols="applicant.income.Ks", na.rm=TRUE))
```


Ejemplo: HMDA
=======================================================
    > summary(hdma)
                                        min max mean NAs
    as.of.year 2.009000e+03 2.009000e+03 2.009000e+03 0.000000e+00
    respondent.id 0.000000e+00 2.140700e+09 6.667449e+07 2.213049e+06
    agency.code 1.000000e+00 7.000000e+00 3.129064e+00 0.000000e+00
    loan.type 1.000000e+00 4.000000e+00 1.365259e+00 0.000000e+00
    property.type 1.000000e+00 3.000000e+00 1.026707e+00 0.000000e+00
    loan.purpose 1.000000e+00 3.000000e+00 2.316770e+00 0.000000e+00

    ...
    
Ejemplo: HMDA
=======================================================

```{r, cache=TRUE}
mod1 <- biglm.big.matrix(loan.amount.Ks ~ applicant.ethnicity +
                           applicant.sex + applicant.income.Ks,
                         data=hmda)
summary(mod1)
```

Ejemplo: HMDA
=======================================================
* Muestreo: 1,000 renglones y 15 columnas

```{r, cache=TRUE}
sampled.rows <- sample(nrow(hmda), 1000)
sampled.cols <- sample(ncol(hmda), 15)

system.time(hmda.sample <- hmda[sampled.rows, sampled.cols])
```

Ejemplo: HMDA
=======================================================
```{r}
dim(hmda.sample)

colnames(hmda.sample)

rm(hmda.sample)

rm(hmda)
```

ff
======================================================

* No está limitado a usar datos numéricos

* Si los `data.frame` son mas grandes que la memoria.

* `ff` desacopla los datos del intérprete de `R`, los guarda en disco.

* Cada operación sobre los datos se ejecuta cargando pedazos de los datos en memoria para manipularlos.

ff
======================================================

- En este caso, quizá haya que considerar mejor poner los datos en alternativas e interactuar con `R` e.g.
  
  * En una base de datos relacional ó
  
  * Usar Apache Hadoop.
  
  * Es mi opinión :)

- Por si no lo habían notado, este es el "truco" de **SAS**
  
  
ff
=====================================================
```{r}
# library(ff)
# library(ffbase)

# system.time(hmda <- read.table.ffdf(file="hmda/2009HMDALAR-National.CSV", header=TRUE, sep=",", FUN="read.csv", na.strings="", colClasses=rep("factor", 45))) # Agregen un VERBOSE=TRUE

# user system elapsed
#1373.74 18.03 1394.91

##

# dim(hmda)
#[1] 19493490 45

#rm(hmda)
```

`foreach`
=======================================================
Ejemplo "Hola mundo"-ish

```{r}
library(foreach)
library(doParallel)
registerDoParallel(cores=3)

foreach(i=1:3) %dopar% sqrt(i)


```

`foreach`
=======================================================
* Combinando la salida

```{r}
ans <- foreach(i = 1:10, .combine = c) %dopar% {
  i^2
}
ans
```

Ejercicio: RITA
=======================================================
* La usaremos para ver un ejemplo combinado de `bash`, `R`.

* Se encuentra en [Airline on-time performance](http://stat-computing.org/dataexpo/2009/) de la ASA Sections on Statistical Graphics and Statistical Computing.

* 123,544,890 de registros.

Ejercicio: RITA
=======================================================

* 29 variables:

      `Year,Month,DayofMonth,DayOfWeek,DepTime,CRSDepTime,ArrTime,`
      `CRSArrTime,UniqueCarrier,FlightNum,TailNum,ActualElapsedTime,`
      `CRSElapsedTime,AirTime,ArrDelay,DepDelay,Origin,Dest,Distance,`
      `TaxiIn,TaxiOut,Cancelled,CancellationCode,Diverted,`
      `CarrierDelay,WeatherDelay,NASDelay,SecurityDelay,LateAircraftDelay`

* Las variables son enteros o categóricos (codificados como enteros).
  - Pero no de inicio (están como `character`)

Ejercicio: RITA

Descarga
=======================================================

- `01_download.sh`

```{bash}
  #!/bin/bash
  
  [-d rita.urls] && rm rita.urls
  
  for year in {1987..2008}
  do
  echo "http://stat-computing.org/dataexpo/2009/$year.csv.bz2" >> rita.urls
  done
  
  # Descargamos en paralelo
  cat rita.urls | parallel curl -O
```

- `02_create_csv.sh`

```{bash}
  #!/bin/bash
  
  for f in *.bz2
  do
    bunzip2 -k "$f"
    cat "${f%.*}" >> rita.csv
  done
```

Descomprimímos
=======================================================

Los *headers* se mezclan por todo el *set* de datos, mejor versión:

- `02_create_csv.sh`

```{bash}

  #!/bin/bash

  for f in *.bz2
  do
  bunzip2 -k "$f"
  # Guardamos el header en una variable temporal
  header=$(head -1 "${f%.*}");
  # ¿Qué hace la siguiente línea? ¿Se puede hacer de otra manera
  # Además borro por default el archivo original csv, para ahorrar espacio
  # Pero si hay errores, hay que empezar de nuevo ...
  awk FNR-1 "${f%.*}" >> rita.csv && rm "${f%.*}"
  done
  # ¿Qué hace esta línea?
  sed -i "1i$header" rita.csv
```

Descomprimímos
=======================================================
type: alert

- Una idea: ¿Y si lo hacemos en paralelo?
  
```{bash}
ls *.bz2 | time parallel -j+0 --eta 'bzcat {} | awk FNR-1 >> rita.csv'
```
- Lamentablemente, esto falla :(

  * El operador `>>` no garantiza que no haya colisiones si la inserción es más grande que el
  `PIPE_BUF` o `PIPE_SIZE`
  
  - Estas variables se pueden modificar, pero habría que *pimpear* el SO y probablemente el disco.
    - Lo veremos en bases de datos...

Descomprimímos
=======================================================

```{bash}

  #!/bin/bash
  
  # Descomprimimos en paralelo
  # Muy rápido
  ls *.bz2 | parallel -j +0 --eta 'bzcat {} > {.}'
  
  # Lento, pero seguro ...
  for year in {1987..2008}
  do
    cat $year.csv | sed '1d' >> rita.csv
  done
```
  


Descomprimímos
=======================================================
type: alert

```{bash}
ls -lh rita.csv
wc -l rita.csv
```

* 12 Gb (!!) en `rita.csv`

Ambiente de R
=======================================================

```{r}
library("bigmemory")
library("biganalytics")
library("bigtabulate")
library("biglm")
```

```{r}
library("ggplot2")
```


Ambiente de R
=======================================================

Usaremos procesamiento en paralelo (si está disponible)

```{r}
if( require("multicore") ) {
  library("doMC")
  registerDoMC()
} else {
  warning("Considera registrar un procesador para el foreach")
}
```

Ejercicio: RITA
=======================================================

Leemos el archivo como `big.matrix`

```
    backing.file <- "rita.bin"
    descriptor.file <- "rita.desc"
    system.time(rita <- read.big.matrix("rita.csv",
                                        type="integer",
                                        backingfile=backing.file,
                                        descriptorfile=descriptor.file,
                                        header=TRUE,
                                        extraCols=c("age")))
    
    user system elapsed
    913.029 31.497 1014.922
```

Lamentablemente así perdemos todos las variables categóricas :(

Ejercicio: RITA
=======================================================

* Lo cargamos de nuevo

```{r}
setwd("rita")
descriptor.file <- "rita.desc"
rita <- attach.big.matrix("rita.desc")
rita
dim(rita)
```

Ejercicio: RITA
=======================================================

```{r}

tabla <- bigtabulate(rita,
                     ccols = "DayOfWeek",
                     summary.cols = "ArrDelay",
                     summary.na.rm = TRUE
                     )
                    
# Poniéndola bonita
# Nombre de las estadísticas
stat.names <- dimnames(tabla$summary[[1]])[2][[1]]
# Nombre de los días
dias <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")

tabla.pretty <- cbind(matrix(unlist(tabla$summary), byrow = TRUE,
                             nrow = length(tabla$summary),
                             ncol = length(stat.names),
                             dimnames = list(dias, stat.names)),
                      ValidObs = tabla$table)
print(tabla.pretty)
```

Ejercicio: RITA
=======================================================

```{r}
plot(tabla.pretty[, "mean"], type="l", ylab="Retraso promedio por día")
```

Ejercicio: RITA
=======================================================

* Subsetting: `mwhich`

```{r}
system.time(
y <- rita[rita[,"Month"]==10 & rita[,"DayofMonth"]==1,]
)
```

```{r}
system.time(y <- rita[mwhich(rita,
                             cols=c("Month", "DayofMonth"),
                             vals=list(10, 1), comps="eq", op="AND"), ])
```

Ejercicio: RITA
=======================================================

* Caching

```{r}
system.time(y <- colrange(rita, 1, na.rm=TRUE))
y
```


```{r}
system.time(y <- colrange(rita, 1, na.rm=TRUE))
y
```




Ejercicio: RITA
=======================================================

* ¿Mejor hora para evitar retrasos?
* `foreach` y `bigmemory`

```{r}
require(foreach)
require(doParallel)
registerDoParallel(cores=4)

probs <- c(0.9, 0.99, 0.999, 0.9999)
desc <- describe(rita)
```

Ejercicio: RITA
=======================================================

```{r}

system.time(anshourofday <- foreach(i=seq(0, colmax(rita,"CRSDepTime", na.rm=TRUE)-1, by=60),
                        .combine=cbind) %dopar%
{
ind <- mwhich(rita, "CRSDepTime", c(i, i+60), comps=c('ge', 'lt'))
m <- cbind(probs, quantile(rita[ind, "DepDelay"], probs=probs, na.rm=TRUE))
colnames(m) <- c("Probabilites", "Quantiles")
t(m)[2,]
})

```

Ejercicio: RITA
=======================================================

```{r}
head(anshourofday)
dim(anshourofday)
```


Ejercicio: RITA
=======================================================

```{r}
is.data.frame(anshourofday)
is.matrix(anshourofday)
df <- as.data.frame(anshourofday) # Lo transformamos a DF
df$quantile <- row.names(df) # Ponemos nombres coherentes
```

Ejercicio: RITA
=======================================================

```{r}
library(reshape2)
melt.df <- melt(data=df, id.vars=c("quantile"))
melt.df <- transform(melt.df, variable=as.character(variable))
melt.df <- transform(melt.df, variable=str_replace_all(variable, "result.", ""))
melt.df <- transform(melt.df, variable=as.integer(variable))
```


Ejericio: RITA
=======================================================

```{r}
ggplot(data=melt.df, aes(x=variable, y=value, colour=quantile)) + geom_line()
```


Ejercicio: RITA
=======================================================

* Hacer lo siguiente (antes de crear el `bigmemory object`):
    - Recodificar el carrier code, plane tail number, y el código de aeropuerto como enteros.
    

Ejecutar un *script*
=======================================================

Esto no es tan eficiente, pero hay que saberlo...

```
    # Supóngase que mi_script.r
    # Contiene la línea print("Hola mundo")
    source("mi_script.r") # Desde R
```

Ejecutar en batch
=======================================================

* Se puede ejecutar desde la línea de comandos

  ```
      R CMD scriptfile outputfile
  ```
  - Aunque esto es muy viejo, y ya **no es recomendado**
  
* La nueva manera es

  ```
      Rscript scriptfile.r arg1 arg2 ...
  ```
  
* Y con esta línea dentro del script se pueden leer
  
  ```
      argv <- commandArgs(trailingOnly = TRUE)
      rnorm(n=as.numeric(args[1]), mean=as.numeric(args[2]))
  ```
* Para ejecutarlo sin invocar el comando `R`, usamos el __shebang__
  
  ```
      #!/usr/bin/Rscript --slave
      ## (no olvides chmod +x scriptfile)
  ```
* Con el parámetro `-e` puedes ejecutar una línea de `R`
  - Muy útil para *knittiear*.
  
- Si quieres usar loggeo a disco (**Siempre quieres tener logs**)
  usa la librería `futile.logger`


Resumiendo
=======================================================

- Sampling
  * Debe de seguir siendo **grande** respecto al *data set* completo.
  * No hay *bias*

- Más **hardware**
  * En 64-bits puedes tener **8 Tb** de objetos en `R`
  
- No uses memoria, usa disco y has las cosas de *a poquito* .
  * Como `ff` y `ffbase`

Resumiendo
=======================================================
- Integra lenguajes de alto rendimiento como `C++` o `Java`
  - **Statistical computing with C++ and R**
  - **Seamless R and C++ Integration with Rcpp**

- Intérpretes alternativos
  - [`pqR`] (http://radfordneal.github.io/pqR/)
  - [`Renjin`](http://www.renjin.org/) `R` en `JVM`
  - Oracle, Tibco tienen los suyos también

¿Cuándo debo de abandonar esto?
=======================================================

- Cientos de Megas...
  - ¿En serio? No uses MS Excel (nunca uses MS Excel), usa `R` o `Pandas`

- Hasta `10 Gb`
  - Compra más RAM, compra un SSD, considera usar `R` avanzado o `Pandas`, escoge bien el tipo de dato.

¿Cuándo debo de abandonar esto?
=======================================================

- Hasta `6 Tb`
  - Pimpea el disco, compra SSD, instala un `file system` adecuado, instala `PostgreSQL`, pimpéalo
  - Indexa tus tablas, usa las técnicas de las próximas clases.

- Hasta `+6Tb`
  - Ahora si, estamos en `Apache Hadoop` (y eso regularmente significa que todas las anteriores aplican).
  
R session
=======================================================
```{r}
print(sessionInfo(), locale = FALSE)
```
