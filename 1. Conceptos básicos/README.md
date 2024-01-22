## Diseño del paquete

El paquete correspondiente a la actividad sobre los *Conceptos Básicos de R* llevará por nombre **dreviewr** abreviación de *Data Review for R*. El paquete tendrá como objetivo principal la revisión de data frames.

### Estructura

El código fuente del paquete se encontrará dentro de la carpeta del mismo nombre en este directorio, y seguirá la estructura recomendada en el recurso https://epirhandbook.com/es/r-projects.html. Específicamente, dado es necesario el uso de paquetes como *roxygen2* y *testthat*, se espera que el paquete siga la siguiente estructura:

- La carpeta *R* almacenará los scripts con las funciones desarrolladas.

- La carpeta *man* almacenará las documentaciones generadas por *roxygen2* de cada una de las funciones.

- La carpeta *tests* almacenará los scripts con los unit tests de las funciones desarrolladas.

- La carpeta *vignettest* almacenará los scripts con los ejemplos de uso de las funciones desarrolladas.

### Funciones clave

El paquete contará con la siguiente función principal:

- `review`, la cual recibirá un data frame y retornará un resúmen de este incluyendo el tipo de dato de cada columna, la cantidad de valores faltantes, entre otras estadísticas e indicios que sugieran suciedad en el conjunto de datos. Además, mostrará al usuario un listado explícito con las posibles incidencias encontradas en el data frame.

Además, se desarrollarán funciones auxiliares que permitan la ejecución de la función principal, tales como:

- `review_classes`, la cual recibirá un data frame y retornará el tipo de dato de cada columna.

- `review_duplicates`, la cual recibirá un data frame y retornará la cantidad de filas duplicadas en el mismo.

- `review_missing`, la cual recibirá un data frame y retornará la cantidad de valores faltantes en cada columna.

- `review_outliers`, la cual recibirá un data frame y retornará la cantidad de valores atípicos en cada columna.

- `review_stats`, la cual recibirá un data frame y retornará un resúmen estadístico de cada columna dependiendo de su tipo de dato.

## Procesamiento y análisis de datos

### Procesamiento

Dado que la función principal, además de recibir un dataframe como parámetro, también debe poder recibir una ruta a un archivo externo con un conjunto de datos, haré uso del paquete *rio*, específicamente la función *import*, la cual permite importar archivos de diferentes formatos (csv, xls, xlsx, sav, dta, entre otros) y convertirlos en data frames. De esta manera, la función principal podrá recibir tanto un data frame como un archivo externo.

### Análisis

El análisis de los datos se realizará mediante la función principal, la cual retornará un resúmen de los datos, incluyendo el tipo de dato de cada columna, la cantidad de valores faltantes, entre otras estadísticas (por ejemplo, para las variables numéricas se mostrará la media, mediana, desviación estándar, mínimo, máximo, entre otros). Además, mostrará al usuario un listado explícito con las posibles incidencias encontradas en el data frame.
