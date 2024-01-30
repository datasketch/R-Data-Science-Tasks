# Construcción de interfaces de usuario en Shiny - Actividad

Para esta actividad, se pide construir una aplicación en Shiny que permita visualizar la información de casos de feminicidios ocurridos en Colombia durante el año 2017.

## Objetivo

El objetivo de esta actividad fue aprender a construir aplicaciones en Shiny y reforzar los conocimientos adquiridos en las demás actividades.

## Estructura

La actividad fue dividida en dos partes, una para la limpieza de los datos y otra para la construcción de la aplicación (femicides-data y femicides-app respectivamente).

### femicides-data

Esta carpeta contiene los archivos necesarios para la limpieza de los datos. Estos archivos son:

  - `script.R`: Archivo R con el código de la limpieza de los datos.
  - `documentación.qmd`: Archivo de Quarto con la documentación paso por paso de la limpieza de los datos. Este archivo fue compilado en archivos HTML y PDF para una mejor visualización.
  
### femicides-app

Esta carpeta contiene los archivos necesarios para la construcción de la aplicación. Estos archivos son:

  - `data`: Carpeta con los datos limpios.
  - `app.R`: Archivo R con el código de la aplicación.
  - `utils.R`: Archivo R con el código de las funciones auxiliares.

## Tareas pendientes

  - [x] Agregar modals para cada card.
  - [ ] Reescribir el `server` usando `reactiveValues` y `observe`.
  - [x] Estilizar la aplicación usando CSS (cards y markers' popups).
  - [ ] Vincular el topojson de la actividad anterior con la aplicación.