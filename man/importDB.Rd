% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database.R
\name{importDB}
\alias{importDB}
\title{Importar una tabla de datos del directorio}
\usage{
importDB(query)
}
\arguments{
\item{query}{ID de la tabla de datos, un string.}
}
\value{
Regresa una tabla de datos desde el directorio
}
\description{
V2.1 - usar los paquetes here y folders para los paths relativos
V2 - usar saveRDS y readRDS para los archivos
V1 - usar dput y dget para los archivos
}
