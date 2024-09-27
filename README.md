
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DOEtestR

<!-- badges: start -->
<!-- badges: end -->

Version 2.0.1

Este paquete (doe.testR) contiene funciones para generar ejercicios y
examenes para un curso de Diseño de Experimentos. En la versión actual,
contiene las funcionalidades para desarrollar ejercicios de Media
Muestral, Prueba de Hipotesis para comparación de medias, Anova de 1
Factor y Pruebas Post-Hoc. Incluye desarrollo de un sistema de
directorio de tablas relacionales para el manejo de todos los ejercicios
y examenes desarrollados. Para la version 2.0.0 se diseñó un Command
Line Interface para facilitar el uso de la libreria - para usarla, se
llama a la variable ´doeExam()´.

## Instalación

Se puede instalar doe.testR from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("Frnkcast/DOEtestR")
```

## Explicación

### Definiciones iniciales

Para realizar mejor la labor, se debe explicar el razonamiento detrasdel
generador de preguntas. Se tienen contemplados 5 diferentes modulos,
dependiendo del tema a tratar:

- Estimaciones para medias muestrales
- Pruebas de hipotesis sobre media y varianza
- ANOVA de 1 Factor
- Pruebas Post Hoc de 1 Factor
- Diseños de 1 Factor con Bloque o 2 Factores
- Diseños 2^k

El pipeline es el siguiente:

1.  Se genera una población para el problema, y se guarda dentro de una
    base de datos, asignando una clave unica a cada población.
2.  Se genera una serie de problemas a partir de la base de dato de las
    poblaciones generadas, se asigna una clave unica según el tema y el
    tipo de problema.
3.  Se elabora una lista de enunciados de problema, se adapta a un
    formato, que puede ser de dos tipos: HTML para aplicar en CANVAS, o
    LaTeX para aplicar en papel.

Ejemplo de Codigo indicador: “1k05A11”

Decodificado, significa:

- “1”, Primer problema generado
- “k”, Problema del tipo 2^k
- “05”, id del enunciado del caso problema, de una base de datos de
  Enunciados
- “A”, tipo de Problema (Media muestal, prueba de Hipotesis, Anova de un
  factor, etc)
- “11”, id de los datos del Problema (Población), de la base de datos de
  sets de atos muestrales del tema

### Tema 1. Medias Muestrales

Los problemas que se busca generar son del tipo donde a los alumnos se
les explica que tienen una muestra de tamaño “n” proveniente de una
población normal. Se le pide que con la información provista puedan
determinar el tipo de distribución a utilizar, y el resultado. Los tipos
de problema son:

- p: Se pide la probabilidad de obtener un valor mayor/menor que un
  valor “q”
- q: Se pide obtener el valor “q” a partir de una condición de
  probabilidad dada
- m: Se pide que se calcule la media poblacional dada la información, y
  una probabilidad ya calculada.
- s: Se pide que se calcule la varianza poblacional dada la información,
  y una probabilidad ya calculada

1.  Se genera aleatoreamente los parametros de una población normal
    (media y varianza), y un valor para el tamaño de muestra
2.  Se simula la generación de una muestra de tamaño n a partir de la
    cual se obtienen los estimadores muestrales (media y varianza)
3.  Se guardan los datos de las poblaciones y muestras

### Tema 2. Pruebas de Hipotesis

Los problemas que se busca generar son de 2 tipos, dependiendo del
numero de muestras que se tienen. Si se tiene 1 muestra, dependiendo del
tamaño de muestra se deberá usar Z o T-student para los calculos de
probabilidad. Asi tambien, la región de rechazo y la Hipotesis
alternativa puede ser de tipo \>, \<, o !=

### Tema 3. Anova de 1 Factor

Los problemas que se busca generar son del tipo en el que se plantea una
situación, donde se proporcionan los datos de un experimento con A
niveles/grupos, y B replicas. Se solicita el calculo del ANOVA para
verificar si la variable experimental tiene un efecto significativo o
no. 2 Tipos de problemas:

1.  Se proporciona una tabla de ANOVA incompleta, y se debe de
    completar.
2.  Se solicita el procedimiento completo del ANOVA

## Example

This is a basic example which shows you how to solve a common problem:
