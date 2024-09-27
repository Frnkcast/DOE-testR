
#### TEMA 1 - Media Muestral -------------------

## Reporte de sets de datos
#' Media Muestral: Reporte de la tabla de datos muestrales
#'
#' @param DB tabla de datos del tema Media Muestral
#'
#' @return Genera un archivo de texto
#'
#' @examples
rep_D_MM <- function(DB){
  wd <- here::here("doetest_out", "reportes")
  outpath <- here::here(wd,paste0(attr(DB,"DBname"), ".txt"))

  sink(outpath)
  on.exit(sink())
  cat("Inicializando.... Base de datos muestrales simulados","\n")
  cat("   id: ", attr(DB, "id_bytes"), "\n")
  cat("   id: ", attr(DB, "id_pokemon"), "\n")
  cat("- Elaborado en: ", attr(DB,"Fecha"), "\n")
  cat("- Para el tema: ", attr(ans,"Tema"),"\n\n")

  for(i in 1:length(DB$mu)){
    cat("-----------------------------------------------------------\n")
    cat(" Entrada #: ", "M",as.character(DB[[i,1]]), "\n")
    cat("-----------------------------------------------------------\n")
    cat("Población::: \n")
    cat("   Mu = ", as.character(DB[[i,"mu"]]), "\n")
    cat("   Sig = ", as.character(DB[[i,"sig"]]), "\n \n")
    cat("Muestra:: \n")
    cat("   n:  ", as.character(DB[[i,"n"]]), "\n")
    cat("   Datos:  ", as.character(DB[[i,"data"]]), "\n")
    cat("   Media muestal = ", as.character(DB[[i, "sam_mu"]]), "\n")
    cat("   Desv. Muestral(S) = ", as.character(DB[[i, "sam_sig"]]), "\n")
    cat("\n \n")
    cat("Probabilidades:: \n")
    cat("   Distribución: ",as.character(DB[[i, "distr"]]) ,"\n")
    cat("   Valor de query: ",as.character(DB[[i, "q"]]) ,"\n")
    cat("   Valor de query(Critico): ",as.character(DB[[i, "q_est"]]) ,"\n")
    cat("   P(X > Query): ",as.character(DB[[i, "p_right"]]) ,"\n")
    cat("   P(X < Query): ",as.character(DB[[i, "p_left"]]) ,"\n")
    cat("\n \n")
    cat("-----------------------------------------------------------\n")
    cat("\n")
  }
  sink()
  file.show(outpath)
}

## Reporte de Pregunta
#' Media Muestral: Reporte de preguntas generadas
#'
#' @param Data Tabla de datos muestrales
#' @param Ques Tabla de ejercicios generados a parir de la tabla de datos muestrales
#' @param i Id de la pregunta especifica a describir
#'
#' @return
#'
#' @examples
rep_Q_MM <- function(Data,Ques,i){
  cat("-----------------------------------------------------------\n")
  cat(" Entrada #: ",as.character(Ques[[i,"Code"]]), "\n")
  cat("-----------------------------------------------------------\n")
  cat("Población::: \n")
  cat("   Mu = ",
      as.character(Data$mu[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("   Sig = ",
      as.character(Data$sig[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("\n")
  cat("Muestra:: \n")
  cat("   n:  ", as.character(Data$n[[match(Ques$dbID[i], Data$id)]]), "\n")
  #cat("   Datos:  ", as.character(Data$data[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("   Media muestal = ", as.character(Data$sam_mu[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("   Desv. Muestral(S) = ", as.character(Data$sam_sig[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("\n")
  cat("Probabilidades:: \n")
  cat("   Distribución: ",as.character(Ques[[i, "distr"]]),"\n")
  cat("   Valor de query: ",as.character(Ques[[i,"q"]]) ,"\n")
  cat("   Valor de query(Critico): ",
      as.character(Data$q_est[[match(Ques$dbID[i], Data$id)]]) ,"\n")
  cat("   P(X ", as.character(Ques[[i,"cola"]]),
      as.character(Ques[[i,"q"]])," ) = ",
      as.character(Ques[[i, "p"]]) ,"\n")
  cat("\n")
  cat("Enunciado:: \n")
  cat("   ",Ques[[i, "z"]], "....... \n")
  cat("   Buscando: ", as.character(Ques[[i,"Ques"]]),"\n")
  cat("   Respuesta: ", as.character(Ques[[i, "Ans"]]), "\n")
  cat("   Rango aceptable: ", as.character(Ques[[i, "Min"]]), " - ", as.character(Ques[[i, "Max"]]), "\n")
  cat("\n \n")
  cat("-----------------------------------------------------------\n")
  cat("\n")
}


### TEMA 2 - Prueba de Hipotesis -------------------

## Reporte de datos muestrales
#' Prueba de Hipotesis: Reporte de la tabla de datos muestrales
#'
#' @param DB tabla de datos del tema Prueba de Hipotesis
#'
#' @return Genera un archivo de texto
#'
#' @examples
rep_D_PH <- function(DB){
  wd <- here::here("doetest_out", "reportes")
  outpath <- here::here(wd,paste0(attr(DB,"DBname"), ".txt"))

  sink(outpath)
  on.exit(sink())
  cat("Inicializando.... Base de datos muestrales simulados","\n")
  cat("   id: ", attr(DB, "id_bytes"), "\n")
  cat("   id: ", attr(DB, "id_pokemon"), "\n")
  cat("- Elaborado en: ", attr(DB,"Fecha"), "\n")
  cat("- Para el tema: ", attr(DB,"Tema"),"\n\n")
  for(i in 1:length(DB$id)){
    cat("-----------------------------------------------------------\n")
    cat(" Entrada #: ", "H",as.character(DB[[i,1]]), "\n")
    cat("-----------------------------------------------------------\n")
    cat("Población 1::: \n")
    cat("   Mu = ", as.character(DB[[i,"mu_1"]]), "\n")
    cat("   Sig = ", as.character(DB[[i,"sig_1"]]), "\n")
    cat("   Muestra ::: \n")
    cat("      n:  ", as.character(DB[[i,"n_1"]]), "\n")
    cat("      Datos:  ", as.character(DB[[i,"data_1"]]), "\n")
    cat("      Media muestal = ", as.character(DB[[i, "s_mu_1"]]), "\n")
    cat("      Desv. Muestral(S) = ", as.character(DB[[i, "s_sig_1"]]), "\n")
    cat("\n")
    cat("Población 2::: \n")
    cat("   Mu = ", as.character(DB[[i,"mu_2"]]), "\n")
    cat("   Sig = ", as.character(DB[[i,"sig_2"]]), "\n")
    cat("   Muestra ::: \n")
    cat("      n:  ", as.character(DB[[i,"n_2"]]), "\n")
    cat("      Datos:  ", as.character(DB[[i,"data_2"]]), "\n")
    cat("      Media muestal = ", as.character(DB[[i, "s_mu_2"]]), "\n")
    cat("      Desv. Muestral(S) = ", as.character(DB[[i, "s_sig_2"]]), "\n")
    cat("\n")
    cat("Probabilidades:: \n")
    cat("   Distribución: ",as.character(DB[[i, "distr"]]) ,"\n")
    cat("   Grados de libertad (t): ",as.character(DB[[i,"n_1"]]+DB[[i,"n_2"]]-2) ,"\n")

    cat("\n")
    cat("-----------------------------------------------------------\n")
    cat("\n")
  }
  sink()
  file.show(outpath)
}

## Reporte de preguntas
#' Prueba de Hipotesis: Reporte de preguntas generadas
#'
#' @param Data Tabla de datos muestrales
#' @param Ques Tabla de ejercicios generados a parir de la tabla de datos muestrales
#' @param i Id de la pregunta especifica a describir
#'
#' @return
#'
#' @examples
rep_Q_PH <- function(Data,Ques,i){
  cat("-----------------------------------------------------------\n")
  cat(" Entrada #: ",as.character(Ques[[i,"Code"]]), "\n")
  cat("-----------------------------------------------------------\n")
  cat("Población 1::: \n")
  cat("   Mu = ", as.character(Data$mu_1[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("   Sig = ", as.character(Data$sig_1[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("   Muestra ::: \n")
  cat("      n:  ", as.character(Data$n_1[[match(Ques$dbID[i], Data$id)]]), "\n")
  #cat("      Datos:  ", as.character(Data$data_1[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("      Media muestal = ", as.character(Data$s_mu_1[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("      Desv. Muestral(S) = ", as.character(Data$s_sig_1[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("\n")
  cat("Población 2::: \n")
  cat("   Mu = ", as.character(Data$mu_2[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("   Sig = ", as.character(Data$sig_2[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("   Muestra ::: \n")
  cat("      n:  ", as.character(Data$n_2[[match(Ques$dbID[i], Data$id)]]), "\n")
  #cat("      Datos:  ", as.character(Data$data_2[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("      Media muestal = ", as.character(Data$s_mu_2[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("      Desv. Muestral(S) = ", as.character(Data$s_sig_2[[match(Ques$dbID[i], Data$id)]]), "\n")
  cat("\n")
  cat("Probabilidades:: \n")
  cat("   Enunciado:: \n", as.character(Ques[[i, "z"]]), "\n")
  cat("   Distribución: ",as.character(Ques[[i, "distr"]]) ,"\n")
  cat("   Grados de libertad (t): ",as.character(Ques[[i, "deg"]]) ,"\n")
  cat("   Hipotesis nula: $\\mu_1 - \\mu_2 = ", as.character(Ques[[i, "q"]]) ,"$ \n")
  cat("   Hipotesis alternativa: $\\mu_1 - \\mu_2 ", as.character(Ques[[i,"cola"]])," ",as.character(Ques[[i, "q"]]) ,"$ \n")

  if(Ques[[i,"cola"]] == "ne"){ ## Cuando es de dos colas
    cat("   Prueba de dos colas.","\n")
    cat("   Alfa: ",as.character(Ques[[i, "alfa"]]) ,"\n")
    cat("   Alfa/2: ",as.character(Ques[[i, "alfa"]]/2) ,"\n")
    cat("   Estadistico de Prueba: ",as.character(Ques[[i, "q_est"]]) ,"\n")
    cat("   Valor Critico: ",as.character(Ques[[i, "crit"]]) ,"\n")
    cat("   P(X > |EP|) = p_value = " ,as.character(Ques[[i, "pval"]]) ,"\n")
    cat("   Conclusion: ",as.character(Ques[[i, "comp"]]) ," H0. \n")

  }else { ## Cuando NO es de dos colas.
    cat("   Prueba de una cola.","\n")
    cat("   Alfa: ",as.character(Ques[[i, "alfa"]]) ,"\n")
    cat("   Estadistico de Prueba: ",as.character(Ques[[i, "q_est"]]) ,"\n")
    cat("   Valor Critico: ",as.character(Ques[[i, "crit"]]) ,"\n")
    cat("   P(X",as.character(Ques[[i,"cola"]])," EP) = p_value = " ,as.character(Ques[[i, "pval"]]) ,"\n")
    cat("   Conclusion: ",as.character(Ques[[i, "comp"]]) ," H0. \n")
  }

  cat("\n")
  cat("-----------------------------------------------------------\n")
  cat("\n")
}


#### TEMA 3 - Anova de 1 Factor -------------------

## Reporte de datos muestrales
#' Anova de 1 Factor: Reporte de la tabla de datos muestrales
#'
#' @param DB tabla de datos del tema Anova de 1 Factor
#'
#' @return Genera un archivo de texto
#'
#' @examples
rep_D_A1 <- function(DB){
  wd <- here::here("doetest_out", "reportes")
  outpath <- here::here(wd,paste0(attr(DB,"DBname"), ".txt"))

  sink(outpath)
  on.exit(sink())
  cat("Inicializando.... Base de datos muestrales simulados","\n")
  cat("   id: ", attr(DB, "id_bytes"), "\n")
  cat("   id: ", attr(DB, "id_pokemon"), "\n")
  cat("- Elaborado en: ", attr(DB,"Fecha"), "\n")
  cat("- Para el tema: ", attr(DB,"Tema"),"\n\n")

  for(i in 1:length(DB$id)){
    cat("-----------------------------------------------------------\n")
    cat(" Entrada #: ", as.character(DB[[i,"id"]]), "\n")
    cat("-----------------------------------------------------------\n")
    cat("   # Niveles de A: ", as.character(DB[[i,"a"]]), "\n")
    cat("   Replicas: ", as.character(DB[[i,"n"]]), "\n \n")
    cat("Datos crudos: \n")
    cat(knitr::kable(DB[[i,"data"]], "pipe"))
    cat("\n \n")
    cat("Tabla de ANOVA: \n")
    cat(knitr::kable(DB[[i,"anova"]], "pipe"))
    cat("\n")
    cat("\n")
    cat("   F estadistica: ", DB[[i,"Fes"]], "\n")
    cat("   F critica: ", DB[[i,"Fcr"]], "\n")
    cat("\n")
    cat("   Comparación F0 vs Fest: ", DB[[i,"signif"]])
    cat("\n")
    cat("   Conclusión sobre H0: ", DB[[i,"H0"]])
    cat("\n \n")
    cat("Pruebas Post Hoc: \n")

    rep_PostHoc(DB[i,])

    cat("-----------------------------------------------------------\n")
    cat("\n")
  }
  sink()
  file.show(outpath)
}

## Elemento especifico para prueba de hipotesis
#' Anova de 1 Factor: Reporte de resultados de Pruebas Post Hoc
#'
#' @param Data Fila de la tabla de datos a describir
#'
#' @return
#'
#' @examples
rep_PostHoc <- function(Data){
  #Data es la fila de la base de datos
  #x1 es solo la tabla de datos muestrales
  x1 <- Data$data[[1]] #La tabla.

  ## Calcula los promedios por nivel de la variable
  x1m <- x1 %>% tidyr::gather(key = "X", value = "Y")%>%
    dplyr::group_by(X) %>% dplyr::summarise(Y=mean(Y))

  ## Prepara la matriz de comparación por parejas
  names(x1m$Y) <- x1m$X
  x1pw <- abs(outer(x1m$Y, x1m$Y, FUN = "-"))
  x1pw <- upper.tri(x1pw)*x1pw
  x1pw[x1pw == 0] <- NA
  #x3pw

  ## Guarda los valores de LSD y HSD
  x1_LSD <- Data$lsd[[1]][[6]]
  x1_HSD <- Data$tukey[[1]][[6]]

  x1_PostHoc <- list(outer(x1pw, x1_LSD, FUN = ">")[,,1],
                     outer(x1pw, x1_LSD, FUN = ">")[,,1]) #[[1]] es LSD, [[2]] es HSD

  #return(x1_PostHoc)

  cat("   Fisher: \n")
  cat(knitr::kable(list(Data[[1,"lsd"]]), "pipe"))
  cat("\n\n")
  cat("Semejanza entre grupos.
  (Grupos que no comparten una letra son DIFERENTES.) \n")
  cat(knitr::kable(list(Data[[1,"lsd_groups"]]), "pipe"))
  cat("\n\n")
  cat("Matriz de diferencia entre parejas.
  (¿Es la diferencia mayor que el valor de LSD? TRUE = Si, FALSE = No) \n")
  cat(knitr::kable(x1_PostHoc[1],"pipe"))
  cat("\n\n")
  cat("\n")
  cat("   Tukey: \n")
  cat(knitr::kable(list(Data[[1,"tukey"]]), "pipe"))
  cat("\n\n")
  cat("Semejanza entre grupos.
  (Grupos que no comparten una letra son DIFERENTES.) \n")
  cat(knitr::kable(list(Data[[1,"tukey_groups"]]), "pipe"))
  cat("\n\n")
  cat("Matriz de diferencia entre parejas.
  (¿Es la diferencia mayor que el valor de HSD? TRUE = Si, FALSE = No) \n")
  cat(knitr::kable(x1_PostHoc[2],"pipe"))
  cat("\n\n")
}

## Reporte de preguntas elaboradas
#' Anova de 1 Factor: Reporte de preguntas generadas
#'
#' @param Data Tabla de datos muestrales
#' @param Ques Tabla de ejercicios generados a parir de la tabla de datos muestrales
#' @param i Id de la pregunta especifica a describir
#'
#' @return
#'
#' @examples
rep_Q_A1 <- function(Data,Ques,i){
  cat("-----------------------------------------------------------\n")
  cat(" Entrada #: ", as.character(Ques[[i,"Code"]]), "\n")
  cat("-----------------------------------------------------------\n")
  cat("   # Niveles de A: ", as.character(Data$a[[match(Ques$id[i], Data$id)]]), "\n")
  cat("   Replicas: ", as.character(Data$n[[match(Ques$id[i], Data$id)]]), "\n \n")
  cat("Datos crudos: \n")
  cat(knitr::kable(Ques[[i,"table"]], "pipe"))
  cat("\n \n")
  cat("Tabla de ANOVA: \n")
  cat(knitr::kable(Ques[[i,"table2"]], "pipe"), "\n")
  cat("\n")
  cat("   F estadistica: ", Data$Fes[[match(Ques$id[i], Data$id)]], "\n")
  cat("   F critica: ", Data$Fcr[[match(Ques$id[i], Data$id)]], "\n")
  cat("\n")
  cat("   Comparación F0 vs Fest: ", Data$signif[[match(Ques$id[i], Data$id)]])
  cat("\n")
  cat("   Conclusión sobre H0: ", Data$H0[[match(Ques$id[i], Data$id)]])
  cat("\n \n")
  cat("Pruebas Post Hoc:: \n")

  rep_PostHoc(Data[match(Ques$id[i], Data$id),])
  #cat("   Fisher: \n")
  #cat("\n")
  #cat(knitr::kable(list(Data$lsd[[match(Ques$id[i], Data$id)]]), "pipe"))
  #cat("\n")
  #cat("\n")
  #cat(knitr::kable(list(Data$lsd_groups[[match(Ques$id[i], Data$id)]]), "pipe"))
  #cat("\n")
  #cat("\n")
  #cat("   Tukey: \n")
  #cat("\n")
  #cat(knitr::kable(list(Data$tukey[[match(Ques$id[i],Data$id)]]), "pipe"))
  #cat("\n")
  #cat("\n")
  #cat(knitr::kable(list(Data$tukey_groups[[match(Ques$id[i],Data$id)]]), "pipe"))
  #cat("\n")
  cat("\n")
  cat("-----------------------------------------------------------\n")
  cat("\n")
}
