#Analisis programas electorales 


# author: Paula Pareja
# date: Julio, 2023

# Limpio la memoria de trabajo
rm(list = ls())

# Fijo el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Instalo las librerías
library(tidyverse)
library(rvest) 
library(tidytext) 
library(udpipe)
library(data.table) 
library(BTM)
library(tm)
library(quanteda)
library(topicmodels) 
library(wordcloud) 
library(pdftools) 
library(dplyr)
library(stopwords)
library(stringi)
library(stringr)
library(ggplot2)
library(scales)
library(tidyr)
library(widyr)
library(ggraph)
library(igraph)
library(cvTools)
library(syuzhet) 


#Empiezo leyendo los datos 
programa <- pdftools::pdf_text("programas/programa_VOX.pdf")
programa


#voy a estudiar el índice 

# Crear vectores para los títulos de los capítulos y temas y los valores numéricos correspondientes
indice_texto <- programa[[5]]
print(indice_texto)

numeros_capitulo <- str_extract_all(indice_texto, "(?<=\\n\\n)\\d+(?=\\.)")[[1]]

indices_temas <- str_extract_all(indice_texto, "(?<=\\. ).+?(?=\\s{3,})")[[1]]
num_pagina <- as.numeric(str_extract_all(indice_texto, "(?<=  )\\d+(?=\\n)")[[1]])
str(num_pagina)

data_frame_indices <- data.frame(capitulo = numeros_capitulo,
                                 tema = indices_temas,
                                 pagina = num_pagina)


#Parece que hay 20 capitulos, que tratan sobre igual para los españoles, Unidad de España, empleos, educación en libertad, 
#vivienda, producido en España, salud, protección social de los españoles, fiscalidad para la prosperidad, seguridad y defensa, 
#inmigración, España verde, reindustrialización, despolitización de la justifica, UE, iberosfera, España rural, 
#libertad de expresión, perspectiva de familia y dignifidad humana. 
print(data_frame_indices)



#Voy a estructurar todas las frases del texto
frases_texto <-get_sentences(programa)

#limpiamos el texto 
frases_texto <- tolower(frases_texto) # Convertir el texto a minúsculas
frases_texto <- gsub("\\d+", "", frases_texto) # Eliminar números
frases_texto <- gsub("[[:punct:]]", "", frases_texto) # Eliminar signos de puntuación


# Lo convierto a un dataframe para ver cómo queda
frases_texto <-data.frame(frases_texto)


#   1. Quitamos las páginas del índice
frases_texto<-data.frame(frases_texto[26:nrow(frases_texto),])
#   2. Quitamos el epílogo y todo lo que viene al final
frases_texto<-data.frame(frases_texto[1:1442,])
#   3. Renombramos la columna para dejar todo listo
colnames(frases_texto)[1]<-"frase"
#   4. Quitamos los espacios al comienzo de capítulos y demás
frases_texto$frase<-trimws(frases_texto$frase, "l")
#   5. Convertimos a carácter para poder hacer su análisis
frases_texto$frase<-as.character(frases_texto$frase)

summary(frases_texto)

print(frases_texto[1:50,])
print(data_frame_indices$tema)


###################################################################################################
###################################################################################################
# Empezamos el análisis
###################################################################################################
###################################################################################################
###################################################################################################
# 0. Análisis exploratorio de datos
###################################################################################################
###################################################################################################
# Nos creamos un lexicon de stopwords en español a medida 
lexiconSW<-stopwords("es")
lexiconSW<-append(lexiconSW,c(""))
lexiconSW<-data.frame(lexiconSW)
names(lexiconSW)<-"word"
lexiconSW$word<-as.character(lexiconSW$word)

# Nos creamos un dataframe para identificar cada frase
df <- tibble::rowid_to_column(frases_texto, "ID")

###########################################
###########################################
# 1.1. Fecuencia de palabras

# Limpiar y tokenizar el texto
palabras <- df %>% 
  unnest_tokens(word, frase) %>% 
  anti_join(lexiconSW)

palabras <- palabras %>%
  filter(!(word %in% c("así")))

# Obtener la frecuencia de las palabras
frecuencia_palabras <- palabras %>% 
  count(word, sort = TRUE)

# Visualizar las palabras más comunes en un gráfico de barras
ggplot(frecuencia_palabras[1:10,], aes(x = reorder(word, -n), y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Palabras más comunes en el programa de Vox",
       x = "Palabra",
       y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, max(frecuencia_palabras$n) + 10))

# Visualizar las palabras más comunes en una nube de palabras
library(wordcloud2)
wordcloud2(data = frecuencia_palabras, size=0.4, color='random-dark', shape = "circle")

# Definir el máximo número de palabras que deseas mostrar
max_palabras <- 150

# Filtrar el conjunto de datos para mostrar solo las palabras con mayor frecuencia
frecuencia_palabras_max <- head(frecuencia_palabras, max_palabras)

# Crear una nueva nube de palabras con el conjunto de datos filtrado
wordcloud2(data = frecuencia_palabras_max, size = 0.4, color = 'random-dark', shape = "circle")



###########################################
###########################################
# 1.2. Bigramas
# Podemos visualizarlo también
paresPalabras <- df %>% 
  unnest_tokens(word, frase) %>% 
  anti_join(lexiconSW)%>% 
  pairwise_count(word, ID, sort = TRUE, upper = FALSE)
set.seed(1234)
paresPalabras %>%
  filter(n >= 10) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  ggtitle('Bigrama programa electoral VOX')



###########################################
###########################################
# Topic models


cvLDA <- function(Ntopics,dtm,K=10) {
  folds<-cvFolds(nrow(dtm),K,1)
  perplex <- rep(NA,K)
  llk <- rep(NA,K)
  for(i in unique(folds$which)){
    cat(i, " ")
    which.test <- folds$subsets[folds$which==i]
    which.train <- {1:nrow(dtm)}[-which.test]
    dtm.train <- dtm[which.train,]
    dtm.test <- dtm[which.test,]
    lda.fit <- LDA(dtm.train, k=Ntopics, method="Gibbs",
                   control=list(verbose=50L, iter=100))
    perplex[i] <- perplexity(lda.fit,dtm.test)
    llk[i] <- logLik(lda.fit)
  }
  return(list(K=Ntopics,perplexity=perplex,logLik=llk))
}

numeroTopicsOptimo<-function(dtm){
  K <- c(5,10,20, 30, 40, 50, 60, 70, 80)
  results <- list()
  
  i = 1
  for (k in K){
    cat("\n\n\n##########\n ", k, "topics", "\n")
    res <- cvLDA(k, dtm)
    results[[i]] <- res
    i = i + 1
  }
  
  ## plot
  df <- data.frame(
    k = rep(K, each=10),
    perp =  unlist(lapply(results, '[[', 'perplexity')),
    loglk = unlist(lapply(results, '[[', 'logLik')),
    stringsAsFactors=F)
  
  min(df$perp)
  df$ratio_perp <- df$perp / max(df$perp)
  df$ratio_lk <- df$loglk / min(df$loglk)
  
  df <- data.frame(cbind(
    aggregate(df$ratio_perp, by=list(df$k), FUN=mean),
    aggregate(df$ratio_perp, by=list(df$k), FUN=sd)$x,
    aggregate(df$ratio_lk, by=list(df$k), FUN=mean)$x,
    aggregate(df$ratio_lk, by=list(df$k), FUN=sd)$x),
    stringsAsFactors=F)
  names(df) <- c("k", "ratio_perp", "sd_perp", "ratio_lk", "sd_lk")
  library(reshape)
  pd <- melt(df[,c("k","ratio_perp", "ratio_lk")], id.vars="k")
  pd2 <- melt(df[,c("k","sd_perp", "sd_lk")], id.vars="k")
  pd$sd <- pd2$value
  levels(pd$variable) <- c("Perplexity", "LogLikelihood")
  
  library(ggplot2)
  library(grid)
  
  p <- ggplot(pd, aes(x=k, y=value, linetype=variable))
  pq <- p + geom_line() + geom_point(aes(shape=variable),
                                     fill="white", shape=21, size=1.40) +
    geom_errorbar(aes(ymax=value+sd, ymin=value-sd), width=4) +
    scale_y_continuous("Ratio wrt worst value") +
    scale_x_continuous("Number of topics",
                       breaks=K) +
    theme_bw()
  pq
  return(pq)
}

# Función de análisis clúster
corpus <- corpus(frases_texto$frase)
cdfm <- dfm(corpus, remove=c(stopwords("spanish"), 
                             ""), 
            verbose=TRUE, remove_punct=TRUE, remove_numbers=TRUE)
cdfm <- dfm_trim(cdfm, min_docfreq = 2, verbose=TRUE)



# Ahora lo exportamos a un formato para procesar los Topic Models.
dtm <- convert(cdfm, 
               to="topicmodels")
# Seleccionando el número de topics óptimo
#vis<-numeroTopicsOptimo(dtm)
#vis


# Obtenemos el LDA con el número óptimo de topics que nos haya salido
lda <- LDA(dtm, k = 20, method = "Gibbs",
           control = list(verbose=25L, seed = 123, burnin = 100, iter = 500))

# Obtenemos la palabra más representativa de cada topic
terms(lda)
# Las top 10-palabras de cada topic
trms <- t(terms(lda, k=10))
# Algunos topics son fáciles de identificar
trms[1,]
trms[11,] 
trms[17,] 
trms[13,]
trms[16,]
trms[19,]
trms[12,]
trms[7,]
trms[8,]
trms[5,]

# Sacamos los datos de cada topic
terminosTopic <- tidy(lda, matrix = "beta")
terminosTopic

# Sacamos los top 20 términos por cada topic
top_terms <- terminosTopic %>%
  group_by(topic) %>%
  top_n(12, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Los visualizamos
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()+
  labs(x = NULL, y = "Importancia palabras en topic",
       title = paste0("Topics y sus palabras descriptivas VOX"))



#Analisis framing 
#Identificar y analizar cómo se presenta la información en un texto para influir en la percepción del lector o audiencia


# Preprocesamiento del texto
programa <- tolower(programa) # Convertir el texto a minúsculas
programa <- gsub("\\d+", "", programa) # Eliminar números
programa <- gsub("[[:punct:]]", "", programa) # Eliminar signos de puntuación
programa <- strsplit(programa, "\\s+") # Dividir el texto en palabras

# Crear un data frame con las palabras
df_programa <- data.frame(word = unlist(programa))

# Identificar categorías o temas clave
categorias <- c("tecnología", "medioambiental", "vivienda", "educación", "justicia", "igualdad", "inmigrantes", "salud", "seguridad", "impuestos")


# Filtrar palabras relacionadas con cada categoría
df_programa_framing <- df_programa %>%
  filter(word %in% categorias)

# Contar la frecuencia de palabras por categoría
frecuencia_framing <- df_programa_framing %>%
  count(word)
num_categorias_mostrar <- 10

# Visualizar las palabras más frecuentes por categoría
ggplot(frecuencia_framing[1:num_categorias_mostrar,], aes(x = reorder(word, -n), y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Framing en el Programa Político VOX",
       x = "Categoría",
       y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






