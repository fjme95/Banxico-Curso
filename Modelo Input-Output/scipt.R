# Ejecutar si no están instalados los paquetes
# install.packages("leontief")
# install.packages("tidyverse")

library(leontief)
library(tidyverse)
library(readxl)

obten_efectos <- function(X, d, exogenas) {
  # A <- input_requirement(X, d)
  B <- output_allocation(X, d)
  # Inversa de Ghosh
  G <- leontief_inverse(B)
  # # Producción bruta
  # eq <- equilibrium_output(G, d)
  
  G1 <- as_tibble(G) %>%
    mutate_all(function(x) x * exogenas) %>%
    as.matrix
  
  efecto_total <- forward_linkage(G1) %>% as.vector()
  efecto_directo <- diag(G1)
  efecto_indirecto <- efecto_total - efecto_directo
  
  return(tibble(efecto_total, efecto_directo, efecto_indirecto))
}

obten_efectos_agregados <- function(efectos, agrupador) {
  return(efectos %>%
           add_column(agrupador) %>%
           group_by(agrupador) %>%
           summarise(across(everything(), sum)))
}

# Lectura de los datos

norte <- read_excel("Datos 2013.xlsx", sheet <- "norte")
norte_util <- read_excel("Datos 2013.xlsx", sheet <- "norteutil")
norte_choq <- read_excel("Datos 2013.xlsx", sheet <- "nortechoq")

# Conversión a matrices
X = as.matrix(norte)
d = as.matrix(norte_util)

# Variables exógenas
exogenas <-summarise(norte_choq, value = `VAB matriz (mdp 2013)` * -`% teta`)

# Creación de variables de agrupamiento
sector <- c( "serv", "serv", "serv", "serv", "cont", "m31", "m31", "m31", "m31", "m32", "m32", "m32", "m32", "m33", "m33", "m33", "m33", "m33", "m33", "com", "emaer", "emaer", "emaer", "emaer", "emaer", "emaer", "emaer", "serv", "serv", "emaer", "emaer", "serv", "serv")
manuf <- c( "nman", "nman", "nman", "nman", "nman", "man", "man", "man", "man", "man", "man", "man", "man", "man", "man", "man", "man", "man", "man", "nman", "nman", "nman", "nman", "nman", "nman", "nman", "nman", "nman", "nman", "nman", "nman", "nman", "nman")

# Obtención de los efectos
efectos <- obten_efectos(X, d, exogenas)
efectos

# Agregación de los efectos por sector o manuf
obten_efectos_agregados(efectos, sector)

