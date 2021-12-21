#######       EJERCICIO I      #######


# Establecemos la ruta de trabajo
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())
library(matlib)

#Importamos los documentos necesarios#

library(readxl)

norte <- read_excel("Datos 2013.xlsx",
                    sheet = "norte")

norteutil <- read_excel("Datos 2013.xlsx",
                        sheet = "norteutil")

nortechoq <- read_excel("Datos 2013.xlsx",
                        sheet = "nortechoq")


centronorte <- read_excel("Datos 2013.xlsx",
                          sheet = "centronorte")

centronorteutil <- read_excel("Datos 2013.xlsx",
                              sheet = "cnorteutil")

centronortechoq <- read_excel("Datos 2013.xlsx",
                              sheet = "cnortechoq")

centro <- read_excel("Datos 2013.xlsx",
                     sheet = "centro")

centroutil <- read_excel("Datos 2013.xlsx",
                         sheet = "centroutil")

centrochoq <- read_excel("Datos 2013.xlsx",
                         sheet = "centrochoq")

sur <- read_excel("Datos 2013.xlsx",
                  sheet = "sur")

surutil <- read_excel("Datos 2013.xlsx",
                      sheet = "surutil")

surchoq <- read_excel("Datos 2013.xlsx",
                      sheet = "surchoq")
library(dplyr)
library(matlib)
#lista de sectores#
sector <-
  c(
    "serv",
    "serv",
    "serv",
    "serv",
    "cont",
    "m31",
    "m31",
    "m31",
    "m31",
    "m32",
    "m32",
    "m32",
    "m32",
    "m33",
    "m33",
    "m33",
    "m33",
    "m33",
    "m33",
    "com",
    "emaer",
    "emaer",
    "emaer",
    "emaer",
    "emaer",
    "emaer",
    "emaer",
    "serv",
    "serv",
    "emaer",
    "emaer",
    "serv",
    "serv"
  )
manuf <-
  c(
    "nman",
    "nman",
    "nman",
    "nman",
    "nman",
    "man",
    "man",
    "man",
    "man",
    "man",
    "man",
    "man",
    "man",
    "man",
    "man",
    "man",
    "man",
    "man",
    "man",
    "nman",
    "nman",
    "nman",
    "nman",
    "nman",
    "nman",
    "nman",
    "nman",
    "nman",
    "nman",
    "nman",
    "nman",
    "nman",
    "nman"
  )


###########       NORTE      ##########

#Indicamos a R que trabajaremos en formato de matriz#
minc1 <- as.matrix(norte)
vinc1 <- as.matrix(norteutil)
cinc1 <- as.matrix(nortechoq)

#Creamos un loop para automatizar la division de la MIP entre la utilizacion#
m1 <- matrix(0, 33, 33)
for (t in 1:33) {
  m1[, t] <- minc1[, t] / vinc1[, 1]
}

#Creamos una matriz identidad con las dimensiones necesarias#
I <- diag(33)

#Hacemos los calculos necesarios y obtenemos la matriz de Ghosh#
A1 <- I - m1
G1 <- inv(A1)

#Obtenemos el vector de variables exogenas, mediante la informacion sobre los choques#
ve1 <- as.matrix(nortechoq[, 1] * -nortechoq[, 2])

#Multiplicamos el vector de choques/var.exog. por cada columna de la matriz de Ghosh#
mfin1 <- matrix(0, 33, 33)
for (t in 1:33) {
  mfin1[, t] <- G1[, t] * ve1[, 1]
}

#Obtenemos los efectos#
etotal1 <- rowSums(mfin1)
edirecto1 <- diag(mfin1)
eindirecto1 <- etotal1 - edirecto1

#Regresamos las matrices a data frame para un mejor manejo y presentacion de la informacion#
efectos1 <- data.frame(sector, manuf, etotal1, edirecto1, eindirecto1)

#Calculamos los efectos por sectores#
servicios1 <- filter(efectos1, sector == "serv")
efservicios1 <- colSums(servicios1[, 3:5])

construccion1 <- filter(efectos1, sector == "cont")
efconstruccion1 <- colSums(construccion1[, 3:5])

manufact31n <- filter(efectos1, sector == "m31")
efmanufact31n <- colSums(manufact31n[3:5])

manufact32n <- filter(efectos1, sector == "m32")
efmanufact32n <- colSums(manufact32n[3:5])

manufact33n <- filter(efectos1, sector == "m33")
efmanufact33n <- colSums(manufact33n[3:5])

comercio1 <- filter(efectos1, sector == "com")
efcomercio1 <- colSums(comercio1[, 3:5])

semaer1 <- filter(efectos1, sector == "emaer")
efsemaer1 <- colSums(semaer1[3:5])


#Anexamos la informacion en una sola tabla#
tablanorte <-
  data.frame(
    efservicios1,
    efconstruccion1,
    efmanufact31n,
    efmanufact32n,
    efmanufact33n,
    efcomercio1,
    efsemaer1
  )

#Por ultimo, para 2 grandes subsectores manufacturero y no manufactureros#
manufactura1 <- filter(efectos1, manuf == "man")
efmanufactura1 <- colSums(manufactura1[, 3:5])
nomanufactura1 <- filter(efectos1, manuf == "nman")
efnomanufactura1 <- colSums(nomanufactura1[, 3:5])
manufnorte <- data.frame(efmanufactura1, efnomanufactura1)

#Si se desea se puede cambiar el ordenamiento de las tablas#
sect1n <- as.matrix(tablanorte)
sect2n <- t(sect1n)
sectfinalnorte <- data.frame(sect2n)
manuf1n <- as.matrix(manufnorte)
manuf2n <- t(manuf1n)
manufinalnorte <- data.frame(manuf2n)

###########       CENTRO NORTE      ##########

#Indicamos a R que trabajaremos en formato de matriz#
minc2 <- as.matrix(centronorte)
vinc2 <- as.matrix(centronorteutil)
cinc2 <- as.matrix(centronortechoq)

#Creamos un loop para automatizar la division de la MIP entre la utilizacion#
m2 <- matrix(0, 33, 33)
for (t in 1:33) {
  m2[, t] <- minc2[, t] / vinc2[, 1]
}

#Hacemos los calculos necesarios y obtenemos la matriz de Ghosh#
A2 <- I - m2
G2 <- inv(A2)

#Obtenemos el vector de variables exogenas, mediante la informacion sobre los choques#
ve2 <- as.matrix(centronortechoq[, 1] * -centronortechoq[, 2])

#Multiplicamos el vector de choques/var.exog. por cada columna de la matriz de Ghosh#
mfin2 <- matrix(0, 33, 33)
for (t in 1:33) {
  mfin2[, t] <- G2[, t] * ve2[, 1]
}

#Obtenemos los efectos#
etotal2 <- rowSums(mfin2)
edirecto2 <- diag(mfin2)
eindirecto2 <- etotal2 - edirecto2

#Regresamos las matrices a data frame para un mejor manejo y presentacion de la informacion#
efectos2 <- data.frame(sector, manuf, etotal2, edirecto2, eindirecto2)

#Calculamos los efectos por sectores#
library(dplyr)
servicios2 <- filter(efectos2, sector == "serv")
efservicios2 <- colSums(servicios2[, 3:5])
construccion2 <- filter(efectos2, sector == "cont")
efconstruccion2 <- colSums(construccion2[, 3:5])
manufact31cn <- filter(efectos2, sector == "m31")
efmanufact31cn <- colSums(manufact31cn[3:5])
manufact32cn <- filter(efectos2, sector == "m32")
efmanufact32cn <- colSums(manufact32cn[3:5])
manufact33cn <- filter(efectos2, sector == "m33")
efmanufact33cn <- colSums(manufact33cn[3:5])
comercio2 <- filter(efectos2, sector == "com")
efcomercio2 <- colSums(comercio2[, 3:5])
semaer2 <- filter(efectos2, sector == "emaer")
efsemaer2 <- colSums(semaer2[3:5])


#Anexamos la informacion en una sola tabla#
tablacentronorte <-
  data.frame(
    efservicios2,
    efconstruccion2,
    efmanufact31cn,
    efmanufact32cn,
    efmanufact33cn,
    efcomercio2,
    efsemaer2
  )

#Por ultimo, para 2 grandes subsectores manufacturero y no manufactureros#
manufactura2 <- filter(efectos2, manuf == "man")
efmanufactura2 <- colSums(manufactura2[, 3:5])
nomanufactura2 <- filter(efectos2, manuf == "nman")
efnomanufactura2 <- colSums(nomanufactura2[, 3:5])
manufcentronorte <- data.frame(efmanufactura2, efnomanufactura2)

#Si se desea se puede cambiar el ordenamiento de las tablas#
sect1cn <- as.matrix(tablacentronorte)
sect2cn <- t(sect1n)
sectfinalcentronorte <- data.frame(sect2cn)
manuf1cn <- as.matrix(manufcentronorte)
manuf2cn <- t(manuf1cn)
manufinalcentronorte <- data.frame(manuf2cn)


###########       CENTRO       ##########

#Indicamos a R que trabajaremos en formato de matriz#
minc3 <- as.matrix(centro)
vinc3 <- as.matrix(centroutil)
cinc3 <- as.matrix(centrochoq)

#Creamos un loop para automatizar la division de la MIP entre la utilizacion#
m3 <- matrix(0, 33, 33)
for (t in 1:33) {
  m3[, t] <- minc3[, t] / vinc3[, 1]
}

#Hacemos los calculos necesarios y obtenemos la matriz de Ghosh#
A3 <- I - m3
G3 <- inv(A3)

#Obtenemos el vector de variables exogenas, mediante la informacion sobre los choques#
ve3 <- as.matrix(centrochoq[, 1] * -centrochoq[, 2])

#Multiplicamos el vector de choques/var.exog. por cada columna de la matriz de Ghosh#
mfin3 <- matrix(0, 33, 33)
for (t in 1:33) {
  mfin3[, t] <- G3[, t] * ve3[, 1]
}

#Obtenemos los efectos#
etotal3 <- rowSums(mfin3)
edirecto3 <- diag(mfin3)
eindirecto3 <- etotal3 - edirecto3

#Regresamos las matrices a data frame para un mejor manejo y presentacion de la informacion#
efectos3 <- data.frame(sector, manuf, etotal3, edirecto3, eindirecto3)

#Calculamos los efectos por sectores#
library(dplyr)
servicios3 <- filter(efectos3, sector == "serv")
efservicios3 <- colSums(servicios3[, 3:5])
construccion3 <- filter(efectos3, sector == "cont")
efconstruccion3 <- colSums(construccion3[, 3:5])
manufact31c <- filter(efectos3, sector == "m31")
efmanufact31c <- colSums(manufact31c[3:5])
manufact32c <- filter(efectos3, sector == "m32")
efmanufact32c <- colSums(manufact32c[3:5])
manufact33c <- filter(efectos3, sector == "m33")
efmanufact33c <- colSums(manufact33c[3:5])
comercio3 <- filter(efectos3, sector == "com")
efcomercio3 <- colSums(comercio3[, 3:5])
semaer3 <- filter(efectos3, sector == "emaer")
efsemaer3 <- colSums(semaer3[3:5])


#Anexamos la informacion en una sola tabla#
tablacentro <-
  data.frame(
    efservicios3,
    efconstruccion3,
    efmanufact31c,
    efmanufact32c,
    efmanufact33c,
    efcomercio3,
    efsemaer3
  )

#Por ultimo, para 2 grandes subsectores manufacturero y no manufactureros#
manufactura3 <- filter(efectos3, manuf == "man")
efmanufactura3 <- colSums(manufactura3[, 3:5])
nomanufactura3 <- filter(efectos3, manuf == "nman")
efnomanufactura3 <- colSums(nomanufactura3[, 3:5])
manufcentro <- data.frame(efmanufactura3, efnomanufactura3)

#Si se desea se puede cambiar el ordenamiento de las tablas#
sect1c <- as.matrix(tablacentronorte)
sect2c <- t(sect1n)
sectfinalcentro <- data.frame(sect2c)
manuf1c <- as.matrix(manufcentro)
manuf2c <- t(manuf1c)
manufinalcentro <- data.frame(manuf2c)


###########       SUR       ##########

#Indicamos a R que trabajaremos en formato de matriz#
minc4 <- as.matrix(sur)
vinc4 <- as.matrix(surutil)
cinc4 <- as.matrix(surchoq)

#Creamos un loop para automatizar la division de la MIP entre la utilizacion#
m4 <- matrix(0, 33, 33)
for (t in 1:33) {
  m4[, t] <- minc4[, t] / vinc4[, 1]
}

m4[!is.finite(m4)] <- 0

#Hacemos los calculos necesarios y obtenemos la matriz de Ghosh#
A4 <- I - m4
G4 <- inv(A4)

#Obtenemos el vector de variables exogenas, mediante la informacion sobre los choques#
ve4 <- as.matrix(surchoq[, 1] * -surchoq[, 2])

#Multiplicamos el vector de choques/var.exog. por cada columna de la matriz de Ghosh#
mfin4 <- matrix(0, 33, 33)
for (t in 1:33) {
  mfin4[, t] <- G4[, t] * ve4[, 1]
}

#Obtenemos los efectos#
etotal4 <- rowSums(mfin4)
edirecto4 <- diag(mfin4)
eindirecto4 <- etotal4 - edirecto4

#Regresamos las matrices a data frame para un mejor manejo y presentacion de la informacion#
efectos4 <- data.frame(sector, manuf, etotal4, edirecto4, eindirecto4)

#Calculamos los efectos por sectores#
library(dplyr)
servicios4 <- filter(efectos4, sector == "serv")
efservicios4 <- colSums(servicios4[, 3:5])
construccion4 <- filter(efectos4, sector == "cont")
efconstruccion4 <- colSums(construccion4[, 3:5])
manufact31s <- filter(efectos4, sector == "m31")
efmanufact31s <- colSums(manufact31s[3:5])
manufact32s <- filter(efectos4, sector == "m32")
efmanufact32s <- colSums(manufact32s[3:5])
manufact33s <- filter(efectos4, sector == "m33")
efmanufact33s <- colSums(manufact33s[3:5])
comercio4 <- filter(efectos4, sector == "com")
efcomercio4 <- colSums(comercio4[, 3:5])
semaer4 <- filter(efectos4, sector == "emaer")
efsemaer4 <- colSums(semaer4[3:5])


#Anexamos la informacion en una sola tabla#
tablasur <-
  data.frame(
    efservicios4,
    efconstruccion4,
    efmanufact31s,
    efmanufact32s,
    efmanufact33s,
    efcomercio4,
    efsemaer4
  )

#Por ultimo, para 2 grandes subsectores manufacturero y no manufactureros#
manufactura4 <- filter(efectos4, manuf == "man")
efmanufactura4 <- colSums(manufactura4[, 3:5])
nomanufactura4 <- filter(efectos4, manuf == "nman")
efnomanufactura4 <- colSums(nomanufactura4[, 3:5])
manufsur <- data.frame(efmanufactura4, efnomanufactura4)

#Si se desea se puede cambiar el ordenamiento de las tablas#
sect1s <- as.matrix(tablasur)
sect2s <- t(sect1s)
sectfinalsur <- data.frame(sect2s)
manuf1s <- as.matrix(manufsur)
manuf2s <- t(manuf1s)
manufinalsur <- data.frame(manuf2s)



#########        TABLAS FINALES        ########

# EFECTOS POR SECTORES #
print(sectfinalnorte)
print(sectfinalcentronorte)
print(sectfinalcentro)
print(sectfinalsur)
# EFECTOS POR SECTORES MANUFACTUREROS Y NO MANUFACTUREROS
print(manufinalnorte)
print(manufinalcentronorte)
print(manufinalcentro)
print(manufinalsur)
