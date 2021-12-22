


# Dependencias ------------------------------------------------------------


# install.packages("tidyverse")
# install.packages("apsrtable")
# install.packages('plm')


library(plm)
library(tidyverse)
library(lubridate)

theme_set(theme_minimal())



# Lectura y preparación de los datos --------------------------------------

# Se crea un vector de fechas que inician desde el primero de enero de 1980
fecha_inicio <- as.POSIXct("1980-01-01")
secuencia_fechas <- seq.POSIXt(from = fecha_inicio,
                               length.out = 434,
                               by = "1 month")

# Se cargan los datos
brazil <- read_delim("data/Brazil_Petroleo.csv", delim = ';') %>%
  mutate(fecha = secuencia_fechas) %>%
  select(-`...1`)
canada <- read_delim("data/Canada_Petroleo.csv", delim = ';') %>%
  mutate(fecha = secuencia_fechas) %>%
  select(-`...1`)
chile <- read_delim("data/Chile_Petroleo.csv", delim = ';') %>%
  mutate(fecha = secuencia_fechas) %>%
  select(-`...1`)
czech <- read_delim("data/CzechRep_petroleo.csv", delim = ';') %>%
  mutate(fecha = secuencia_fechas) %>%
  select(-`...1`)
france <- read_delim("data/France_Petroleo.csv", delim = ';') %>%
  mutate(fecha = secuencia_fechas) %>%
  select(-`...1`)
germany <- read_delim("data/Germany_Petroleo.csv", delim = ';') %>%
  mutate(fecha = secuencia_fechas) %>%
  select(-`...1`)
ireland <- read_delim("data/Ireland_Petroleo.csv", delim = ';') %>%
  mutate(fecha = secuencia_fechas) %>%
  select(-`...1`)
israel <- read_delim("data/Israel_Petroleo.csv", delim = ';') %>%
  mutate(fecha = secuencia_fechas) %>%
  select(-`...1`)
italy <- read_delim("data/Italy_Petroleo.csv", delim = ';') %>%
  mutate(fecha = secuencia_fechas) %>%
  select(-`...1`)
japan <- read_delim("data/Japan_Petroleo.csv", delim = ';') %>%
  mutate(fecha = secuencia_fechas) %>%
  select(-`...1`)
mexico <- read_delim("data/Mexico_Petroleo.csv", delim = ';') %>%
  mutate(fecha = secuencia_fechas) %>%
  select(-`...1`)
norway <- read_delim("data/Norway_Petroleo.csv", delim = ';') %>%
  mutate(fecha = secuencia_fechas) %>%
  select(-`...1`)
spain <- read_delim("data/Spain_Petroleo.csv", delim = ';') %>%
  mutate(fecha = secuencia_fechas) %>%
  select(-`...1`)
USA <- read_delim("data/USA_Petroleo.csv", delim = ';') %>%
  mutate(fecha = secuencia_fechas) %>%
  select(-`...1`)


#############################################
#                 EJERCICIO                 #
#############################################
# Cargar los datos de polonia, rusia y reino unido.




# Visualización -----------------------------------------------------------


# Se grafican algunas variables como series de tiempo
ts.plot(as.numeric(brazil$Qoil), ylab = "Qoil")
ts.plot(as.numeric(brazil$PoilA), ylab = "Precio petróleo")
ts.plot(as.numeric(brazil$Y), ylab = "Actividad Económica")

# Se hace una gráfica de linea poniendo Qoil en función de las fechas
plot(
  as.numeric(canada$Qoil) ~ fechas,
  type = "l",
  ylab = "Qoil",
  xlab = "fecha"
)

# La gráfica anterior es equivalente a esta
canada %>%                            # Agarramos los datos de canada
  ggplot(aes(x = fecha, y = Qoil)) +  # Creamos una gráfica "mapeando" a "x" la fecha y a "y" Qoil
  geom_line()                         # Indicamos que queremos que sea una linea

#############################################
#                 EJERCICIO                 #
#############################################
# Graficar el precio del petroleo con ggplot y cambiar el título de los ejes


# colnames(prueba) <-
#   c("fechas",
#     "Qoil",
#     "Poil",
#     "Ymun",
#     "Prod. Ind.",
#     "Precios",
#     "tasa",
#     "cambiario")

# Vamos a comparar la variable Y de distintos paises

plot(
  germany$Y ~ secuencia_fechas,
  type = "l",
  xlab = "tiempo",
  ylab = "prod. ind.",
  ylim = c(39, 129)
)
points(
  italy$Y ~ secuencia_fechas,
  type = "l",
  lty = 2,
  col = "blue"
)
points(
  norway$Y ~ secuencia_fechas,
  type = "l",
  lty = 3,
  col = "red"
)
points(
  spain$Y ~ secuencia_fechas,
  type = "l",
  lty = 4,
  col = "green"
)
points(USA$Y ~ secuencia_fechas,
       type = "l",
       lty = 5,
       col = "darkgray")


# ¿Cómo se haría en ggplot?
ggplot(germany, aes(fecha, Y)) +
  geom_line() +
  geom_line(data = italy, color = 'red') +
  geom_line(data = norway, color = 'blue') +
  geom_line(data = spain, color = 'green') +
  geom_line(data = USA, color = 'darkgray')

# Usar la paleta de colores por defecto
ggplot(germany, aes(fecha, Y)) +
  geom_line(aes(color = 'Alemania'), size = .3) +
  geom_line(data = italy, aes(color = 'Italia'), size = .3) +
  geom_line(data = norway, aes(color = 'Noruega'), size = .3) +
  geom_line(data = spain, aes(color = 'España'), size = .3) +
  geom_line(data = USA, aes(color = 'USA'), size = .3)


#ts.plot(as.numeric(germany[,5]),ylab="prod.ind.")


# Regresión lineal --------------------------------------------------------

prodind <-
  c(germany$Y,
    italy$Y,
    norway$Y,
    spain$Y,
    USA$Y)

ofertapetr <-
  c(germany$Qoil,
    italy$Qoil,
    norway$Qoil,
    spain$Qoil,
    USA$Qoil)

plot(ofertapetr, prodind)

ols1 <- lm(prodind ~ ofertapetr)

summary(ols1)
tasacorta <-
  c(
    germany$I,
    italy$I,
    norway$I,
    spain$I,
    USA$I
  )

plot(tasacorta ~ ofertapetr, col = "blue")

ols2 <- lm(tasacorta ~ ofertapetr)
summary(ols2)
#alpha=40 homogeneo

#plot(ofertapetr,tasacorta)

summary(ols2)

######dummies
dum1 <- c(rep(1, 434), rep(0, 1736))
dum2 <- c(rep(0, 434), rep(1, 434), rep(0, 1302))
dum3 <- c(rep(0, 868), rep(1, 434), rep(0, 868))
dum4 <- c(rep(0, 1302), rep(1, 434), rep(0, 434))
dum5 <- c(rep(0, 1736), rep(1, 434))
  
ols3 <-
  lm(tasacorta ~ ofertapetr + dum1 + dum2 + dum3 + dum4 + dum5 - 1)
summary(ols3)
#alpha_i=40
#Evidencia de heterogeneidad.

summary(ols3)

ts.plot(ols3$residuals)
#####RESULTADOS EN LATEX
# library(apsrtable)

# apsrtable(ols3, ols2)
#####

pais <-
  c(rep(1, 434), rep(2, 434), rep(3, 434), rep(4, 434), rep(5, 434))
XX1 <- data.frame(cbind(pais, tasacorta, ofertapetr, prodind))

glm1 <-
  plm(
    XX1$tasacorta ~ XX1$ofertapetr,
    data = XX1,
    index = c("pais", "ofertapetr"),
    model = "within"
  )

summary(glm1)

fixef(glm1)
#Comparemos los coeficientes de las dummies con el ols3
glm2 <-
  plm(
    XX1$tasacorta ~ XX1$ofertapetr,
    data = XX1,
    index = c("pais", "ofertapetr"),
    model = "random"
  )

summary(glm2)

#n=5 paises, T=434 observaciones por país

phtest(glm2, glm1)


#si p-value es menor a 0.05 usar fixed effects



#shortrateprod<-data.frame(cbind(as.numeric(brazil1$I),as.numeric(canada1$I),as.numeric(mexico1$I),as.numeric(norway1$I),as.numeric(rusia1$I)))

brazI <- as.numeric(brazil1$I)
#brazI[is.na(brazI)]<-0
canaI <- as.numeric(canada1$I)
#canaI[is.na(canaI)]<-0
mexiI <- as.numeric(mexico1$I)
#mexiI[is.na(mexiI)]<-0
norwI <- as.numeric(norway1$I)
#norwI[is.na(norwI)]<-0
rusiI <- as.numeric(rusia1$I)
#rusiI[is.na(rusiI)]<-0

shortrateprod <-
  data.frame(cbind(brazI, canaI, mexiI, norwI, rusiI))


chilI <- as.numeric(chile1$I)
czecI <- as.numeric(czech1$I)
franI <- as.numeric(france1$I)
germI <- as.numeric(germany1$I)
irelI <- as.numeric(ireland1$I)
israI <- as.numeric(israel1$I)
italI <- as.numeric(italy1$I)
japaI <- as.numeric(japan1$I)
polaI <- as.numeric(poland1$I)
spaiI <- as.numeric(spain1$I)
UKI <- as.numeric(UK1$I)
USAI <- as.numeric(USA1$I)
OFERTAPET <- as.numeric(germany1$Qoil)

shortrateprod_t <-
  data.frame(
    cbind(
      brazI,
      canaI,
      mexiI,
      norwI,
      rusiI,
      chilI,
      czecI,
      franI,
      germI,
      irelI,
      israI,
      italI,
      japaI,
      polaI,
      spaiI,
      UKI,
      USAI,
      OFERTAPET
    )
  )


shortrateprod_t <- na.omit(shortrateprod_t)


productor <- c(rep(1, 186 * 5), rep(2, 186 * 12))


XX2 <-
  data.frame(cbind(
    productor,
    c(
      shortrateprod_t[, 1],
      shortrateprod_t[, 2],
      shortrateprod_t[, 3],
      shortrateprod_t[, 4],
      shortrateprod_t[, 5],
      shortrateprod_t[, 6],
      shortrateprod_t[, 7],
      shortrateprod_t[, 8],
      shortrateprod_t[, 9],
      shortrateprod_t[, 10],
      shortrateprod_t[, 11],
      shortrateprod_t[, 12],
      shortrateprod_t[, 13],
      shortrateprod_t[, 14],
      shortrateprod_t[, 15],
      shortrateprod_t[, 16],
      shortrateprod_t[, 17]
    ),
    c(
      shortrateprod_t[, 18],
      shortrateprod_t[, 18],
      shortrateprod_t[, 18],
      shortrateprod_t[, 18],
      shortrateprod_t[, 18],
      shortrateprod_t[, 18],
      shortrateprod_t[, 18],
      shortrateprod_t[, 18],
      shortrateprod_t[, 18],
      shortrateprod_t[, 18],
      shortrateprod_t[, 18],
      shortrateprod_t[, 18],
      shortrateprod_t[, 18],
      shortrateprod_t[, 18],
      shortrateprod_t[, 18],
      shortrateprod_t[, 18],
      shortrateprod_t[, 18]
    )
  ))

names(XX2) <- c("productor", "tasacorta", "ofertapetr")

ols_2_1 <- lm(XX2$tasacorta ~ XX2$ofertapetr)

summary(ols_2_1)

ols_2_2 <-
  lm(XX2$tasacorta ~ XX2$ofertapetr + factor(productor) - 1)

summary(ols_2_2)

glm1_2 <-
  plm(
    XX2$tasacorta ~ XX2$ofertapetr,
    index = c("productor"),
    data = XX2,
    model = "within"
  )
fixef(glm1_2)

pFtest(glm1_2, ols_2_1)
#Si el p-value es menor a 0.05 concluimos que el modelo con efectos fijos es mejor. En este caso concluimos
#que el modelo es efetivamente mejor.

random_2 <-
  plm(
    XX2$tasacorta ~ XX2$ofertapetr,
    index = c("productor"),
    data = XX2,
    model = "random"
  )
##problemas con la solución

