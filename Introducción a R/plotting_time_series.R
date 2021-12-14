# https://r4ds.had.co.nz/dates-and-times.html

install.packages("tidyverse")
install.packages("nycflights13")

library(tidyverse)
library(lubridate)
library(nycflights13)

?flights

flights <- flights %>%
  filter(!is.na(arr_time), !is.na(dep_time)) %>% 
  filter(year == 2013, month == 5)



flights %>% 
  filter(!is.na(dep_time), day %in% 1:3) %>% 
  mutate(dt = make_datetime(year, month, day, hour, minute)) %>% 
  ggplot(aes(dt, dep_delay)) +
  geom_line() +
  scale_x_datetime(date_breaks = "4 hours") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 25, 
                                   vjust = 1.0, hjust = 1.0))

# Retraso total por día

flights %>% 
  mutate(dt = make_datetime(year, month, day, hour, minute)) %>% 
  group_by(day = floor_date(dt, "day")) %>% 
  summarise(total_delays = sum(dep_delay, na.rm = TRUE) + sum(arr_delay, na.rm = TRUE)) %>% 
  ggplot(aes(day, total_delays)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  theme_minimal() +
  labs(
    title = 'Retrasos por día', 
    x = 'Fecha', 
    y = "Retraso total en minutos"
  ) +
  scale_x_datetime(date_breaks = '1 day') + 
  theme(
    axis.text.x = element_text(angle= 90), 
    panel.grid.minor.x = element_blank()
  )


#' EJERCICIO
#' 
#' Obtener para el tiempo de salida, el retraso promedio por día acumulado a lo
#' largo mes para las aerolíneas US y VX. Graficarlo.
#' 
#' 1. Filtrar carrier con estos dos valores, 
#' 2. Crear un datetime por día
#' 3. Agrupar por carrier y la variable de fecha
#' 4. Obtener el promedio de retrasos por día
#' 5. Obtener el acumulado (la función cumsum puede ayudar). Es decir, si el 
#'     vector de retrasos para una aerolínea es (2.3, 3, -.1), el acumulado es
#'     (2.3, 2.3 + 3, 2.3 + 3 - .1) = (2.3, 5.3, 5.2)
#' 6. Graficar con ggplot
#' Extra. Graficar bandas a una distancia de 1 * desviación estándar para 
#'     cada día. https://ggplot2.tidyverse.org/reference/geom_ribbon.html


