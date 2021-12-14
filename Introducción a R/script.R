

# Datos ---- 
#'
#' Los datos pueden ser descargados de 
#' https://www.kaggle.com/c/house-prices-advanced-regression-techniques
#' El dataset contiene variables sobre casas y su precio de venta. Sirve para 
#' entrenar modelos de regresión 
#'

# Lectura de los datos ----

housing_dataset = read.csv('data/train.csv')[, c('SalePrice', 'LotArea', 'OverallQual', 'OverallCond', 'FullBath', 'GarageArea')]
head(housing_dataset)

# housing_dataset es un DataFrame con 1460 observaciones y 6 variables.

# Manipulación de los datos ----

# Para seleccionar una columna ocupamos dataframe$nombre_col

housing_dataset$SalePrice

# Exploración de los datos ----

# Un histograma sencillo
hist(housing_dataset$SalePrice)

# Agregándole estilo


