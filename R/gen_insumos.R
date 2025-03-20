
# Antecedentes ------------------------------------------------------------
# Título: Códigos Masterclass 2025
# Tema: Herramientas de Geoanálisis para aplicación de Políticas Públicas
# Autor: Denis Berroeta
# Fecha: 19-03-2025

# Carga de librerías ------------------------------------------------------------

# Librerías necesarias para el análisis geoespacial y visualización de datos
library(tidygeocoder)  # Geocodificación de direcciones
library(dplyr)         # Manipulación de datos
library(sf)            # Datos espaciales
library(raster)        # Manipulación de datos raster
library(ggplot2)       # Visualización de datos
library(mapview)       # Exploración interactiva de mapas
library(SpatialKDE)    # Estimación de densidad kernel
library(exactextractr) # Extracción de valores raster

# Geocodificación de Direcciones ------------------------------------------------

# Definir un conjunto de direcciones para geocodificar
addresses <- data.frame(
  address = c("La Moneda, Santiago, Chile",
              "Avenida Diagonal Las Torres 2640, Peñalolén",
              "Presidente Errázuriz 3485, Las Condes"))

# Realizar la geocodificación utilizando OpenStreetMap (OSM)
geo_data <- addresses %>%
  geocode(address, method = "osm")

# Guardar los resultados en un archivo RDS
saveRDS(geo_data, "data/geoding_base.rds")

# Convertir los datos a un formato espacial
geo_data_sf  <- geo_data %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326)

# Visualizar los puntos en un mapa interactivo
mapview(geo_data_sf)

# Kernel Density Estimation (KDE) -----------------------------------------------

# Cargar datos de delitos
violencia_df <- readRDS(file = "data/delitos/casos_violencia.rds")
head(violencia_df)

# Definir parámetros para el análisis de KDE
cell_size <- 100   # Tamaño de celda en metros
band_width <- 500  # Parámetro de suavizado (ventana de KDE)

# Crear un raster vacío para KDE
raster_violencia <- violencia_df %>%
  create_raster(cell_size = cell_size, side_offset = band_width)

# Generar el mapa de densidad utilizando KDE
kde_raster <- violencia_df %>%
  kde(band_width = band_width, kernel = "quartic", grid = raster_violencia)

# Visualización del histograma de densidad
hist(kde_raster, col="springgreen4", main="Histograma KDE",
     ylab="Número de Pixeles", xlab="Valor KDE")

# Convertir raster a data frame para visualización con ggplot
kde_raster_df <- raster::as.data.frame(kde_raster, xy = TRUE) %>% na.omit()

# Definir umbral para eliminar valores bajos
umbral <- 1
kde_raster_df <- kde_raster_df %>%
  mutate(layer = ifelse(layer < umbral, NA, layer))

# Visualización del mapa de densidad con ggplot2

ggplot() +
  geom_raster(data = kde_raster_df %>% na.omit(),
              aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(name = "KDE",
                       colors = viridis::magma(100), na.value = NA) +
  coord_fixed() +
  ggtitle("KDE Raster de Delitos") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "gray80"),
        panel.grid.minor = element_line(colour = "gray80"))

# Función para eliminar valores por debajo del umbral
na_menor <- function(x) {
  x[x < 1] <- NA
  return(x)
}

# Aplicar la función al raster KDE
kde_raster_f <- raster::calc(kde_raster, fun = na_menor)

# Visualización del raster KDE filtrado
mapview(kde_raster_f)

# Indicador de Escolaridad del Jefe de Hogar ------------------------------------

# Cargar datos espaciales de manzanas censales
mz_comuna <- readRDS("data/mz_las_condes.rds")
plot(mz_comuna$geometry, col = "gray60")

# Calcular el indicador de escolaridad basado en KDE
mz_comuna  <- mz_comuna %>%
  mutate(idelv = exact_extract(kde_raster, y = ., 'mean',
                               progress = FALSE))

# Visualizar los valores calculados
mz_comuna %>% dplyr::select(NOM_COM, COD_INE_ID, POBLACION, idelv)

# Mapa del indicador de delitos violentos

ggplot() +
  geom_sf(data = mz_comuna, aes(fill = idelv), color = "gray80",
          alpha = 0.8, size = 0.1) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  ggtitle("Indicador de Delitos Violentos") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "gray80"),
        panel.grid.minor = element_line(colour = "gray80"))

# Visualización interactiva con mapview
mapview(mz_comuna)
