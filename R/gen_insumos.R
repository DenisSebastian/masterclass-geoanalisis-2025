




# GEocodificación: --------------------------------------------------------

library(tidygeocoder)
library(dplyr)

addresses <- data.frame(
  address = c("La Moneda, Santiago, Chile",
              "Avenida Diagonal Las Torres 2640, Peñalolen",
              "Presidente Errázuriz 3485, Las Condes,"))
geo_data <- addresses %>%
  geocode(address, method = "osm")

saveRDS(geo_data, "data/geoding_base.rds")



geo_data_sf  <- geo_data %>%
  sf::st_as_sf(
    coords = c("long", "lat"),
    crs = 4326)
mapview(geo_data_sf)





# Definir Parámetros


library(SpatialKDE)

#Definirán Parámemetros de Estudio
cell_size <- 100 # Tamaño de Celda
band_width <- 500 #  Parámetro de Suavisado (denominada ventana o h)



# Crear Raster Vacío


raster_violencia <- violencia %>%
  create_raster(cell_size = cell_size, side_offset = band_width)


# Crear Kernel raster


kde_raster <- violencia %>%
  kde(band_width = band_width, kernel = "quartic", grid = raster_violencia)


# Histograma de valores de Densidad


hist(kde_raster, col="springgreen4", main="Histograma KDE",
     ylab="Número de Pixeles", xlab="valor KDE")

# writeRaster(kde_raster, "data/delitos/kde_delvio_LC.tif")


# Visualizar Raster KDE con ggplot


# rastero to df (na omit)
kde_raster_df <- raster::as.data.frame(kde_raster, xy = TRUE) %>% na.omit()

umbral <- 1
kde_raster_df <- kde_raster_df %>%
  mutate(layer = ifelse(layer < umbral, NA, layer))



ggplot() +
  geom_raster(data = kde_raster_df %>% na.omit() ,
              aes(x = x, y = y,
                  fill = layer)) +
  scale_fill_gradientn(name = "KDE",
                       colors = (viridis::magma(100)), na.value = NA)+
  coord_fixed()+
  ggtitle(paste0("KDE Raster de Delitos") ) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "gray80"),
        panel.grid.minor = element_line(colour = "gray80"))


na_menor <- function(x){
  x[x < 1] <- NA
  return(x)
}
kde_raster_f <- raster::calc(kde_raster, fun = na_menor)


kde_results <- list(r_vacio =raster_violencia,
                    r_kernel = kde_raster,
                    r_kde_df = kde_raster_df,
                    r_filtered = kde_raster_f)

saveRDS(kde_results, "data/kde.rds")
