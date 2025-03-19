
# Generarción de Mapas Web para presentaciones

# Opciones Generales ------------------------------------------------------
options(browser="/usr/bin/open -a 'Google Chrome'")
setwd("../../book-change-detection/")

# Librerias ---------------------------------------------------------------
library(raster)
library(mapview)

# Recursos ----------------------------------------------------------------
source("R/DI.R")
source("R/graphics.R")
source("R/functions.R")
source("R/utils_image.R")
source("R/Douglas-Peucker.R")
source("R/log_mean_ratio.R")


# Paletas -----------------------------------------------------------------
palette_grey <- grey(1:100/100)
palette_rainbow <- rainbow(100)

# Si no existe directorio lo crea
make_dir <- function(path){
  if (!dir.exists(path)){
    print(paste0("Directorio Creado: ", path))
    dir.create(path, recursive = TRUE)
  }
}

make_map_web <- function(map, name, path_out, show=T){
  make_dir(path_out)
  
  path_out_html <- paste0(path_out, "/", name,".html") %>% 
    gsub(patter="//", replacement = "/", .)
  mapview::mapshot(map, url = path_out_html)
  if(isTRUE(show)){
    utils::browseURL(path_out_html)
  }
  print(paste0("Mapa web guardado en: ", path_out_html))
}

delete_black <- function(path_tif){
  img_rbg <- terra::rast(path_tif) %>% 
    raster::brick() %>%
    subset(c(1, 2, 3))
  # set all values with 0 to NA
  vals = values(img_rbg)
  idx = which(rowSums(vals) == 0)
  vals[idx, ] = cbind(NA, NA, NA)
  
  img_rbg = setValues(img_rbg, vals)
  return(img_rbg)
}

# Área de Estudio ---------------------------------------------------------

SITIO <- "ST_021"
suffix <- "_ODC"
suffix_null <- ""
path_avence <- "../presentaciones/avance_tesis"
path_html <- paste0(path_avence, "/html/", SITIO)



# ViewRGB -----------------------------------------------------------------
# RGB
st_2017 <- delete_black(paste0("results/", SITIO, "/", SITIO,"_2017_RGB_reproy.tif"))
st_2022 <- delete_black(paste0("results/", SITIO, "/", SITIO,"_2021_RGB_reproy.tif"))

m_2017 <- viewRGB(st_2017, r = 1, g = 2, b = 3, layer.name = "RBG_2017",
                  na.color = "transparent", quantiles = NULL)
m_2022 <- viewRGB(st_2022, r = 1, g = 2, b = 3, layer.name = "RBG_2022",
                  na.color = "transparent", quantiles = NULL)

rgb_img <- m_2017 + m_2022
make_map_web(map = rgb_img, name = "RGB_roi", path_out = path_html)


# Radar -------------------------------------------------------------------
# Insumos -----------------------------------------------------------------

img1_vh <- raster(paste0("data/turberas/tif/", SITIO, "/S1_GRD_SAR_VH_1S_2017.tif"))
img1_vv <- raster(paste0("data/turberas/tif/", SITIO, "/S1_GRD_SAR_VV_1S_2017.tif"))
img2_vh <- raster(paste0("data/turberas/tif/",SITIO,  "/S1_GRD_SAR_VH_1S_2023.tif"))
img2_vv <- raster(paste0("data/turberas/tif/",SITIO,  "/S1_GRD_SAR_VV_1S_2023.tif"))

# img1 <- img1_vv+img1_vh
# img2 <- img2_vv+img2_vh


# img1 <- img1_vh
# img2 <- img2_vh


sar_map <- rgb_img + 
  mapview(img1_vh, layer.name = "img1_vh_gee") + 
  mapview(img2_vh, layer.name = "img2_vh_gee") + 

make_map_web(map = sar_map, name = "SAR_roi", path_out = path_html)


# MC: Diferencia Directa --------------------------------------------------


name_image <- paste0("DD", suffix_null)
dd <- raster(paste0("results/", SITIO,"/", name_image,".tif"))
dd[dd ==0] <- NA
mdd <- mapview(dd, na.color =NA, alpha = 0.8, legend=F)


name_fil <- paste0("DD", suffix)
dd_fil <- raster(paste0("results/", SITIO,"/", name_fil,".tif"))
dd_fil[dd_fil ==0] <- NA
mdd_fil <- mapview(dd_fil, na.color =NA, alpha = 0.8, legend=F)

map_dd <- rgb_img + mdd+mdd_fil
make_map_web(map = map_dd, name = "DD_roi", path_out = path_html)



# Log Ratio ---------------------------------------------------------------

name_image <- paste0("LR", suffix_null)
lr <- raster(paste0("results/", SITIO,"/", name_image,".tif"))

map_lr <- rgb_img + mapview(lr)
make_map_web(map = map_lr, name = "LR_roi", path_out = path_html)



# Relación de verosimilitud logarítmica (LLR) -----------------------------


name_image <- paste0("LLR", suffix_null)
llr <- raster(paste0("results/", SITIO,"/", name_image,".tif"))

llr_cp <- llr
llr_cp[llr_cp <0.05] <- NA


map_llr <- rgb_img + mapview(llr) + mapview(llr_cp, na.color =NA, alpha = 0.8, legend=T)
make_map_web(map = map_llr, name = "LLR_roi", path_out = path_html)



# Enhanced Difference Image (EDI) -----------------------------------------


name_image <- paste0("EDI", suffix_null)
edi <- raster(paste0("results/", SITIO,"/", name_image,".tif"))
name_image_out <- paste0("EDI_Outliers", suffix_null)
edi_out <- raster(paste0("results/", SITIO,"/", name_image_out,".tif"))


map_edi <- rgb_img + mapview(edi)+mapview(edi_out,  na.color = NA)
make_map_web(map = map_edi, name = "EDI_roi", path_out = path_html)


# Log Mean Ratio ----------------------------------------------------------


name_image <- paste0("LMR", suffix_null)
lmr <- raster(paste0("results/", SITIO,"/", name_image,".tif"))

map_lmr <- rgb_img + mapview(lmr, na.color = NA)
make_map_web(map = map_lmr, name = "LMR_roi", path_out = path_html)


# MC: Triangular Threshold Segmentation (Douglas-Peucker) -----------------

name_image_dd <- paste0("TTS_DD", suffix_null)
tts_dd <- raster(paste0("results/", SITIO,"/", name_image_dd,".tif"))
name_image <- paste0("TTS_EDI", suffix_null)
tts_edi <- raster(paste0("results/", SITIO,"/", name_image,".tif"))
name_image_lmr <- paste0("TTS_LMR", suffix_null)
tts_lmr <- raster(paste0("results/", SITIO,"/", name_image_lmr,".tif"))


map_tts <- rgb_img + 
  mapview(tts_lmr, na.color =NA, col.regions ="magenta",  legend=F)+
  mapview(tts_dd, na.color = NA,  col.regions ="cyan", legend=F)+
  mapview(tts_edi, na.color = NA,  col.regions ="navy", legend=F)
make_map_web(map = map_tts, name = "TTS_roi", path_out =path_html)


#  PCA K-Means ------------------------------------------------------------

pcak_edi <- raster("results/ST_001/PCAK_EDI.tif")

map_pcak_edi <- rgb_img + mapview(pcak_edi, na.color = NA, col.region="orange") 
make_map_web(map = map_pcak_edi, name = "PCAK_EDI_roi", path_out = "results/ST_001/")


