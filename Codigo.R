#Integrantes: Javier Morales - Paola Olave - Sofía Perucci - Pedro Rojas
  
#Objetivo: Identificar las localidades beneficiadas por el área de influencia
# del ramal Talca - Constitución. Así como conocer las principales características de 
# la población.


# Librerías ---------------------------------------------------------------

library(sf)
library(dplyr)
library(mapview)
library(DataExplorer)
library(networkD3)
library(readr)
library(units)
options(digits=2)


# Datos -------------------------------------------------------------------

comunas <- st_read("data/Comunas_Ramal.shp") %>% 
  st_transform(32719) %>% 
  st_cast("POLYGON")

localidades <- st_read("data/Localidades.shp") %>% 
  st_transform(32719) %>% 
  st_cast("POLYGON")


vias <- st_read("data/vias.shp") %>% 
  st_transform(32719)

mz <- st_read("data/R07_MZ_INFO.shp") %>% 
  st_transform(32719)

info_ramal <- read_delim("data/QGIS_RAMAL.csv", 
              delim = ";", escape_double = FALSE, 
              col_types = cols(GEOCODIGO = col_character()),
              locale = locale(encoding = "ISO-8859-1"), 
              trim_ws = TRUE)

# Visión Exploratoria de los Datos ----------------------------------------

com_str <- DataExplorer::plot_str(comunas)
localidad_str <- DataExplorer::plot_str(localidades)
vias_str <- DataExplorer::plot_str(vias)
mz_str <- DataExplorer::plot_str(mz)

# Se aprecia que hay una gran cantidad de columnas, por lo que sería bueno seleccionar
# las relevantes.


# Selección de columnas ---------------------------------------------------

localidades <- localidades %>% 
  dplyr::select(COD_REGION,REGION,COD_PROVIN,PROVINCIA,COD_COMUNA
                ,NOMBRE_COM,DISTRITO,AREA,COD_LOCALI,LOCALIDAD,GEOCODIGO,
                SEXO_HOMBR, SEXO_MUJER, SEXO_SEX_T, geometry) %>% 
  dplyr::rename(POBLACION = SEXO_SEX_T)

# Elimino duplicados de localidades
localidades <- localidades %>% dplyr::distinct(GEOCODIGO, .keep_all = TRUE)

mz <- mz %>% 
  dplyr::select(ID_MANZ, MANZ_EN, ZONA,TOTAL_V, HOG_N, geometry )


info_ramal <- info_ramal %>% dplyr::select(GEOCODIGO,                                    
              'Indice de Masculinidad', 'Tasa de Desocupación', 'Tasa de Participación',                     
              'Indice de Envejecimiento', 'Indice de Dependencia', 'Participación Sector Primario'                 
              ,'Participación de Mujeres en Jefatura del Hogar')


# Mapa dinámico

mapview(comunas) + localidades + vias + mz

# A primera vista, es posible observar que las localidades comparten geometría con
# las manzanas. Y que su "llave" es el geocodigo, o zona censal.


# Unión de Información ----------------------------------------------------

# Agregar información de comunas a vías

vias <- st_join(vias, comunas)

# Dejar solo registros del área de estudio
vias <- vias %>% filter(!is.na(REGION))

# Unir información de localidades con manzanas / para saber cantidad de viviendas

mz_df <- mz  %>% st_drop_geometry() %>% 
    group_by(ZONA) %>% 
    summarize(TOTAL_V = sum(TOTAL_V),
              HOG_N = sum(HOG_N))

localidades_df <- localidades %>% st_drop_geometry()

localidades_df$GEOCODIGO <- as.character(localidades_df$GEOCODIGO)

mz_localidades <- inner_join(localidades_df, mz_df, by = c("GEOCODIGO" = "ZONA")) %>% 
  as.data.frame()

# Unir información del csv a manzanas

localidades_ramal <- inner_join(info_ramal, mz_localidades, by = "GEOCODIGO") 


# Agregar geometria

localidades_sf <- localidades %>% dplyr::select(GEOCODIGO, geometry) %>% 
  as.data.frame()
localidades_sf$GEOCODIGO <- as.character(localidades_sf$GEOCODIGO)

localidades_ramal <- left_join(localidades_ramal, localidades_sf, by = "GEOCODIGO") %>% 
  st_as_sf()


# De esta manera se tiene la info de la localidad, con la info de mz y la del 
# csv de ramal


# Geoprocesos -------------------------------------------------------------

#Se crea un buffer de 1000 metros como área de influencia

# Buffer  -----------------------------------------------------------------

vias_d <- vias %>% st_union()
vias_buffer <- st_buffer(vias_d, dist = 1000)


# Se intersecta con las localidades, se calcula área

localidades_f <- st_intersection(localidades_ramal, vias_buffer)
 
# Calculo de área y densidad población para cada área de influencia

localidades_f <- localidades_f %>% 
  dplyr::mutate(AREA_KM = as.numeric(st_area(localidades_f) %>%
                         set_units(km2)),
                DENS_POB = as.numeric(POBLACION/AREA_KM))

# Guardar Shapefile

st_write(localidades_f, "resultado/influencia_ramal.shp")


# De este modo se redujo la cantidad de localidades de 56 a 36, las cuales
# son beneficiadas por un área de influencia de 1 km a lo largo del ramal.
# De igual forma se obtuvo una breve caracterización de la poblaciónm
# en relación con temas como cantidad de hogares, viviendas, densidad, 
# índices en general, entre otros. 



