## code to prepare `DATASET` dataset goes here


# TESTING with synthetic data

coords_shp = matrix(data = c(
  0,0,
  0,1,
  0.5,1.5,
  1,1,
  1,0,
  0,0
), ncol = 2, byrow = TRUE) * 1000

coords_extra <- matrix(
  data = c(
    0,0,
    0.5,0.4,
    1,0
  ), ncol = 2, byrow = TRUE) * 1000

coords_extra2 <- matrix(
  data = c(
    0.5, 0.4,
    0.5, 1.5
  ), ncol = 2, byrow = TRUE) * 1000

region_shp <- sf::st_multipoint(coords_shp) |>
  sf::st_cast("POLYGON")|>
  sf::st_sfc() |>
  sf::st_as_sf(crs = 32189) |>
  dplyr::mutate(region_id = "A",
                population = 600)



# region_shp  |> ggplot() + geom_sf()


road1_shp <- sf::st_linestring(coords_shp)
road2_shp <- sf::st_linestring(coords_extra)
road3_shp <- sf::st_linestring(coords_extra2)

road_shp <- sf::st_multilinestring(list(road1_shp, road2_shp, road3_shp)) |>
sf::st_sfc() |>
  sf::st_as_sf(crs = 32189) |>
  dplyr::mutate(road_id = 1:dplyr::n())

#ggplot2::ggplot(road_shp) + ggplot2::geom_sf()

# ggplot2::ggplot() + ggplot2::geom_sf(data = region_shp) + ggplot2::geom_sf(data = road_shp)



# phh_test <- get_phhs_region(region = region_shp, region_idcol = "region_id", region_popcol = NA, roads = road_shp,
# roads_idcol = NA, phh_density = 0.001)

# ggplot2::ggplot() + ggplot2::geom_sf(data = region_shp) + ggplot2::geom_sf(data = road_shp) + ggplot2::geom_sf(data = phh_test, colour = "blue")


# escape unicode in CRS to avoid R CMD CHECK error https://github.com/r-spatial/sf/issues/1341#issuecomment-1120902127
sf::st_crs(region_shp)$wkt <- gsub("°|º", "\\\u00b0", sf::st_crs(region_shp)$wkt)
sf::st_crs(road_shp)$wkt <- gsub("°|º", "\\\u00b0", sf::st_crs(road_shp)$wkt)

usethis::use_data(region_shp, overwrite = TRUE)
usethis::use_data(road_shp, overwrite = TRUE)


######### Ottawa regions, populations, and roads

ottawa_db_shp <- sf::read_sf("~/datascience/data/spatial/ldb_000b21a_e/ldb_000b21a_e.shp") |>
  dplyr::filter(stringr::str_detect(DBUID, "^3506")) |>
  dplyr::select(DBUID)

ottawa_db_pop <- readr::read_csv("~/datascience/data/spatial/geographic_attribute_file/2021_92-151_X.csv") |>
  dplyr::select(DBUID = DBUID_IDIDU, dbpop2021 = DBPOP2021_IDPOP2021) |>
  dplyr::filter(DBUID %in% ottawa_db_shp$DBUID) |>
  dplyr::mutate(DBUID = as.character(DBUID))

ottawa_db_shp <- dplyr::left_join(ottawa_db_shp, ottawa_db_pop, by = "DBUID") |>
  dplyr::select(DBUID, dbpop2021, geometry) |>
  sf::st_transform(crs=32189)

sf::st_crs(ottawa_db_shp)$wkt <- gsub("°|º", "\\\u00b0", sf::st_crs(ottawa_db_shp)$wkt)

usethis::use_data(ottawa_db_shp, overwrite = TRUE)


ottawa_roads_shp <- sf::read_sf("~/datascience/data/spatial/lrnf000r21a_e/lrnf000r21a_e.shp",
                                query = 'SELECT NGD_UID, NAME, RANK, CLASS FROM "lrnf000r21a_e" WHERE CSDNAME_L = \'Ottawa\' OR CSDNAME_R = \'Ottawa\' ')

ottawa_roads_shp <- sf::st_transform(ottawa_roads_shp, crs=32189)

sf::st_crs(ottawa_roads_shp)$wkt <- gsub("°|º", "\\\u00b0", sf::st_crs(ottawa_roads_shp)$wkt)

usethis::use_data(ottawa_roads_shp, overwrite = TRUE)
