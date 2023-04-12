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
