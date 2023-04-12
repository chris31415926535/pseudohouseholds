

testthat::test_that("multiplication works", {
  testthat::expect_equal(2 * 2, 4)
})


testthat::test_that("Single region function works on synthetic data", {

  testthat::test_that(
    "Full function works ",{
    phhs <- get_phhs_region(region = region_shp, region_idcol = "region_id", region_popcol = "population", roads = road_shp, roads_idcol = "road_id")

    testthat::expect_equal(class(phhs), c("sf", "tbl_df", "tbl", "data.frame"))
    testthat::expect_equal(nrow(phhs), 35)
    testthat::expect_equal(sum(phhs$population), region_shp$population)

    }
   )
  # # works with no population column
  # get_phhs_region(region = input_regions[1,], region_idcol = "DBUID",  roads = ottawa_road_filtered_shp, roads_idcol = "NGD_UID")
  #
  # # works with no road id column
  # get_phhs_region(region = input_regions[1,], region_idcol = "DBUID",  roads = ottawa_road_filtered_shp, roads_idcol = NA)
  #
  #
  # # should fail: different crs
  # get_phhs_region(region = input_regions[1,], region_idcol = "DBUID", region_popcol = "dbpop2021", roads = roads_wgs, roads_idcol = "NGD_UID")
  #
  # # should fail: not sf
  # get_phhs_region(region = dplyr::tibble(), region_idcol = "DBUID", region_popcol = "dbpop2021", roads = roads_wgs, roads_idcol = "NGD_UID")
  # get_phhs_region(region = input_regions[1,], region_idcol = "DBUID", region_popcol = "dbpop2021", roads = dplyr::tibble(), roads_idcol = "NGD_UID")
  #
  # # should fail: no roads or regions
  # get_phhs_region(region = input_regions[1,], region_idcol = "DBUID", region_popcol = "dbpop2021", roads = ottawa_road_filtered_shp[0,], roads_idcol = "NGD_UID")
  # get_phhs_region(region = input_regions[0,], region_idcol = "DBUID", region_popcol = "dbpop2021", roads = ottawa_road_filtered_shp[0,], roads_idcol = "NGD_UID")
  #
  # # should fail: using WGS84
  # get_phhs_region(region = sf::st_transform(input_regions[1,], crs="WGS84"), region_idcol = "DBUID", region_popcol = "dbpop2021", roads = roads_wgs, roads_idcol = "NGD_UID")
  #
  # # should fail: bad column ids
  # get_phhs_region(region = input_regions[1,], region_idcol = "asdf", region_popcol = "dbpop2021", roads = ottawa_road_filtered_shp, roads_idcol = "NGD_UID")
  # get_phhs_region(region = input_regions[1,], region_idcol = "DBUID", region_popcol = "asdf", roads = ottawa_road_filtered_shp, roads_idcol = "NGD_UID")
  # get_phhs_region(region = input_regions[1,], region_idcol = "DBUID", region_popcol = "dbpop2021", roads = ottawa_road_filtered_shp, roads_idcol = "asdsf")


})

# get_phhs_region(region = region_shp, region_idcol = "region_id", region_popcol = "population", roads = road_shp, roads_idcol = "road_id")
