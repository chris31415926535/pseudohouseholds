
testthat::test_that("Sequential processing words", {
  regions <- dplyr::bind_rows(region_shp, region_shp, region_shp)
  regions$region_id <- 1:nrow(regions)

  phhs <- get_phhs_parallel(regions = regions, region_idcol = "region_id", region_popcol = "population", roads = road_shp, roads_idcol = "road_id")

  #Result has correct classes"
  testthat::expect_equal(class(phhs), c("sf", "tbl_df", "tbl", "data.frame"))

  #Result has correct number of rows
  testthat::expect_equal(nrow(phhs), 105)

  # PHH populations sum to original region population
  testthat::expect_equal(sum(phhs$population), sum(regions$population))
})
