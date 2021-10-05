test_that("Generate random samples", {

    cube <- sits::sits_cube(
        source = "LOCAL",
        name = "022024",
        origin = "BDC",
        collection = "CB4_64-1",
        data_dir = system.file("extdata/raster/mod13q1",
                               package = "sits"),
        bands = c("NDVI", "EVI"),
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
    )

    samples_tb <- al_get_random_points(data_cube = cube,
                                       n_samples = 100,
                                       multicores = 1)

    expect_true(sits:::.sits_tibble_test(samples_tb))
    expect_true(nrow(samples_tb) == 100)
})
