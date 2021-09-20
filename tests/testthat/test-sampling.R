test_that("Generate random samples", {

    cube <- sits::sits_cube(
        source = "LOCAL",
        name = "sinop-2014",
        satellite = "TERRA",
        sensor = "MODIS",
        data_dir = system.file("extdata/raster/mod13q1", package = "sits"),
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
    )

    samples_tb <- sits_get_random_points(data_cube = cube,
                           n_samples = 100,
                           multicores = 1)

    expect_true(sits:::.sits_test_tibble(samples_tb))
    expect_true(nrow(samples_tb) == 100)
})
