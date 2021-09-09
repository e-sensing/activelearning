test_that("Generate samples for active learning using random selection", {

    samples_tb  <- sits::sits_select(sits::samples_modis_4bands,
                                     bands = "EVI")
    sits_method <- sits::sits_rfor(num_trees = 200)

    data_cube <- sits::sits_cube(
        source = "LOCAL",
        name = "sinop-2014",
        satellite = "TERRA",
        sensor = "MODIS",
        data_dir = system.file("extdata/raster/mod13q1", package = "sits"),
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
    )

    set.seed(123)
    new_samples <- sits_al_random_sampling(samples_tb, sits_method, data_cube,
                                           n_samples = 100,
                                           multicores = 1)

    expect_true(sits:::.sits_test_tibble(new_samples))
    expect_true(inherits(new_samples, "sits"))
    expect_true(nrow(new_samples) == 100)
    expect_true(all(c("entropy", "least_conf",
                      "margin_conf", "ratio_conf") %in% colnames(new_samples)))

    expect_true(is.double(new_samples[["entropy"]]))
    expect_true(is.double(new_samples[["least_conf"]]))
    expect_true(is.double(new_samples[["margin_conf"]]))
    expect_true(is.double(new_samples[["ratio_conf"]]))

    expect_true(all(new_samples[["entropy"]][!is.na(new_samples[["entropy"]])] > 0))
    expect_true(all(new_samples[["least_conf"]]  > 0))
    expect_true(all(new_samples[["margin_conf"]] > 0))
    expect_true(all(new_samples[["ratio_conf"]]  > 0))

    expect_true(all(dplyr::between(new_samples[["least_conf"]], 0, 1)))
    expect_true(all(dplyr::between(new_samples[["margin_conf"]], 0, 1)))

    expect_true(all(new_samples[["label"]] %in% samples_tb[["label"]]))
})

test_that("Generate samples for active learning using EGAL algorithm", {

    samples_tb <- sits::sits_select(sits::samples_modis_4bands, bands = "EVI")
    set.seed(234)

    data_cube <- sits::sits_cube(
        source = "LOCAL",
        name = "sinop-2014",
        satellite = "TERRA",
        sensor = "MODIS",
        data_dir = system.file("extdata/raster/mod13q1", package = "sits"),
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date")
    )

    new_samples <- sits_al_egal(samples_tb, data_cube,
                                n_samples = 100,
                                multicores = 1)

    expect_true(sits:::.sits_test_tibble(new_samples))
    expect_true(inherits(new_samples, "sits"))
    expect_true("egal" %in% colnames(new_samples))

    # sf::st_sample doesn't guarantee the number of samples returned match the
    # number of samples requested. Besides, some random samples cloud fall in
    # data cube's areas with no data.
    expect_true(nrow(new_samples) > 0)
})
