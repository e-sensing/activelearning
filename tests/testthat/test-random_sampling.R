test_that("Test input samples", {

    samples_tb <- sits::sits_select(sits::samples_modis_4bands,
                                    bands = "EVI")

    all_unlabelled <- samples_tb %>%
        dplyr::mutate(label = NA)

    # The sample set must contain both labelled and unlabelled samples.
    expect_error(al_random_sampling(samples_tb))
    expect_error(al_random_sampling(all_unlabelled))
})



test_that("Test expected usage", {

    samples_tb <- sits::samples_modis_4bands %>%
        sits::sits_select(bands = c("NDVI", "EVI")) %>%
        dplyr::mutate(sample_id = 1:nrow(.))

    classification_interval <- samples_tb %>%
        sits::sits_timeline() %>%
        range()

    modis_cube <- sits::sits_cube(
        source = "LOCAL",
        name = "sinop-2014",
        satellite = "TERRA",
        sensor = "MODIS",
        data_dir = system.file("extdata/raster/mod13q1",
                               package = "sits"),
        delim = "_",
        parse_info = c("X1", "X2", "tile", "band", "date"),
        start_date = dplyr::first(classification_interval),
        end_date   = dplyr::last(classification_interval)
    )

    xgb_method <- sits::sits_xgboost(verbose = FALSE)

    labelled_tb <- samples_tb %>%
        dplyr::group_by(label) %>%
        dplyr::sample_n(5) %>%
        dplyr::ungroup()

    unlabelled_tb <- samples_tb %>%
        dplyr::filter(!(sample_id %in% labelled_tb$sample_id)) %>%
        dplyr::group_by(label) %>%
        dplyr::sample_n(5) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(label = NA)

    rs_tb <- labelled_tb %>%
        dplyr::bind_rows(unlabelled_tb) %>%
        dplyr::select(-sample_id) %>%
        magrittr::set_class(class(samples_tb)) %>%
        al_random_sampling(sits_method = xgb_method,
                           data_cube = modis_cube,
                           n_samples = 400)

    rs_vec <- rs_tb %>%
        dplyr::pull(entropy)

    # The result is a valid sits' tibble.
    expect_true(sits:::.sits_test_tibble(rs_tb))

    # The input's rows have to match the output's.
    expect_true(nrow(rs_tb) == (nrow(labelled_tb) + nrow(unlabelled_tb)))

    # The number of NAs in the metric should match the number of labelled
    # samples
    expect_true(nrow(labelled_tb) == sum(is.na(rs_vec)))

    # The labelled samples have egal == NA
    expect_true(any(is.na(rs_vec)))

    # The egal metric is positive (TODO: Check paper)
    expect_true(all(rs_vec[!is.na(rs_vec)] > 0))
})
