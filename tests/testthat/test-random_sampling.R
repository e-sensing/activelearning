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
        al_random_sampling(sits_method = xgb_method)

    rs_vec <- rs_tb %>%
        dplyr::pull(entropy)

    # The result is a valid sits' tibble.
    expect_true(sits:::.sits_tibble_test(rs_tb))

    # The input's number of rows have to match the output's.
    expect_true(nrow(rs_tb) == (nrow(labelled_tb) + nrow(unlabelled_tb)))

    # The metrics exist and they have the right data type.
    expect_true(all(c("entropy", "least_conf",
                      "margin_conf", "ratio_conf") %in% colnames(rs_tb)))
    expect_true(is.double(rs_tb[["entropy"]]))
    expect_true(is.double(rs_tb[["least_conf"]]))
    expect_true(is.double(rs_tb[["margin_conf"]]))
    expect_true(is.double(rs_tb[["ratio_conf"]]))

    # The metrics' values are valid.
  expect_true(all(rs_tb[["least_conf"]][!is.na(rs_tb[["least_conf"]])]   >= 0))
  expect_true(all(rs_tb[["margin_conf"]][!is.na(rs_tb[["margin_conf"]])] >= 0))
  expect_true(all(rs_tb[["ratio_conf"]][!is.na(rs_tb[["ratio_conf"]])]   >= 0))
  expect_true(all(rs_tb[["entropy"]][!is.na(rs_tb[["entropy"]])]         >= 0))
  expect_true(all(rs_tb[["least_conf"]][!is.na(rs_tb[["least_conf"]])]   <= 1))
  expect_true(all(rs_tb[["margin_conf"]][!is.na(rs_tb[["margin_conf"]])] <= 1))

    # The number of NAs in the metric should match the number of labelled
    # samples.
    expect_true(nrow(labelled_tb) == sum(is.na(rs_vec)))

    # The labelled samples have egal == NA
    expect_true(any(is.na(rs_vec)))

    # The egal metric is positive.
    expect_true(all(rs_vec[!is.na(rs_vec)] > 0))

})
