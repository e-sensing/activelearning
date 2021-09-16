test_that("Test input samples", {

    samples_tb <- sits::sits_select(sits::samples_modis_4bands,
                                    bands = "EVI")

    all_unlabelled <- samples_tb %>%
        dplyr::mutate(label = NA)

    # The sample set must contain both labelled and unlabelled samples.
    expect_error(al_egal(samples_tb))
    expect_error(al_egal(all_unlabelled))
})



test_that("Test expected usage", {

    samples_tb <- sits::sits_select(sits::samples_modis_4bands,
                                    bands = "EVI") %>%
        dplyr::mutate(sample_id = 1:nrow(.))

    labelled_tb <- samples_tb %>%
        dplyr::group_by(label) %>%
        dplyr::sample_n(20) %>%
        dplyr::ungroup()

    unlabelled_tb <- samples_tb %>%
        dplyr::filter(!(sample_id %in% labelled_tb$sample_id)) %>%
        dplyr::group_by(label) %>%
        dplyr::sample_n(20) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(label = NA)

    egal_tb <- labelled_tb %>%
        dplyr::bind_rows(unlabelled_tb) %>%
        dplyr::select(-sample_id) %>%
        al_egal()

    egal_vec <- egal_tb %>%
        dplyr::pull(egal)

    # The result is a valid sits' tibble.
    expect_true(sits:::.sits_test_tibble(egal_tb))

    # The input's rows have to match the output's.
    expect_true(nrow(egal_tb) == (nrow(labelled_tb) + nrow(unlabelled_tb)))

    # The number of NAs in the metric should match the number of labelled
    # samples
    expect_true(nrow(labelled_tb) == sum(is.na(egal_vec)))

    # The labelled samples have egal == NA
    expect_true(any(is.na(egal_vec)))

    # The egal metric is positive (TODO: Check paper)
    expect_true(all(egal_vec[!is.na(egal_vec)] > 0))
})
