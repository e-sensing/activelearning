test_that("Test input samples", {

    samples_tb <- sits::sits_select(sits::samples_modis_4bands,
                                    bands = "EVI")

    all_unlabelled <- samples_tb %>%
        dplyr::mutate(label = NA)

    # The sample set must contain both labelled and unlabelled samples.
    expect_error(al_egal(samples_tb))
    expect_error(al_egal(all_unlabelled))

})



test_that("Test expected usage: use similarity measure", {

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
        al_egal(sim_method = "correlation")

    egal_vec <- egal_tb %>%
        dplyr::pull(egal)

    # The result is a valid sits' tibble.
    expect_true(sits:::.sits_tibble_test(egal_tb))

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



test_that("Test expected usage: distance measure", {

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
        al_egal(sim_method = "Euclidean")

    egal_vec <- egal_tb %>%
        dplyr::pull(egal)

    expect_true(sits:::.sits_tibble_test(egal_tb))
    expect_true(nrow(egal_tb) == (nrow(labelled_tb) + nrow(unlabelled_tb)))
    expect_true(nrow(labelled_tb) == sum(is.na(egal_vec)))
    expect_true(any(is.na(egal_vec)))
    expect_true(all(egal_vec[!is.na(egal_vec)] > 0))

})


test_that("Test expected usage: dtw", {

    # NOTE: Only runs if the package dtw is available and registered in
    # proxy::pr_DB.
    if (proxy::pr_DB$entry_exists("DTW") == FALSE)
        skip(paste("DTW distance not found in proxy::pr_DB. Is the dtw",
                   "package installed?")

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
        al_egal(sim_method = "dtw")

    egal_vec <- egal_tb %>%
        dplyr::pull(egal)

    expect_true(sits:::.sits_tibble_test(egal_tb))
    expect_true(nrow(egal_tb) == (nrow(labelled_tb) + nrow(unlabelled_tb)))
    expect_true(nrow(labelled_tb) == sum(is.na(egal_vec)))
    expect_true(any(is.na(egal_vec)))
    expect_true(all(egal_vec[!is.na(egal_vec)] > 0))

})



test_that("Test expected usage: dtw basic", {

    # NOTE: Only runs if the package dtwclust is available and registered in
    # proxy::pr_DB.
    if (proxy::pr_DB$entry_exists("DTW_BASIC") == FALSE)
        skip(paste("DTW BASIC distance not found in proxy::pr_DB.",
                   "Is the dtwclust package installed?"))

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
        al_egal(sim_method = "dtw_basic")

    egal_vec <- egal_tb %>%
        dplyr::pull(egal)

    expect_true(sits:::.sits_tibble_test(egal_tb))
    expect_true(nrow(egal_tb) == (nrow(labelled_tb) + nrow(unlabelled_tb)))
    expect_true(nrow(labelled_tb) == sum(is.na(egal_vec)))
    expect_true(any(is.na(egal_vec)))
    expect_true(all(egal_vec[!is.na(egal_vec)] > 0))

})



test_that("Test beta", {

    S_vec <- c(0.56000687, 0.51189689, 0.47829857, 0.61489947, 0.39519776,
               0.79553551, 0.96079387, 0.85448260, 0.71149583, 0.41611900,
               0.78817815, 0.83565637, 0.73277149, 0.16041648, 0.11691063,
               0.71247692, 0.54083828, 0.90838921, 0.55135690, 0.05993021,
               0.84312628, 0.79776334, 0.11313904, 0.89010095, 0.66622674)

    S_mat <- matrix(data = S_vec,
                    nrow = 5,
                    byrow = TRUE)

    beta <- .al_egal_update_beta(S_mat = S_mat,
                                 w = 0.5)

    expect_equal(length(beta), 1)
    expect_equal(beta, 0.8356564, tolerance = TRUE)
})
