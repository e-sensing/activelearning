test_that("Test input samples", {

    samples_tb <- sits::sits_select(sits::samples_modis_4bands,
                                    bands = "EVI")

    all_unlabelled <- samples_tb %>%
        dplyr::mutate(label = NA)

    # The sample set must contain both labelled and unlabelled samples.
    expect_error(al_s2(samples_tb))
    expect_error(al_s2(all_unlabelled))
})



# test_that("Test expected usage", {
#
#     samples_tb <- sits::sits_select(sits::samples_modis_4bands,
#                                     bands = "EVI") %>%
#         dplyr::mutate(sample_id = 1:nrow(.))
#
#     labelled_tb <- samples_tb %>%
#         dplyr::group_by(label) %>%
#         dplyr::sample_n(20) %>%
#         dplyr::ungroup()
#
#     unlabelled_tb <- samples_tb %>%
#         dplyr::filter(!(sample_id %in% labelled_tb$sample_id)) %>%
#         dplyr::group_by(label) %>%
#         dplyr::sample_n(20) %>%
#         dplyr::ungroup() %>%
#         dplyr::mutate(label = NA)
#
#     s2_tb <- labelled_tb %>%
#         dplyr::bind_rows(unlabelled_tb) %>%
#         dplyr::select(-sample_id) %>%
#         al_s2()
#
# })
