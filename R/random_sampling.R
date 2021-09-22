#' @title Implementation of Active Learning using a random sampling strategy.
#'
#' @name al_random_sampling
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Active Learning improves the results of a classification by
#' feeding the classifier with informative samples. This function receives a
#' set of labelled and unlabelled samples. The labelled samples are used to
#' train a model which is then used to classify the unlabelled samples and then
#' it computes some metrics on the results. These metrics are useful for
#' selecting the samples to be sent to an expert (oracle) for labeling.
#'
#' This function receives a sits tibble and it returns it with metrics.
#' However, this function doesn't guarantee the order of the returned samples.
#'
#' @param samples_tb  A sits tibble.
#' @param sits_method A sits model specification.
#' @param multicores  The number of cores available for active learning.
#' @return            A sits tibble with metrics. Entropy is a measure of the
#'                    amount of information in the probabilities of each label;
#'                    the samples with largest entropy are the best candidates
#'                    for labeling by human experts. Least Confidence is the
#'                    difference between the most confident prediction and 100%
#'                    confidence normalized by the number of labels. Margin of
#'                    Confidence is the difference between the two most
#'                    confident predictions. Ratio of Confidence is the ratio
#'                    between the top two most confident predictions.
#' @export
al_random_sampling <- function(samples_tb,
                               sits_method,
                               multicores = 1){

    if (missing(sits_method))
        stop("Missing classification method.")

    sits:::.sits_tibble_test(samples_tb)

    .al_check_time_series(samples_tb)

    label_tb <- samples_tb %>%
        dplyr::filter(nchar(label) > 0,
                      label != "NoClass")

    no_label_tb <- samples_tb %>%
        dplyr::filter(is.na(label) | label == "" | label == "NoClass")

    assertthat::assert_that(
        nrow(label_tb) > 0,
        msg = "al_random_sampling: please provide some labelled samples"
    )

    assertthat::assert_that(
        nrow(no_label_tb) > 0,
        msg = "al_random_sampling: please provide some unlabelled samples"
    )

    rs_tb <- .al_rs(s_labelled_tb = label_tb,
                    s_unlabelled_tb = no_label_tb,
                    sits_method = sits_method,
                    multicores = multicores)

    points_tb <- label_tb %>%
        dplyr::mutate(entropy     = NA,
                      least_conf  = NA,
                      margin_conf = NA,
                      ratio_conf  = NA,
                      new_label   = NA) %>%
        dplyr::bind_rows(rs_tb)

    sits:::.sits_tibble_test(points_tb)

    return(points_tb)
}



#' @title Implementation of Active Learning using random sampling
#'
#' @name .al_rs
#'
#' @keywords internal
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description This function returns a sits tibble with metrics.
#'
#' @param s_labelled_tb   A sits tibble with labelled samples.
#' @param s_unlabelled_tb A sits tibble with unlabelled samples.
#' @param sits_method A sits model specification.
#' @param multicores  The number of cores available for active learning.
#' @return            A sits tibble with metrics. Entropy is a measure of the
#'                    amount of information in the probabilities of each label;
#'                    the samples with largest entropy are the best candidates
#'                    for labeling by human experts. Least Confidence is the
#'                    difference between the most confident prediction and 100%
#'                    confidence normalized by the number of labels. Margin of
#'                    Confidence is the difference between the two most
#'                    confident predictions. Ratio of Confidence is the ratio
#'                    between the top two most confident predictions.
#'
.al_rs <- function(s_labelled_tb,
                   s_unlabelled_tb,
                   sits_method,
                   multicores) {

    my_model <- sits::sits_train(data = s_labelled_tb,
                                 ml_method = sits_method)

    points_tb <- sits::sits_classify(data = s_unlabelled_tb,
                                     ml_model = my_model,
                                     multicores = multicores)

    metrics <- lapply(seq_len(nrow(points_tb)),
                      .al_rs_compute_metrics,
                      points_tb = points_tb)

    points_tb <- dplyr::bind_cols(points_tb, do.call(rbind, metrics))
    points_tb <- dplyr::arrange(points_tb, dplyr::desc(entropy))
    points_tb[["predicted"]] <- NULL

    return(points_tb)
}



#' @title Compute metrics of active learning using random sampling.
#'
#' @name .al_compute_metrics
#'
#' @keywords internal
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Get a sits tibble of random points in the given data cube.
#'
#' @param x         A length-one integer.
#' @param points_tb A sits tibble.
#'
#' @return           A sits tibble of samples, including their time series.
#'
.al_rs_compute_metrics <- function(x, points_tb){

    pred_df <- points_tb[x, ][["predicted"]][[1]]

    probs <- unlist(pred_df[["probs"]][[1]])

    least_conf <- (1 - max(probs)) * length(probs) / (length(probs) - 1)

    best_two <- sort(probs, decreasing = TRUE)[1:2]
    names(best_two) <- NULL

    margin_conf <- 1 - (best_two[1] - best_two[2])

    ratio_conf <- best_two[1] / best_two[2]

    entropy <- -1 * sum(probs * log(probs))

    data.frame(entropy = entropy,
               least_conf = least_conf,
               margin_conf = margin_conf,
               ratio_conf = ratio_conf,
               new_label = pred_df[["class"]])
}
