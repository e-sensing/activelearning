#' @title Implementation of Active Learning using a random sampling stategy.
#'
#' @name sits_al_random_sampling
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Active Learning improves the results of a classification by
#' feeding the classifier with informative samples. This function returns a sits
#' tibble of new samples selected at random and some metrics useful for
#' selecting the samples to be sent to an expert for labeling.
#'
#' These new_samples could be merged to the original samples (samples_tb) to
#' increase the number of training data set.
#'
#' @param samples_tb  A sits tibble.
#' @param sits_method A sits model specification.
#' @param data_cube   A sits data cube.
#' @param n_samples   The number of random points to take.
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
sits_al_random_sampling <- function(samples_tb, sits_method,
                                    data_cube,
                                    n_samples = 1000,
                                    multicores = 2){

    # Avoid warning while checking the cube.
    entropy <- NULL

    sits:::.sits_test_tibble(samples_tb)

    # precondition: A method is given.
    assertthat::assert_that(
        !purrr::is_null(sits_method),
        msg = "sits_al_random_sampling: please provide a classification method"
    )

    # precondition: A data cube is given.
    assertthat::assert_that(
        !purrr::is_null(data_cube),
        msg = "sits_al_random_sampling: please provide a data cube"
    )

    # Classify the new samples
    my_model <- sits::sits_train(data = samples_tb,
                                 ml_method = sits_method)

    points_tb <- .sits_get_random_points(data_cube = data_cube,
                                         n_samples = n_samples,
                                         multicores = multicores)

    points_tb <- sits::sits_classify(data = points_tb,
                                     ml_model = my_model,
                                     multicores = multicores)

    # Compute metrics using the new sample points.
    metrics <- lapply(seq_len(nrow(points_tb)),
                      .sits_compute_metrics,
                      points_tb = points_tb)
    points_tb <- dplyr::bind_cols(points_tb, do.call(rbind, metrics))
    points_tb <- dplyr::arrange(points_tb, dplyr::desc(entropy))
    points_tb[["label"]] <- points_tb[["new_label"]]
    points_tb[["new_label"]] <- NULL
    points_tb[["predicted"]] <- NULL

    # postcondition: We return a valid sits tibble with additional columns.
    sits:::.sits_test_tibble(points_tb)

    return(points_tb)
}


#' @title Compute metrics of active learning using random sampling.
#'
#' @name .sits_compute_metrics
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
.sits_compute_metrics <- function(x, points_tb){
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
