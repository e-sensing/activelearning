#' @title Implementation of Exploration Guided Active Learning (EGAL)
#'
#' @name al_egal
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Active Learning improves the results of a classification by
#' feeding the classifier with informative samples. The Exploration Guided
#' Active Learning (EGAL) ranks a set of samples based on their density and
#' diversity; those samples with a larger EGAL metric should be submitted first
#' to a human expert (the oracle) for classification.
#'
#' This function receives a sits tibble with time series samples and it computes
#' the EGAL metric on it. However, this function doesn't guarantee the order of
#' the returned samples.
#'
#' @references Hu, R., Jane Delany, S., & Mac Namee, B. (2010). EGAL:
#' Exploration Guided Active Learning for TCBR. In Lecture Notes in Computer
#' Science (including subseries Lecture Notes in Artificial Intelligence and
#' Lecture Notes in Bioinformatics): Vol. 6176 LNAI (pp. 156–170). doi:
#' 10.1007/978-3-642-14274-1_13
#'
#' @param samples_tb      A sits tibble with both labelled and unlabelled
#'                        samples (i.e. NA).
#' @param data_cube       A sits data cube.
#' @param sim_method      A character. A method for computing the similarity
#'                        among samples. See proxy::simil for details.
#' @param alpha           A double. It controls the radius of the neighborhood
#'                        used in the estimation of sample density.
#' @param beta            A double. It controls the radius of the neighborhood
#'                        used in the estimation of the sample candidate set. A
#'                        bigger beta gives a bigger set. By default is set to
#'                        be equal to alpha. If NULL, it is computed internally.
#' @param w               A numeric (between 0 and 1) only used when beta is
#'                        NULL. This proportion parameter balances the influence
#'                        of diversity and density in the selection strategy.
#'                        When w is 0, EGAL becomes a pure-diversity and when w
#'                        is 1, EGAL becomes a pure density-based sampling
#'                        algorithm.
#' @return                A sits tibble with the EGAL metric. This metric
#'                        ranks samples based on their density and diversity.
#'                        Those samples with highest EGAL should be selected
#'                        first for labeling.
#' @export
#'
al_egal <- function(samples_tb,
                    sim_method = "correlation",
                    alpha = NULL,
                    beta = alpha,
                    w = 0.5) {

    sits:::.sits_tibble_test(samples_tb)

    .al_check_time_series(samples_tb)

    label_tb <- samples_tb %>%
        dplyr::filter(nchar(label) > 0,
                      label != "NoClass")

    no_label_tb <- samples_tb %>%
        dplyr::filter(is.na(label) | label == "" | label == "NoClass")

    assertthat::assert_that(
        nrow(label_tb) > 0,
        msg = "al_egal: please provide some labelled samples"
    )

    assertthat::assert_that(
        nrow(no_label_tb) > 0,
        msg = "al_egal: please provide some unlabelled samples"
    )

    egal_tb <- .al_egal(s_labelled_tb = label_tb,
                        s_unlabelled_tb = no_label_tb,
                        alpha = alpha,
                        beta = beta,
                        w = w)

    assertthat::assert_that(
        nrow(no_label_tb) == nrow(egal_tb),
        msg = "al_random_sampling: please provide a classification method."
    )

    egal_tb <- label_tb %>%
        dplyr::mutate(egal = NA) %>%
        dplyr::bind_rows(egal_tb)

    sits:::.sits_tibble_test(egal_tb)

    return(egal_tb)
}



#' @title Implementation of Exploration Guided Active Learning (EGAL)
#'
#' @name .al_egal
#'
#' @keywords internal
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description This function returns a sits tibble with their score in the
#' metric Exploration Guided Active Learning (EGAL). Samples with a larger EGAL
#' metric should be submitted first to a human expert for classification.
#'
#' @param s_labelled_tb   A sits tibble with labelled samples.
#' @param s_unlabelled_tb A sits tibble with unlabelled samples.
#' @param sim_method      A character. A method for computing the similarity
#'                        among samples as described in the package proxy. If
#'                        the method is registered in proxy::pr_DB as a
#'                        distance, then the inverse of the distance is used
#'                        (e.g. 1/distance).
#'                        proxy::pr_DB as a distance, then the
#' @param alpha           A double. It controls the radius of the neighborhood
#'                        used in the estimation of sample density.
#' @param beta            A double. It controls the radius of the neighborhood
#'                        used in the estimation of the sample candidate set. A
#'                        bigger beta gives a bigger set. By default is set to
#'                        be equal to alpha. If NULL, it is computed internally.
#' @param w               A numeric (between 0 and 1) only used when beta is
#'                        NULL. This proportion parameter balances the influence
#'                        of diversity and density in the selection strategy.
#'                        When w is 0, EGAL becomes a pure-diversity
#'                        and when w is 1, EGAL becomes a pure density-based
#'                        sampling algorithm.
#' @return                A sits tibble with the EGAL metric. This metric
#'                        ranks samples based on their density and diversity.
#'                        Those samples with highest EGAL should be selected
#'                        first for labeling.
#'
.al_egal <- function(s_labelled_tb,
                     s_unlabelled_tb,
                     sim_method = "correlation",
                     alpha = NULL,
                     beta = alpha,
                     w = 0.5) {
    # NOTE:
    # - D is a dataset consisting of:
    # - U pool of unlabeled examples.
    # - L case base of labelled examples.
    # - x_i are selected from U and presented to the oracle for labeling and
    #       added to L.
    # - N_i is the neighborhood of x_i
    # - CS is the candidate set
    # - S is the similarity between each unlabelled example and its neareast
    #     labelled neighbor.

    # Check if the provided method is supported.
    stopifnot(proxy::pr_DB$entry_exists(sim_method))

    # Merge the samples and format them.
    dataset_tb <- dplyr::bind_rows(s_labelled_tb,
                                   s_unlabelled_tb)
    time_series <- as.matrix(sits:::.sits_distances(dataset_tb)[,-2:0])

    # Compute similarity.
    sim_entry <- proxy::pr_DB$get_entry(sim_method)
    if (sim_entry$distance) {
        similarity_mt <- 1 / as.matrix(proxy::dist(time_series,
                                                   method = sim_method))
    }else{
        similarity_mt <- as.matrix(proxy::simil(time_series,
                                                method = sim_method))
    }

    # Compute alpha
    sim_diag <- similarity_mt
    sim_diag[lower.tri(sim_diag, diag = TRUE)] <- NA

    if (any(is.infinite(sim_diag), na.rm = TRUE))
        stop("Duplicated samples found!")

    if (is.null(alpha)) {
        alpha <- mean(sim_diag, na.rm = TRUE) - 0.5 * stats::sd(sim_diag,
                                                                na.rm = TRUE)
    }

    # Build the candidate set. NOTE: In CS_mat, the rows are the Unlabeled
    # samples and the columns are the Labeled samples.
    CS_mat <- similarity_mt

    CS_mat <- CS_mat[(nrow(s_labelled_tb) + 1):nrow(CS_mat),
                     1:nrow(s_labelled_tb)]

    # Update beta
    if (is.null(beta))
        beta = .al_egal_update_beta(CS_mat, w)

    cs_vec <- apply(CS_mat, MARGIN = 1, FUN = function(x, beta){
        x[x > beta] <- NA
        if (all(is.na(x)))
            return(0)
        return(max(x, na.rm = TRUE))
    }, beta = beta)
    names(cs_vec) <- NULL

    points_tb <- s_unlabelled_tb
    points_tb["egal"] <- cs_vec
    points_tb <- points_tb[order(points_tb[["egal"]], decreasing = TRUE), ]

    return(points_tb)
}



#' @title Compute a new value for the beta parameter.
#'
#' @name .al_egal_update_beta
#'
#' @keywords internal
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Compute a new beta parameter for the Exploration Guided Active
#' Learning (EGAL) algorithm.
#'
#' @param S_mat A numeric matrix representing the similarity between unlabelled
#'              (rows) and labelled (columns) samples.
#' @param w     A numeric (between 0 and 1). A proportion parameter.
#'
#' @return      A length-one numeric.
#'
.al_egal_update_beta <- function(S_mat, w){
    S <- apply(S_mat, MARGIN = 1, FUN = max)
    threshold <- floor(w * length(S))
    return(sort(S)[[threshold]])
}
