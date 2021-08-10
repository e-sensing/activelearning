#' @title Implementation of Exploration Guided Active Learning (EGAL)
#'
#' @name sits_al_egal
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Active Learning improves the results of a classification by
#' feeding the classifier with informative samples. This function returns a sits
#' tibble of new samples selected at random along with their score in the metric
#' Exploration Guided Active Learning (EGAL).
#'
#' @references Hu, R., Jane Delany, S., & Mac Namee, B. (2010). EGAL:
#' Exploration Guided Active Learning for TCBR. In Lecture Notes in Computer
#' Science (including subseries Lecture Notes in Artificial Intelligence and
#' Lecture Notes in Bioinformatics): Vol. 6176 LNAI (pp. 156–170). doi:
#' 10.1007/978-3-642-14274-1_13
#'
#' @param samples_tb      A sits tibble.
#' @param data_cube       A sits data cube.
#' @param sim_method      A character. A method for computing the similarity
#'                        among samples as described in the function dist in the
#'                        package proxy.
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
#' @param n_samples       The number of random points to take.
#' @param multicores      The number of cores available for active learning.
#' @return                A sits tibble with the EGAL metric. This metric
#'                        ranks samples based on their density and diversity.
#'                        Those samples with highest EGAL should be selected
#'                        first for labeling.
#' @export
#'
sits_al_egal <- function(samples_tb,
                         data_cube,
                         sim_method = "correlation",
                         alpha = NULL,
                         beta = alpha,
                         w = 0.5,
                         n_samples = 1000,
                         multicores = 2) {

    sits:::.sits_tibble_test(samples_tb)

    assertthat::assert_that(
        !purrr::is_null(data_cube),
        msg = "sits_al_egal: please provide data cube"
    )

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

    # Get new samples and merge them to the original sits tibble.
    points_tb <- .sits_get_random_points(data_cube, n_samples, multicores)
    dataset_tb <- dplyr::bind_rows(samples_tb, points_tb)

    # Compute similarity.
    time_series <- as.matrix(sits:::.sits_distances(dataset_tb)[,-2:0])
    similarity_mt <- as.matrix(proxy::simil(time_series,
                                            method = sim_method))

    # Compute alpha
    sim_diag <- similarity_mt
    sim_diag[lower.tri(sim_diag, diag = TRUE)] <- NA
    if (any(is.infinite(sim_diag), na.rm = TRUE))
        stop("Duplicated samples found!")
    if (is.null(alpha)) {
        alpha <- mean(sim_diag, na.rm = TRUE) - 0.5 * stats::sd(sim_diag,
                                                                na.rm = TRUE)
    }

    # Compute density
    N_mat <- similarity_mt
    diag(N_mat) <- NA
    N_mat[N_mat < alpha] <- NA
    density_vec <- rowSums(N_mat, na.rm = TRUE)
    stopifnot(length(density_vec) == nrow(dataset_tb))

    # Compute diversity. NOTE: In L_mat, the rows are the dataset D and the
    # columns are the labeled samples L
    L_mat <- similarity_mt[, 1:nrow(samples_tb)]
    diag(L_mat) <- NA
    diversity_vec <- 1 / apply(L_mat, MARGIN = 1, FUN = max, na.rm = TRUE)
    stopifnot(length(density_vec) == length(diversity_vec))

    # Build the candidate set. NOTE: In CS_mat, the rows are the Unlabeled
    # samples and the columns are the Labeled samples.
    CS_mat <- similarity_mt
    CS_mat <- CS_mat[(nrow(samples_tb) + 1):nrow(CS_mat), 1:nrow(samples_tb)]

    # Update beta
    if (is.null(beta)) {
        beta = .sits_update_beta(CS_mat, w)
    }

    cs_vec <- apply(CS_mat, MARGIN = 1, FUN = function(x, beta){
        x[x > beta] <- NA
        if (all(is.na(x)))
            return(0)
        return(max(x, na.rm = TRUE))
    }, beta = beta)
    names(cs_vec) <- NULL

    points_tb["egal"] <- cs_vec
    points_tb <- points_tb[order(points_tb[["egal"]], decreasing = TRUE), ]

    # postcondition: We return a valid sits with additional columns.
    sits:::.sits_tibble_test(points_tb)

    return(points_tb)
}


#' @title Compute a new value for the beta parameter.
#'
#' @name .sits_update_beta
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
.sits_update_beta <- function(S_mat, w){
    S <- apply(S_mat, MARGIN = 1, FUN = max)
    threshold <- floor(w * length(S))
    return(sort(S)[[threshold]])
}
