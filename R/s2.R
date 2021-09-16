#' @title Implementation of Shortest Shortest Path (S2) algorithm
#'
#' @name al_s2
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Semi-Supervised Learning is a set of classification methods that
#' use both labelled and unlabelled samples. The Shortest Shortest Path (S2)
#' algorithm uses a undirected graph of the samples and iteratively removes
#' edges, trying to identify the boundaries of each class in the graph.
#'
#' This function receives a sits tibble with time series samples and it returns
#' the label of each sample
#'
#' @references Dasarathy, G., Nowak, R., & Zhu, X. (2015). S2: An efficient
#' graph based active learning algorithm with application to nonparametric
#' classification. Journal of Machine Learning Research, 40(2015), 1–20.
#'
#' @param samples_tb      A sits tibble with both labelled and unlabelled
#'                        samples (i.e. NA).
#' @param sim_method      A character. A method for computing the similarity
#'                        among samples. See proxy::simil for details.
#' @param keep_n          An integer. The number of most similar samples to
#'                        keep while building the graph.
#' @param budget          An integer. It controls the maximum number of queries
#'                        of the S2 algorithm to complete a learning task.
#' @return                A sits tibble with updated labels.
#'
#' @export
#'
al_s2 <- function(samples_tb,
                  sim_method,
                  keep_n) {

    sits:::.sits_test_tibble(samples_tb)

    label_tb <- samples_tb %>%
        dplyr::filter(is.na(label) == FALSE,
                      nchar(label) > 0,
                      label != "NoClass")

    no_label_tb <- samples_tb %>%
        dplyr::filter(is.na(label) | label == "" | label == "NoClass")

    stopifnot(nrow(samples_tb) == nrow(label_tb) + nrow(no_label_tb))

    assertthat::assert_that(
        nrow(label_tb) > 0,
        msg = "al_egal: please provide some labelled samples"
    )

    assertthat::assert_that(
        nrow(no_label_tb) > 0,
        msg = "al_egal: please provide some unlabelled samples"
    )

    points_tb <- .al_s2(s_labelled_tb = label_tb,
                        s_unlabelled_tb = no_label_tb,
                        budget = budget,
                        keep_n = keep_n)

    sits:::.sits_test_tibble(points_tb)

    return(points_tb)
}



#' @title Implementation of Shortest Shortest Path (S2) algorithm
#'
#' @name .al_s2
#'
#' @keywords internal
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description The S2 algorithm uses a undirected graph of the samples and
#' iteratively removes edges, trying to identify the boundaries of each class
#' in the graph.
#'
#' @param s_labelled_tb   A sits tibble with labelled samples.
#' @param s_unlabelled_tb A sits tibble with unlabelled samples.
#' @param sim_method      A character. A method for computing the similarity
#'                        among samples. See proxy::simil for details.
#' @param keep_n          An integer. The number of most similar samples to
#'                        keep while building the graph.
#' @return                A sits tibble with updated labels.
#'
.al_s2 <- function(s_labelled_tb,
                   s_unlabelled_tb,
                   sim_method,
                   keep_n) {




    #---- Algorithm S2 ----

    # NOTE:
    # G is an undirected graph (V, E).
    # V is a vertex set.
    # E is an edge set.
    # f(v) is the label of v which belongs to V.
    # L is the subset of labelled samples in V.
    # x a random unlabelled sample

    # budget <- nrow(s_labelled_tb)
    #
    # G_mt <- .al_s2_build_graph_mt(
    #     samples_tb = dplyr::bind_rows(s_labelled_tb, s_unlabelled_tb),
    #     sim_method = sim_method,
    #     keep_n = keep_n
    # )
    #
    # L  <- list()
    #
    # # While 1 do
    # labelled_id <- seq_len(nrow(s_labelled_tb))
    # while (sum(is.na(labelled_id)) < length(labelled_id)) {
    #
    #     # Randomly choose unlabeled vertex.
    #     x <- sample(labelled_id,
    #                 size = 1,
    #                 replace = FALSE)
    #     labelled_id[which(labelled_id == x)] <- NA
    #
    #     f_x <- s_labelled_tb[["label"]][x]
    #     L[[length(L) + 1]] <- list(x = x, f_x = f_x)
    #
    #     # NOTE: An edge requires at least 2 vertex.
    #     if (length(L) == 1)
    #         next()
    #
    #     while (TRUE) {
    #
    #         # Remove from G all edges whose two ends have different labels.
    #         G_mt <- .al_s2_remove_mismatch_edges(G_mt,
    #                                              dataset_tb = dplyr::bind_rows(
    #                                                  s_labelled_tb,
    #                                                  s_unlabelled_tb
    #                                              ))
    #
    #         if (length(L) >= budget)
    #             return(.sits_label_completion(G, L))
    #
    #         x <- .al_s2_mssp(G = G_mt,
    #                          L = L)
    #
    #     }
    # }
}




.al_s2_mssp <- function(G, L) {

    # NOTE: This breaks down when length(L) == 1
    # TODO: here I go!!!

    # G_gh <- igraph::graph_from_adjacency_matrix(adjmatrix = G,
    #                                             mode = "undirected",
    #                                             weighted = TRUE,
    #                                             diag = FALSE)
    #
    # L_x  <- unlist(as.data.frame(do.call(rbind, L))[["x"]])
    # L_fx <- unlist(as.data.frame(do.call(rbind, L))[["f_x"]])
    #
    # for (v_from in L_x) {
    #     # TODO: Exclude edges with the same label at both ends.
    #     short_path <- igraph::shortest_paths(graph = G_gh,
    #                                          from = v_from,
    #                                          to = L_vec[which(L_vec != v_from)],
    #                                          output = "both")
    #     short_path[["vpath"]][[1]]
    #     short_path[["epath"]][[1]]
    # }
}

.al_s2_label_completion <- function(G, L){
    TRUE
}


#' @title Remove mismatching edged from a matrix graph.
#'
#' @name .al_s2_remove_mismatch_edges
#'
#' @keywords internal
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description This function takes a matrix representing a graph and removes
#' the edges with different labels at their extremes.
#'
#' @param G          A matrix representing a graph (see .al_s2_build_graph_mt).
#' @param dataset_tb A sits tibble.
#' @return           A matrix representation of a graph with potentially less
#'                   edges than G.
#'
.al_s2_remove_mismatch_edges <- function(G, dataset_tb){

    dataset_tb["sample_id"] <- seq_len(nrow(dataset_tb))

    # Remove from G all edges which two ends have different labels
    mismatch_ls <- lapply(1:nrow(G),
                          FUN = function(sample_id, G, dataset_tb){

                              this_label <- dataset_tb[["label"]][sample_id]

                              if (is.na(this_label) || this_label == "")
                                  return(integer())

                              this_row <- G[sample_id, ]

                              closest_ids <- which(!is.na(this_row))

                              closest_labels <-
                                  dataset_tb[["label"]][closest_ids]

                              mismatch_ids <-
                                  closest_ids[this_label != closest_labels]

                              return(mismatch_ids)
                          },
                          G = G,
                          dataset_tb = dataset_tb)

    stopifnot(length(mismatch_ls) == nrow(dataset_tb))

    for (i in seq_along(mismatch_ls)) {

        mismatch_vec <- mismatch_ls[[i]]

        if (length(mismatch_vec) == 0)
            next()

        G[i, mismatch_vec] <- NA
    }

    return(G)
}





#' @title Build a matrix representing a graph
#'
#' @name .al_s2_build_graph_mt
#'
#' @keywords internal
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description This function takes a sits tibble and returns a matrix
#' representation of a similarity graph.
#'
#' @param samples_tb      A sits tibble with both labelled and unlabelled
#'                        samples (i.e. NA).
#' @param sim_method      A character. A method for computing the similarity
#'                        among samples. See proxy::simil for details.
#' @param keep_n          An integer. The number of most similar samples to
#'                        keep while building the graph.
#' @return                A lower triangular matrix. The matrix's upper
#'                        triangular part along its principal diagonal are set
#'                        to NA. The rows in the matrix's lower triangular part
#'                        are non-NA for the keep_n elements.
#'
.al_s2_build_graph_mt <- function(samples_tb,
                                  sim_method = "Euclidean",
                                  keep_n = 10){

    time_series <- as.matrix(sits:::.sits_distances(samples_tb)[,-2:0])
    dist_mt <- as.matrix(proxy::dist(time_series,
                                     method = sim_method))
    dist_mt[upper.tri(dist_mt)] <- NA
    diag(dist_mt) <- NA
    dist_mt <- t(apply(dist_mt,
                       MARGIN = 1,
                       FUN = function(x, n_closest){
                           top <- head(sort(x),
                                       n_closest)
                           x[which(!(x %in% top))] <- NA
                           return(x)
                       },
                       n_closest = keep_n))
    return(dist_mt)
}
