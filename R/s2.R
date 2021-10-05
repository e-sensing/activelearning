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

    sits:::.sits_tibble_test(samples_tb)

    label_tb <- samples_tb %>%
        dplyr::filter(is.na(label) == FALSE,
                      nchar(label) > 0,
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

    points_tb <- .al_s2(s_labelled_tb = label_tb,
                        s_unlabelled_tb = no_label_tb,
                        sim_method = sim_method,
                        keep_n = keep_n)

    sits:::.sits_tibble_test(points_tb)

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
                   keep_n,
                   mode = "active_learning") {
#TODO: use igraph object the whole time. Check s2_v2.R


    #---- Algorithm S2 ----

    # NOTE:
    # G is an undirected graph (V, E).
    # V is a vertex set.
    # E is an edge set.
    # f(v) is the label of v which belongs to V.
    # L is the subset of labelled samples in V.
    # x a random unlabelled sample

    G_mt <- .al_s2_build_graph_mt(
        samples_tb = rbind(s_labelled_tb, s_unlabelled_tb),
        sim_method = sim_method,
        keep_n = keep_n
    )

    # Remove from G all edges whose two ends have different labels.
    G_mt <- .al_s2_remove_mismatch_edges(
        G_mt,
        dataset_tb = rbind(s_labelled_tb, s_unlabelled_tb)
    )

    L <- list()
    labelled_id <- sample(seq_len(nrow(s_labelled_tb)))
    for (x in labelled_id) {
        f_x <- s_labelled_tb[["label"]][x]
        L[[length(L) + 1]] <- list(x = x, f_x = f_x)
    }


    if (mode == "active_learning") {
        midpoints <- .al_s2_mssp(G_mt = G_mt,
                                 L = L)
        stopifnot(length(midpoints) == nrow(s_labelled_tb))

        # Get the samples of the midpoints.
        # NOTE: 1 represents the samples that sould be sent to the oracle; NA
        # are the training samples, and 0 are the remaining samples.
        samples_tb <- rbind(s_labelled_tb, s_unlabelled_tb)
        samples_tb["s2"] <- 0.0
        samples_tb[["s2"]][midpoints] <- 1.0
        samples_tb[["s2"]][1:nrow(s_labelled_tb)] <- NA_real_

        return(samples_tb)
   }

   # TODO: Do the classification.
   # - test igraph::cluster_label_prop

}



#' @title Compute the MSSP subroutine.
#'
#' @name .al_s2_mssp
#'
#' @keywords internal
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description This function takes a graph (represented as a matrix) and a set
#' of labelled vertexes and computes the length of the shortest paths and the
#' mid-points along them.
#'
#' @param G_mt A matrix representing a graph. See function
#'             .al_s2_build_graph_mt
#' @param L    A list of lists containing the ids of the vertexes in G_mt and
#'             their labels.
#' @return     A vector. The vertex ids of the mid-points along the shortest
#'             paths between the elements of L.
#'
.al_s2_mssp <- function(G_mt, L) {

    L_x  <- unlist(as.data.frame(do.call(rbind, L))[["x"]])
    L_fx <- unlist(as.data.frame(do.call(rbind, L))[["f_x"]])
    stopifnot(length(L_x) == length(L_fx))
    stopifnot(ncol(G_mt) == nrow(G_mt))

    path_lt <- lapply(L_x,
                       FUN = .al_s2_short_paths,
                       to_vertices = L_x,
                       G_mt = G_mt)

    s_lengths <- sapply(path_lt,
                        FUN = function(x) {
                            return(x[["path_lengths"]])
                        })

    # NOTE: Read mid_points by row.
    mid_points <- t(sapply(path_lt,
                         FUN = function(x) {
                             return(x[["mid_point_ids"]])
                         }))

    stopifnot(length(L_x) == nrow(s_lengths))
    stopifnot(length(L_x) == nrow(mid_points))
    stopifnot(nrow(s_lengths) == ncol(s_lengths))
    stopifnot(nrow(mid_points) == ncol(mid_points))

    # Exclude edges with the same label at both ends.
    same_label <- expand.grid(L_fx, L_fx,
                              stringsAsFactors = FALSE)

    same_label  <- matrix(data = same_label[[1]] == same_label[[2]],
                          ncol = length(L_fx),
                          byrow = TRUE)

    s_lengths[same_label] <- NA
    s_lengths[is.na(same_label)] <- NA

    # Select the shortest shortest path.
    ss_ids <- apply(s_lengths,
                    MARGIN = 1,
                    FUN = which.min)

    if (length(ss_ids) == 0)
        ss_ids <- rep(NA_integer_, times = nrow(mid_points))

    stopifnot(length(ss_ids) == nrow(mid_points))

    # Get the midpoints.
    mid_point_vertexes <- vapply(seq_len(nrow(mid_points)),
                                 FUN = function(x, mid_points, ss_ids) {
                                     if (length(ss_ids[[x]]) == 0)
                                         return(NA_integer_)
                                     if (is.na(ss_ids[[x]]))
                                         return(NA_integer_)
                                     return(mid_points[x, ss_ids[[x]]])
                                 },
                                 FUN.VALUE = integer(1),
                                 mid_points = mid_points,
                                 ss_ids = ss_ids)

    stopifnot(length(L_x) == length(mid_point_vertexes))
    return(mid_point_vertexes)
}



#' @title Get the shortest paths between vertices.
#'
#' @name .al_s2_short_paths
#'
#' @keywords internal
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description This function computes the paths from a vertex to a set of
#' vertices in graph.
#'
#' @param fom_vertex  A single integer. The index of a vertex in G_mt.
#' @param to_vertices An integer. The indices of vertices in G_mt.
#' @param G_mt        A matrix representing a graph. See function
#'                    .al_s2_build_graph_mt
#' @return            A list of two: The shortest paths' lengths, and
#'                    the vertex id of their mid points.
#'
.al_s2_short_paths <- function(from_vertex, to_vertices, G_mt) {

    # Replace NAs with 0s; make the matrix whole again.
    G_mt[is.na(G_mt)] <- 0
    G_mt <- G_mt + t(G_mt)

    G_gh <- igraph::graph_from_adjacency_matrix(adjmatrix = G_mt,
                                                mode = "undirected",
                                                weighted = TRUE,
                                                diag = FALSE)

    s_paths <- igraph::get.shortest.paths(graph = G_gh,
                                          from = from_vertex,
                                          to = to_vertices,
                                          output = "vpath")

    # Path from vertex to each other vertex.
    paths <- s_paths[["vpath"]]

    # Length from vertex to each other vertex.
    lengths <- vapply(seq_along(paths),
                      FUN = function(x, paths, G_mt) {
                          sum(igraph::E(G_gh,
                                        path = paths[[x]])$weight)
                      },
                      FUN.VALUE = double(1),
                      paths = paths,
                      G_mt = G_mt)

    # Compute the midpoints.
    # NOTE: The options are:
    # - Choose the middle vertex.
    # - Choose one of the vertices of the longest edge?
    # - Do the cummulative sum of the distances along the path and select one
    #   of the vertices of the edge in the middle. ANSWER: It doesn't make
    #   sense. The cummulative distance along the path is not the same as the
    #   distance from the path's first and last vertices (triangular
    #   inequality).

    # Choose the middle vertex.
    mid_point_ids <- vapply(paths,
                            FUN = function(path) {
                                x <- as.vector(unlist(path))
                                return(x[median(1:length(x))])
                            },
                            FUN.VALUE = integer(1))

    return(list(path_lengths = lengths,
                mid_point_ids = mid_point_ids))
}



.al_s2_label_completion <- function(G, L){
    # TODO: implement!
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
#' @param G_mt       A matrix representing a graph (see .al_s2_build_graph_mt).
#' @param dataset_tb A sits tibble.
#' @return           A matrix representation of a graph with potentially less
#'                   edges than G.
#'
.al_s2_remove_mismatch_edges <- function(G_mt, dataset_tb){

    dataset_tb["sample_id"] <- seq_len(nrow(dataset_tb))

    # Remove from G all edges which two ends have different labels
    mismatch_ls <- lapply(1:nrow(G_mt),
                          FUN = function(sample_id, G_mt, dataset_tb){

                              this_label <- dataset_tb[["label"]][sample_id]

                              if (is.na(this_label) || this_label == "")
                                  return(integer())

                              this_row <- G_mt[sample_id, ]

                              closest_ids <- which(!is.na(this_row))

                              closest_labels <-
                                  dataset_tb[["label"]][closest_ids]

                              mismatch_ids <-
                                  closest_ids[this_label != closest_labels]

                              return(mismatch_ids)
                          },
                          G_mt = G_mt,
                          dataset_tb = dataset_tb)

    stopifnot(length(mismatch_ls) == nrow(dataset_tb))

    for (i in seq_along(mismatch_ls)) {

        mismatch_vec <- mismatch_ls[[i]]

        if (length(mismatch_vec) == 0)
            next()

        G_mt[i, mismatch_vec] <- NA
    }

    return(G_mt)
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
