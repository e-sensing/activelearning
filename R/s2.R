#' @title Implementation of Shortest Shortest Path (S2) algorithm
#'
#' @name al_s2
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Semi-Supervised Learning is a set of classification methods
#' that use both labelled and unlabelled samples. The Shortest Shortest Path
#' (S2) algorithm uses a undirected graph of the samples and iteratively
#' removes edges, trying to identify the boundaries of each class in the graph.
#'
#' This function receives a sits tibble with time series samples and it returns
#' a sits tibble with either the unlabelled samples to be sent to the oracle
#' or the label of each sample.
#'
#' @references Dasarathy, G., Nowak, R., & Zhu, X. (2015). S2: An efficient
#' graph based active learning algorithm with application to nonparametric
#' classification. Journal of Machine Learning Research, 40(2015), 1–20.
#'
#' @param samples_tb      A sits tibble with both labelled and unlabelled
#'                        samples (i.e. NA).
#' @param sim_method      A character. A method for computing the similarity
#'                        among samples. See proxy::simil for details.
#' @param closest_n       An integer. The number of most similar samples to
#'                        keep while building a similarity graph of the
#'                        samples.
#' @param mode            A character telling if the functin runs on either the
#'                        "active_learning" or "semi_supervised_learning" mode.
#' @return                A sits tibble with either the samples to be sent to
#'                        the oracle (mode "semi_supervised_learning", column
#                         s2 == 1) or the label column updated.
#'
#' @importFrom rlang .data
#'
#' @export
#'
al_s2 <- function(samples_tb,
                  sim_method = "correlation",
                  closest_n = 6,
                  mode = "active_learning") {

    sits:::.sits_tibble_test(samples_tb)

    label_tb <- samples_tb %>%
        dplyr::filter(is.na(.data$label) == FALSE,
                      nchar(.data$label) > 0,
                      .data$label != "NoClass")

    no_label_tb <- samples_tb %>%
        dplyr::filter(is.na(.data$label) |
                      .data$label == ""  |
                      .data$label == "NoClass")

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
                        closest_n = closest_n,
                        mode = mode)

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
#' @param closest_n       An integer. The number of most similar samples to
#'                        keep while building the graph.
#' @param mode            A character telling if the functin runs on either the
#'                        "active_learning" or "semi_supervised_learning" mode.
#' @return                A sits tibble with either the samples to be sent to
#'                        the oracle (mode "semi_supervised_learning", column
#                         s2 == 1) or the label column updated.
#'
.al_s2 <- function(s_labelled_tb,
                   s_unlabelled_tb,
                   sim_method,
                   closest_n,
                   mode) {

    stopifnot(mode %in% c("active_learning",
                          "semi_supervised_learning"))

    # NOTE:
    # G is an undirected graph (V, E).
    # V is a vertex set.
    # E is an edge set.
    # f(v) is the label of v which belongs to V.
    # L is the subset of labelled samples in V.
    # x a random unlabelled sample

    samples_tb <- rbind(s_labelled_tb, s_unlabelled_tb)

    # Build a graph.
    G_mt <- .al_s2_bild_closest_vertex_graph(samples_tb = samples_tb,
                                             sim_method = sim_method,
                                             closest_n = closest_n)
    G <- igraph::graph_from_adjacency_matrix(adjmatrix = G_mt,
                                             mode = "undirected",
                                             weighted = TRUE,
                                             diag = FALSE)
    igraph::V(G)$label <- samples_tb$label

    G <- .al_s2_remove_mismatch_edges(G)

    # Build L, a subset of labelled samples in V (scrambled).
    L <- list()
    labelled_id <- sample(seq_len(nrow(s_labelled_tb)))
    for (x in labelled_id) {
        f_x <- s_labelled_tb[["label"]][x]
        L[[length(L) + 1]] <- list(x = x, f_x = f_x)
    }

    if (mode == "active_learning") {
        midpoints <- .al_s2_mssp(G = G, L = L)
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

    # Propagate the labels in G.
    # TODO: test igraph::min_cut and other clustering functions in
    #      igraph::cluster_label_prop's help.
    label_vec <- sort(unique(igraph::V(G)$label))
    initial_vec <- as.integer(factor(igraph::V(G)$label,
                                     levels = label_vec))
    initial_vec[is.na(initial_vec)] <- -1
    community <-  igraph::cluster_label_prop(G,
                                             initial = initial_vec,
                                             fixed = initial_vec > 0)
    new_label <- igraph::membership(community)
    new_label <- label_vec[new_label]
    stopifnot(length(new_label) == nrow(samples_tb))

    samples_tb["label"] <- new_label

    return(samples_tb)

}



#' @title Remove mismatching edges from a graph.
#'
#' @name .al_s2_remove_mismatch_edges
#'
#' @keywords internal
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description This function takes a graph and removes the edges with
#' different labels at their extremes.
#'
#' @param G       An igraph object whose veteces have a label attribute.
#' @return        A graph with the same or less edges than G.
#'
.al_s2_remove_mismatch_edges <- function(G) {

    labels <- igraph::V(G)$label

    same_label <- expand.grid(lab_1 = labels,
                              lab_2 = labels,
                              stringsAsFactors = FALSE)
    same_label <- cbind(same_label,
                        expand.grid(id_1 = 1:length(labels),
                                    id_2 = 1:length(labels)))
    same_label["same"] <- same_label[[1]] == same_label[[2]]
    for(i in seq_len(nrow(same_label))){
        if (same_label[i, "id_1"] < same_label[i, "id_2"])
            next()
        if (any(is.na(c(same_label[i, "lab_1"], same_label[i, "lab_2"]))))
            next()
        if (same_label[i, "same"])
            next()
        if (G[same_label[i, "id_1"], same_label[i, "id_2"]] > 0)
            G[same_label[i, "id_1"], same_label[i, "id_2"]] <- 0
    }

    return(G)
}



#' @title Compute the MSSP subroutine.
#'
#' @name .al_s2_mssp
#'
#' @keywords internal
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description This function takes a graph and a set of labelled vertexes and
#' computes the length of the shortest paths and the points along them.
#'
#' @param G    A igraph object.
#' @param L    A list of lists containing the ids of the vertexes in G_mt and
#'             their labels.
#' @return     A vector. The vertex ids of the mid-points along the shortest
#'             paths between the elements of L.
#'
.al_s2_mssp <- function(G, L) {

    L_x  <- unlist(as.data.frame(do.call(rbind, L))[["x"]])
    L_fx <- unlist(as.data.frame(do.call(rbind, L))[["f_x"]])
    stopifnot(length(L_x) == length(L_fx))

    path_lt <- lapply(L_x,
                      FUN = .al_s2_short_paths,
                      to_vertices = L_x,
                      G = G)

    s_lengths <- sapply(path_lt,
                        FUN = function(x) {
                            return(x[["path_lengths"]])
                        })
    stopifnot(length(L_x) == nrow(s_lengths))
    stopifnot(nrow(s_lengths) == ncol(s_lengths))

    # NOTE: Read mid_points by row.
    mid_points <- t(sapply(path_lt,
                         FUN = function(x) {
                             return(x[["mid_point_ids"]])
                         }))
    stopifnot(length(L_x) == nrow(mid_points))
    stopifnot(nrow(mid_points) == ncol(mid_points))

    # Exclude paths with the same label at both ends.
    same_label <- expand.grid(L_fx, L_fx, stringsAsFactors = FALSE)
    same_label <- matrix(data = same_label[[1]] == same_label[[2]],
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
#' @param fom_vertex  A single integer. The index of a vertex in G.
#' @param to_vertices An integer. The indices of vertices in G.
#' @param G           An igraph object.
#' @return            A list of two: The shortest paths' lengths, and
#'                    the vertex id of their mid points.
#'
.al_s2_short_paths <- function(from_vertex, to_vertices, G) {

    s_paths <- igraph::get.shortest.paths(graph = G,
                                          from = from_vertex,
                                          to = to_vertices,
                                          output = "vpath")

    # Path from vertex to each other vertex.
    paths <- s_paths[["vpath"]]

    # Length from vertex to each other vertex.
    lengths <- vapply(seq_along(paths),
                      FUN = function(x, paths) {
                          sum(igraph::E(G, path = paths[[x]])$weight)
                      },
                      FUN.VALUE = double(1),
                      paths = paths)

    # Compute the midpoints. Choose the middle vertex.
    mid_point_ids <- vapply(paths,
                            FUN = function(path) {
                                x <- as.vector(unlist(path))
                                return(x[stats::median(1:length(x))])
                            },
                            FUN.VALUE = integer(1))

    return(list(path_lengths = lengths,
                mid_point_ids = mid_point_ids))
}



#' @title Build a matrix representing a graph made of the closest verteces.
#'
#' @name .al_s2_bild_closest_vertex_graph
#'
#' @keywords internal
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description This function takes a sits tibble and returns a matrix
#' representation of a similarity graph where only the most similar samples
#' are connected.
#'
#' @param samples_tb      A sits tibble with both labelled and unlabelled
#'                        samples (i.e. NA).
#' @param sim_method      A character. A method for computing the similarity
#'                        among samples. See proxy::simil for details.
#' @param closest_n       An integer. The number of most similar samples to
#'                        link in the graph.
#' @return                A lower triangular matrix. The matrix's upper
#'                        triangular part along its principal diagonal are set
#'                        to NA. The rows in the matrix's lower triangular part
#'                        are non-NA for the closest_n elements.
#'
.al_s2_bild_closest_vertex_graph <- function(samples_tb, sim_method,
                                             closest_n) {

    time_series <- as.matrix(sits:::.sits_distances(samples_tb)[,-2:0])
    dist_mt <- as.matrix(proxy::dist(time_series,
                                     method = sim_method))
    dist_mt[upper.tri(dist_mt)] <- NA
    diag(dist_mt) <- NA
    dist_mt <- t(apply(dist_mt,
                       MARGIN = 1,
                       FUN = function(x, n_closest){
                           top <- utils::head(sort(x),
                                              n_closest)
                           x[which(!(x %in% top))] <- 0
                           return(x)
                       },
                   n_closest = closest_n))
    dist_mt[is.na(dist_mt)] <- 0
    dist_mt <- dist_mt + t(dist_mt)

    return(dist_mt)
}
