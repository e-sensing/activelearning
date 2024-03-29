#---- Util ----

#' Build a matrix representing a graph with one path from the first to the
#' last vertex stopping at each vertex.
#'
#' @param n_diag   Integer. Number of elements in the matrix's diagonal.
#' @param e_weight Double. The weight of each edge.
one_path_mt <- function(n_diag, e_weight) {

    stopifnot(length(n_diag) == 1)
    stopifnot(length(e_weight) == n_diag)

    one_path <- matrix(0, nrow = n_diag, ncol = n_diag)
    for (i in 2:nrow(one_path)) {
        one_path[i, i - 1] <- e_weight[i]
    }
    return(one_path)
}



#---- Tests -----



test_that("Test input samples", {

    samples_tb <- sits::sits_select(sits::samples_modis_4bands,
                                    bands = "EVI")

    all_unlabelled <- samples_tb %>%
        dplyr::mutate(label = NA)

    # The sample set must contain both labelled and unlabelled samples.
    expect_error(al_s2(samples_tb))
    expect_error(al_s2(all_unlabelled))

})



test_that("Test matrix representation of graph", {

    samples_tb <- sits::sits_select(sits::samples_modis_4bands,
                                    bands = "EVI") %>%
        dplyr::slice(1:4)

    G_mt <- .al_s2_bild_closest_vertex_graph(samples_tb,
                                             sim_method = "Euclidean",
                                             closest_n = 2)

    # The matrix is squared.
    expect_equal(nrow(G_mt), ncol(G_mt))

    # The matrix is symmetric.
    expect_true(isSymmetric(G_mt))

})



test_that("Remove missmatching edges: Remove nothing", {

    # No mismatching labels in data_set.

    samples_tb <- sits::samples_modis_4bands %>%
        sits::sits_select(bands = "EVI") %>%
        dplyr::slice(1:4) %>%
        dplyr::mutate(sample_id = dplyr::row_number(),
                      label = dplyr::if_else(sample_id < 3,
                                             "Pasture",
                                             NA_character_))

    G_mt <- .al_s2_bild_closest_vertex_graph(samples_tb,
                                             sim_method = "Euclidean",
                                             closest_n = 2)

    G <- igraph::graph_from_adjacency_matrix(adjmatrix = G_mt,
                                             mode = "undirected",
                                             weighted = TRUE,
                                             diag = FALSE)
    igraph::V(G)$label <- samples_tb$label

    G <- .al_s2_remove_mismatch_edges(G = G)

    new_G_mt <- as.matrix(G[])

    expect_true(all(dim(G_mt) == dim(new_G_mt)))
    expect_true(isSymmetric(G_mt))
    expect_equal(is.na(G_mt), is.na(as.matrix(G[])))
    expect_equal(sum(G_mt, na.rm = TRUE),
                 sum(as.matrix(G[]), na.rm = TRUE))

})



test_that("Remove missmatching edges: one label", {

    # one pair of mismatching label in data_set.

    samples_tb <- sits::samples_modis_4bands %>%
        sits::sits_select(bands = "EVI") %>%
        dplyr::slice(1:4) %>%
        dplyr::mutate(sample_id = dplyr::row_number(),
                      label = dplyr::if_else(sample_id < 3,
                                             "Pasture",
                                             NA_character_)) %>%
        dplyr::mutate(label = dplyr::if_else(sample_id == 1,
                                             "Forest",
                                             label))

    G_mt <- .al_s2_bild_closest_vertex_graph(samples_tb,
                                             sim_method = "Euclidean",
                                             closest_n = 2)

    G <- igraph::graph_from_adjacency_matrix(adjmatrix = G_mt,
                                             mode = "undirected",
                                             weighted = TRUE,
                                             diag = FALSE)
    igraph::V(G)$label <- samples_tb$label

    G <- .al_s2_remove_mismatch_edges(G = G)

    new_G_mt <- as.matrix(G[])

    expect_true(all(dim(G_mt) == dim(new_G_mt)))
    expect_true(!is.na(G_mt[2, 1]))
    expect_true(!is.na(new_G_mt[2, 1]))

})



test_that("Test shortest paths: one path", {

    # Only one path in matrix, from the first label to the last.
    # All the edges have the same weight.

    G_mt <- one_path_mt(n_diag = 5,
                        e_weight = rep(10, times = 5))
    G <- igraph::graph_from_adjacency_matrix(adjmatrix = G_mt,
                                             mode = "undirected",
                                             weighted = TRUE,
                                             diag = FALSE)

    L_x <- 1:nrow(G_mt)
    res <- lapply(seq_along(L_x),
                  FUN = function(x, L_x, G_mt) {
                      .al_s2_short_paths(from_vertex = L_x[x],
                                         to_vertices = L_x,
                                         G = G)
                  },
                  L_x = L_x,
                  G = G)

    s_length_mt <- t(sapply(seq_along(res),
                          FUN = function(x) {
                              return(res[[x]][["path_lengths"]])
                          }))

    m_points <- t(sapply(seq_along(res),
                       FUN = function(x) {
                           return(res[[x]][["mid_point_ids"]])
                       }))

    # Test the properties of the shortest length matrix.
    expect_true(is.matrix(s_length_mt))
    expect_true(isSymmetric(s_length_mt))
    expect_true(nrow(s_length_mt) == ncol(s_length_mt))
    expect_true(nrow(s_length_mt) == length(L_x))

    # Test the lengths.
    expect_equal(s_length_mt,
                 matrix(c(0,  10, 20, 30, 40,
                          10, 0,  10, 20, 30,
                          20, 10, 0,  10, 20,
                          30, 20, 10, 0,  10,
                          40, 30, 20, 10, 0),
                        nrow = 5,
                        byrow = TRUE))

    # Test the properties of the midpoint matrix.
    expect_true(is.matrix(m_points))
    expect_equal(diag(m_points), 1:nrow(m_points))
    expect_true(nrow(m_points) == ncol(m_points))
    expect_true(nrow(m_points) == length(L_x))

    # Test the midpoints.
    expect_equal(m_points,
                 matrix(c(1, 1, 2, 2, 3,
                          2, 2, 2, 3, 3,
                          2, 3, 3, 3, 4,
                          3, 3, 4, 4, 4,
                          3, 4, 4, 5, 5),
                        nrow = 5,
                        byrow = TRUE))

})



test_that("Test shortest paths: Two paths, no change", {

    # A matrix with two paths from the first to the last vertex:
    # - One goes through all the vertexes (shortest path).
    # - One goes striaght from the first to the last vertex (longest path).
    G_mt <- one_path_mt(n_diag = 5,
                        e_weight = rep(10, times = 5))
    G_mt[5, 1] <- 100

    G <- igraph::graph_from_adjacency_matrix(adjmatrix = G_mt,
                                             mode = "undirected",
                                             weighted = TRUE,
                                             diag = FALSE)

    L_x <- 1:nrow(G_mt)

    res <- lapply(seq_along(L_x),
                  FUN = function(x, L_x, G_mt) {
                      .al_s2_short_paths(from_vertex = L_x[x],
                                         to_vertices = L_x,
                                         G = G)
                  },
                  L_x = L_x,
                  G = G)

    s_length_mt <- t(sapply(seq_along(res),
                          FUN = function(x) {
                              return(res[[x]][["path_lengths"]])
                          }))

    m_points <- t(sapply(seq_along(res),
                       FUN = function(x) {
                           return(res[[x]][["mid_point_ids"]])
                       }))

    expect_equal(s_length_mt,
                 matrix(c(0,  10, 20, 30, 40,
                          10, 0,  10, 20, 30,
                          20, 10, 0,  10, 20,
                          30, 20, 10, 0,  10,
                          40, 30, 20, 10, 0),
                        nrow = 5,
                        byrow = TRUE))
    expect_equal(m_points,
                 matrix(c(1, 1, 2, 2, 3,
                          2, 2, 2, 3, 3,
                          2, 3, 3, 3, 4,
                          3, 3, 4, 4, 4,
                          3, 4, 4, 5, 5),
                        nrow = 5,
                        byrow = TRUE))

})



test_that("Test shortest paths: Two paths, change", {

    # A matrix with two paths from the first to the last vertex:
    # - One goes through all the vertexes (longest path).
    # - One goes striaght from the first to the last vertex (shortest path).

    G_mt <- one_path_mt(n_diag = 5,
                        e_weight = rep(10, times = 5))
    G_mt[5, 1] <- 1

    G <- igraph::graph_from_adjacency_matrix(adjmatrix = G_mt,
                                             mode = "undirected",
                                             weighted = TRUE,
                                             diag = FALSE)

    L_x <- 1:nrow(G_mt)

    res <- lapply(seq_along(L_x),
                  FUN = function(x, L_x, G_mt) {
                      .al_s2_short_paths(from_vertex = L_x[x],
                                         to_vertices = L_x,
                                         G= G)
                  },
                  L_x = L_x,
                  G = G)

    s_length_mt <- t(sapply(seq_along(res),
                          FUN = function(x) {
                              return(res[[x]][["path_lengths"]])
                          }))

    m_points <- t(sapply(seq_along(res),
                       FUN = function(x) {
                           return(res[[x]][["mid_point_ids"]])
                       }))

    expect_equal(s_length_mt,
                 matrix(c(0,  10, 20, 11, 1,
                          10, 0,  10, 20, 11,
                          20, 10, 0,  10, 20,
                          11, 20, 10, 0,  10,
                          1,  11, 20, 10, 0),
                        nrow = 5,
                        byrow = TRUE))

    expect_equal(m_points,
                 matrix(c(1, 1, 2, 5, 1,
                          2, 2, 2, 3, 1,
                          2, 3, 3, 3, 4,
                          5, 3, 4, 4, 4,
                          5, 1, 4, 5, 5),
                        nrow = 5,
                        byrow = TRUE))

})



test_that("MSSP: one path, different labels", {

    # A matrix with one path from the first to the last vertex.
    # Two distinct labels.
    G_mt <- one_path_mt(n_diag = 5,
                        e_weight = rep(10, times = 5))

    G <- igraph::graph_from_adjacency_matrix(adjmatrix = G_mt,
                                             mode = "undirected",
                                             weighted = TRUE,
                                             diag = FALSE)

    L_fx <- paste("label", 1:nrow(G_mt))
    L_fx[2:(length(L_fx) - 1)] <- NA
    L <- lapply(seq_along(L_fx),
                FUN = function(x, L_fx) {
                    list(x = x, f_x = L_fx[x])
                },
                L_fx = L_fx)
    mid_points <- .al_s2_mssp(G = G,
                              L = L)
    expect_true(length(mid_points) == length(L_fx))
    expect_equal(mid_points,
                 c(3, NA, NA, NA, 3))

})



test_that("MSSP: one path, repeated labels", {

    # A matrix with one path from the first to the last vertex.
    # Two identical labels.

    G_mt <- one_path_mt(n_diag = 5,
                        e_weight = rep(10, times = 5))

    G <- igraph::graph_from_adjacency_matrix(adjmatrix = G_mt,
                                             mode = "undirected",
                                             weighted = TRUE,
                                             diag = FALSE)
    L_fx <- rep("label 1", times = nrow(G_mt))
    L_fx[2:(length(L_fx) - 1)] <- NA
    L <- lapply(seq_along(L_fx),
                FUN = function(x, L_fx) {
                    list(x = x, f_x = L_fx[x])
                },
                L_fx = L_fx)
    mid_points <- .al_s2_mssp(G = G,
                              L = L)

    expect_true(length(mid_points) == length(L_fx))
    expect_equal(mid_points,
                 rep(NA_integer_, times = length(L_fx)))

})



test_that("MSSP: two paths, no change", {

    # A matrix with two paths from the first to the last vertex:
    # - One goes through all the vertexes (shortest path).
    # - One goes striaght from the first to the last vertex (longest path).

    G_mt <- one_path_mt(n_diag = 5,
                        e_weight = rep(10, times = 5))
    G_mt[5, 1] <- 100

    G <- igraph::graph_from_adjacency_matrix(adjmatrix = G_mt,
                                             mode = "undirected",
                                             weighted = TRUE,
                                             diag = FALSE)

    L_fx <- paste("label", 1:nrow(G_mt))
    L_fx[2:(length(L_fx) - 1)] <- NA
    L <- lapply(seq_along(L_fx),
                FUN = function(x, L_fx) {
                    list(x = x, f_x = L_fx[x])
                },
                L_fx = L_fx)
    mid_points <- .al_s2_mssp(G = G,
                              L = L)
    expect_true(length(mid_points) == length(L_fx))
    expect_equal(mid_points,
                 c(3, NA, NA, NA, 3))

})



test_that("MSSP: two paths, no change", {

    # The alternative path is shorter.
    G_mt <- one_path_mt(n_diag = 5,
                        e_weight = rep(10, times = 5))
    G_mt[5, 1] <- 10

    G <- igraph::graph_from_adjacency_matrix(adjmatrix = G_mt,
                                             mode = "undirected",
                                             weighted = TRUE,
                                             diag = FALSE)


    L_fx <- paste("label", 1:nrow(G_mt))
    L_fx[2:(length(L_fx) - 1)] <- NA
    L <- lapply(seq_along(L_fx),
                FUN = function(x, L_fx) {
                    list(x = x, f_x = L_fx[x])
                },
                L_fx = L_fx)
    mid_points <- .al_s2_mssp(G = G,
                              L = L)
    expect_true(length(mid_points) == length(L_fx))
    expect_equal(mid_points,
                 c(1, NA, NA, NA, 5))

})



test_that("Test expected usage", {

    samples_tb <- sits::sits_select(sits::samples_modis_4bands,
                                    bands = "EVI") %>%
        dplyr::mutate(sample_id = 1:nrow(.))

    labelled_tb <- samples_tb %>%
        dplyr::group_by(label) %>%
        dplyr::sample_n(5) %>%
        dplyr::ungroup()

    unlabelled_tb <- samples_tb %>%
        dplyr::filter(!(sample_id %in% labelled_tb$sample_id)) %>%
        dplyr::group_by(label) %>%
        dplyr::sample_n(5) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(label = NA)

    samples_tb <- labelled_tb %>%
        dplyr::bind_rows(unlabelled_tb) %>%
        dplyr::select(-sample_id)

    label_vec <- sort(unique(samples_tb[["label"]]))

    expect_error(al_s2(samples_tb,
                       sim_method = "correlation",
                       closest_n = 6,
                       mode = "make_up_mode"))

    res_al <- al_s2(samples_tb,
                    sim_method = "correlation",
                    closest_n = 6,
                    mode = "active_learning")

    expect_true(nrow(res_al) == nrow(samples_tb))
    expect_true("s2" %in% colnames(res_al))
    expect_true(all(res_al[["s2"]] %in% c(0, 1, NA)))

    res_ssl <- al_s2(samples_tb,
                     sim_method = "correlation",
                     closest_n = 6,
                     mode = "semi_supervised_learning")

    expect_true(nrow(res_ssl) == nrow(samples_tb))
    expect_true(sum(is.na(res_ssl[["label"]])) == 0)
    expect_true(all(res_ssl[["label"]] %in% label_vec))

    labelled_samples <- samples_tb[["label"]]
    labelled_samples <- labelled_samples[!is.na(labelled_samples)]

    expect_true(
        all(res_ssl[["label"]][1:length(labelled_samples)] == labelled_samples)
    )

})

