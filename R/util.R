#' @title Get a sits tibble of random points in the given data cube.
#'
#' @name .sits_get_random_points
#'
#' @keywords internal
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Get a sits tibble of random points in the given data cube.
#'
#' @param data_cube  A sits data cube.
#' @param n_samples  The number of random points to take. The returned
#'                   sample size may be less than the requested size.
#' @param multicores The number of cores available for active learning.
#'
#' @return           A sits tibble of samples, including their time series.
#'
.sits_get_random_points <- function(data_cube, n_samples, multicores){

    # Get the extent of the data cube.
    xmin <- data_cube[["xmin"]]
    xmax <- data_cube[["xmax"]]
    ymin <- data_cube[["ymin"]]
    ymax <- data_cube[["ymax"]]
    pol <- sf::st_sfc(sf::st_polygon(list(rbind(c(xmin, ymin), c(xmax, ymin),
                                                c(xmax, ymax), c(xmin, ymax),
                                                c(xmin, ymin)))))
    sf::st_crs(pol) <- data_cube[["crs"]]

    # Get n_samples random points (plus a margin) in the data_cube's extent.
    # NOTE: the extra points would help if case st_sample returns less points or
    # if some of them have empty time series.
    points_tb <- sf::st_sf(sf::st_sample(pol,
                                         size = n_samples,
                                         type = "random"))
    points_tb <- sf::st_transform(points_tb,
                                  crs = 4326)
    points_tb <- cbind(points_tb, sf::st_coordinates(points_tb))
    colnames(points_tb) <- c("longitude", "latitude", "geometry")
    sf::st_geometry(points_tb) <- "geometry"
    points_tb <- sf::st_set_geometry(points_tb, NULL)
    time_line <- sits::sits_timeline(data_cube)
    points_tb["start_date"]  <- time_line[1]
    points_tb["end_date"]    <- time_line[length(time_line)]
    points_tb["label"]       <- "NoClass"
    points_tb["cube"]        <- NA
    points_tb["time_series"] <- NA
    points_tb["id"] <- seq_len(nrow(points_tb))
    class(points_tb) <- c(class(points_tb), "sits")

    # Get the points' time series.
    tmp_file <- tempfile(pattern = "points_",
                         fileext = ".csv")
    utils::write.csv(points_tb, file = tmp_file, row.names = FALSE)
    points_tb <- sits::sits_get_data(cube = data_cube,
                               file = tmp_file,
                               multicores = multicores)

    # Remove samples with NAs in their time series.
    points_tb[["have_na"]] <- vapply(points_tb[["time_series"]], function(x){
        any(is.na(x))
    }, logical(1))
    points_tb <- points_tb[points_tb[["have_na"]] == FALSE, ]
    points_tb[["have_na"]] <- NULL

    return(points_tb)
}
