#' @title Check the time series in a sits tibble.
#'
#' @name .al_check_time_series
#'
#' @keywords internal
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description This function returns TRUE of the time series in the given sits
#' tibble are valid; otherwise it throws an error.
#'
#' @param s_labelled_tb   A sits tibble.
#' @return                TRUE or it throws an error.
#'
.al_check_time_series <- function(sits_tb) {

    res <- TRUE

    if (any(.al_count_row(sits_tb) == 0)) {
        warning("Some samples have 0 time steps!")
        res <- FALSE
    }

    if (length(unique(.al_count_row(sits_tb))) != 1) {
        warning("The number of time steps don't match among the samples!")
        res <- FALSE
    }

    if (any(.al_count_col(sits_tb) < 2)) {
        warning("Some samples are missing variables!")
        res <- FALSE
    }

    if (any(.al_count_na(sits_tb) != 0)) {
        warning("NAs found in some time series!")
        res <- FALSE
    }

    if (!res)
        stop("Invalid time series!")

    return(res)
}



#' @title Count the number of columns in the time series of a sits tibble.
#'
#' @name .al_count_col
#'
#' @keywords internal
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Count the number of columns in the time series of each
#' observation in the given sits tibble.
#'
#' @param sits_tb A sits tibble.
#' @return        An integer.
#'
.al_count_col <- function(sits_tb) {
    vapply(sits_tb[["time_series"]],
           FUN = ncol,
           integer(1))
}



#' @title Count the number of NAs in the time series of a sits tibble.
#'
#' @name .al_count_na
#'
#' @keywords internal
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Count the number of NAs in the time series of each observation
#' in a sits tibble.
#'
#' @param sits_tb A sits tibble.
#' @return        An integer.
#'
.al_count_na <- function(sits_tb) {
    vapply(sits_tb[["time_series"]],
           FUN = function(x){sum(is.na(x))},
           integer(1))
}



#' @title Count the number of rows in the time series of each observation in a
#' sits tibble.
#'
#' @name .al_count_row
#'
#' @keywords internal
#'
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Count the number of rows in the time series of each observation
#' in a the given sits tibble.
#'
#' @param sits_tb A sits tibble.
#' @return        An integer.
#'
.al_count_row <- function(sits_tb) {
    vapply(sits_tb[["time_series"]],
           FUN = nrow,
           integer(1))
}
