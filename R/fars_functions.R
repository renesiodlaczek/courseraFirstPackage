#' Read a Fatality Analysis Reporting System data file
#'
#' This function reads a Fatality Analysis Reporting System .csv data file and
#' returns the data in a \code{tibble} format.
#'
#' @param filename A character string giving the name of the data file.
#'
#' @return This function returns the read data in a \code{tibble} format.
#'
#' @note The function execution is stopped if no file with the provided
#'      filename exists in the working directory.
#'
#' @examples
#' \dontrun{
#'         accident_2013 <- fars_read("accident_2013.csv.bz2")
#'         }
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' Make a filename for a Fatality Analysis Reporting System "csv.bz2"-file
#'
#' This function creates a character vector to create a Fatality Analysis
#' Reporting System filename. The single values consists of "accident", the
#' parameter \code{year} and the ending "csv.bz2".
#'
#' @param year A character or integer vector with the years that are
#'      displayed in the filenames.
#'
#' @note The function execution is stopped if the given parameter cannot be
#'      coerced into a integer value or vector.
#'
#' @return This function returns a character vector with the created filenames.
#'
#' @examples
#' filename_2013 <- make_filename("2013")
#' filename_2013_2014 <- make_filename(c(2013, 2014))
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}


#' Read multiple Fatality Analysis Reporting System "csv.bz2"-files
#'
#' This function reads multiple Fatality Analysis Reporting System "csv.bz2"-files
#' and returns a list with the corresponding tibbles that contain the month and
#' year of the original data entries. The parameter \code{years} specifies from
#' which years the data shall be read.
#'
#' @param years A character or integer vector that contains the years from which
#'      the data shall be read.
#'
#' @return This function returns a list of tibbles that contain the month and
#'      year of the original data entries.
#'
#' @details The function calls the functions \code{\link{make_filename}} and
#'      \code{\link{fars_read}}.
#'
#' @note The function execution is stopped if the filename creation fails or if
#'      no corresponding file for the generated filename exists in the working
#'      directory.
#'
#' @examples
#' \dontrun{
#'         fars_month_year <- fars_read_years(c(2013, 2014, 2015))
#'         fars_month_year <- fars_read_years(c("2013", "2014", "2015"))
#'         }
#'
#' @importFrom dplyr %>%
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}


#' Summarize Fatality Analysis Reporting System data
#'
#' This function summarizes multiple Fatality Analysis Reporting System
#' "csv.bz2"-files after reading them in. For every year the count of incidents
#' per month is returned. The parameter \code{years} specifies from which years
#' the data shall be read.
#'
#' @param years A character or integer vector that contains the years from which
#'      the data shall be read and summarized.
#'
#' @return This function returns a tibble in the wide-format with one column per
#'      year and one row per month. The values are the corresponding counts of
#'      incidents.
#'
#' @details The function calls the function \code{\link{fars_read_years}}.
#'
#' @examples
#' \dontrun{
#'         fars_summarize_years(c(2013, 2014, 2015))
#'         fars_summarize_years(c("2013", "2014", "2015"))
#'         }
#'
#' @importFrom dplyr %>%
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = dplyr::n()) %>%
                tidyr::spread(year, n)
}


#' Map Fatality Analysis Reporting System data
#'
#' This function creates a map with the geo-locations of the accidents for the
#' given state and given year.
#'
#' @param state.num A character or integer value of the state number for which
#'      the accidents shall be mapped.
#' @param year A character or integer value of the year for which the data shall
#'      be read and mapped.
#'
#' @return This function returns a map with the geo-locations of the accidents
#'      that happened in the requested state and year.
#'
#' @details The function calls the functions \code{\link{make_filename}} and
#'      \code{\link{fars_read}}.
#'
#' @note The function execution is stopped if an invalid state number or year is
#'      given or when there is no data to plot.
#'
#' @examples
#' \dontrun{
#'         fars_map_state(1, 2015)
#'         fars_map_state("1", "2015")
#'         }
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}

