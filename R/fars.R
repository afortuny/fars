#' fars_read
#' fars read first check if the filename provided exists, then loads the file suppresing all messages 
#' and  transfor the data into a tibble.
#'
#' @param  filename Path and name of the file to be downloaded and treated
#' @return If the file exists it returns a tible, otherwise it informs that the filename does not exists
#' @examples
#' \dontrun{
#' data<-fars_read("inst/extdata/accident_2013.csv.bz2")
#'}
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' 
#' @export
#'
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}




#' make_filename
#' make_filename create the filename based on the year from which we want to extract the information 
#'
#' @param  year An integer that indicates the year from which we can to extract the information
#' @return The filename string for that specific year
#' @examples
#' \dontrun{
#' make_filename<-make_filename(2015)
#'}
#' 
#' @export
#'
#'

make_filename <- function(year) {
  year <- as.integer(year)
  as.character(sprintf("accident_%d.csv.bz2", year))
}




#' fars_read_years
#' fars_read_years Read and store the month and year column for each of the years provided as argument. Function make_file and fars_read name needed.
#'
#' @param  years Year or range of years from which we want to extract the information
#' @return A list with as many tibles as valid years provided. If invalid years are provided the code will create a warning specifying the unvalid year.
#' @examples
#' \dontrun{
#' setwd(system.file("extdata", package = "fars"))
#' data<-fars_read_years(2013:2015)
#'}
#'
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @export
#'
#'


fars_read_years <- function(years) {
  data<-list()
  for (year in years){
    
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      subset<-dplyr::mutate_(dat, year = ~year) %>%
        dplyr::select_("MONTH", "year")
      
      data[paste0(year)]<-list(subset)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  }
  return(data)
}




#' fars_summarize_years
#' fars_summarize_years Read the data from the specified years and calculate how many observations were for each month
#'
#' @param  years Year or range of years from which we want to extract the information
#' @return A tibble with the number of observations per month for each year
#' @examples
#' \dontrun{
#' summary_data<-fars_summarize_years(2013:2015)
#'}
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' 
#' 
#' @export
#'
#'


fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}




#' fars_map_state
#' fars_map_state Read the data for a specific state and year and plot it in a map of the state with the location
#'
#' @param  year Year from which we want to extract the information. Not a range!
#' @param  state.num The integer number refering to the state we want to plot the information
#' @return A plot of the state with the location of the data, as long as the state is valid and there is data to plot
#' @examples
#' \dontrun{
#' fars_map_state(6,2014)
#'}
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' 
#' 
#' @export
#'
#'


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







