#' @title Subtract POSIXt Class Objects in HH:MM:SS Format
#' @description subtract POSIXt class objects and return value in HH:MM:SS format.
#' @param start old Sys.time()
#' @param end new Sys.time()
#' @return subtracted time in hh:mm:ss format 
#' @details source: https://stackoverflow.com/questions/32100133/print-the-time-a-script-has-been-running-in-r answer by nrussell
#' @rdname hms_span
#' @export 
hms_span <- function(start, end) {
    dsec <- as.numeric(difftime(end, start, units = "secs"))
    hours <- floor(dsec / 3600)
    minutes <- floor((dsec - 3600 * hours) / 60)
    seconds <- dsec - 3600*hours - 60*minutes
    paste0(
           sapply(c(hours, minutes, seconds), function(x) {
                      formatC(x, width = 2, format = "d", flag = "0")}),
           collapse = ":")
}
