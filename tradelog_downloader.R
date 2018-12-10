#' Download Tradelog data from Lykke.
#'
#' @param date_start Starting date (YYYY-MM-DD, default ="2018-10-01")
#' @param end_date End date(YYYY-MM-DD, default = NA)
#' @return A data frame with all trades that took place on Lykke exchange between \code{date_start} and \code{date_end}
#' @examples
#' lykke_tradelog()
#' lykke_tradelog("2018-09-12", "2018-09-15")
#'
lykke_tradelog <- function(date_start = "2018-10-01",
                              date_end = NA){

if(!is.na(date_end) & date_start < date_end & date_start >= "2018-02-19"){
  interval <- seq(from=as.Date(date_start), to=min(as.Date(date_end), Sys.Date()-1), by='days')
}else{
  interval <- date_start
  cat("No valid end date - data is downloaded only for", max("2018-02-19", date_start), "\n")
}

url <- paste0("https://lykkedwhpublic.blob.core.windows.net/lykke-dwh-tradelog/tradelog_", interval, ".gz")

data <-purrr::map(url, function(x) {
  tryCatch({
  tmp <- tempfile()
  download.file(x,tmp, quiet=TRUE)
  data <- data.frame(read.csv(gzfile(tmp), sep=";", header=TRUE, stringsAsFactors = FALSE))
  data <- dplyr::mutate(data, DateTime = lubridate::ymd_hms(DateTime),
           OrderCreated = lubridate::ymd_hms(OrderCreated))
  }, error=function(er) cat('Date not available'))

})

data <- dplyr::bind_rows(data)

return(data)
}

