#' corona2
#'
#' Data taken from coronascraper

corona2 = function(){

  f = suppressMessages(data.table::fread("https://coronadatascraper.com/timeseries-tidy.csv", verbose = FALSE))
  f = tidyr::pivot_wider(f, values_from = value, names_from = type)
  f$date = as.Date(f$date)

  f$city[f$city == ""] = NA
  f$state[f$state == ""] = NA
  f$county[f$county == ""] = NA
  f$country[f$country == ""] = NA

  f = f[!(f$county == "AUS" & is.na(f$state)),]

  return(f)
}



