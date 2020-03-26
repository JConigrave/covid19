#' corona
#'
#' Live dataset for coronavirus
#' @import data.table
#' @export corona
#' @examples
#' library(ggplot2)
#' aus = subset(corona(), region == "Australia")
#' ggplot(aus, aes(x = date, y = active)) +
#' geom_point() + facet_wrap( ~ state) +
#' geom_line() + theme_bw()



corona = function(){

confirmed = data.table::fread("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv&filename=time_series_covid19_confirmed_global.csv", showProgress = F)
deaths =  data.table::fread("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv&filename=time_series_covid19_deaths_global.csv", showProgress = F)
#recovered =  data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv", showProgress = F)

basic_cols = names(confirmed)[1:4]


con_final = confirmed %>% tidyr::pivot_longer(names(confirmed)[!names(confirmed) %in% basic_cols], names_to = "date", values_to = "cases")


dth_final = deaths %>%  tidyr::pivot_longer(names(deaths)[!names(deaths) %in% basic_cols], names_to = "date", values_to = "deaths")

#rcvr  = recovered %>% tidyr::pivot_longer(names(recovered)[!names(recovered) %in% basic_cols], names_to = "date", values_to = "recovered")

f = dplyr::left_join(con_final, dth_final , by = c("Province/State", "Country/Region", "Lat", "Long", "date")) #%>%
  #dplyr::left_join(rcvr, by = c("Province/State", "Country/Region", "Lat", "Long", "date"))

f$active = f$cases - f$deaths # - f$recovered

f$date = as.Date(f$date, format = "%m/%d/%y")

f = data.table::data.table(f)
names(f) = c("state","region","lat","long","date","cases","deaths","active")

f = f[order(date)]

f[,new_cases := (cases - data.table::shift(cases)), by = c("region","state")]
f[,new_deaths := (deaths - data.table::shift(deaths)), by = c("region","state")]
#f[,new_recovered := (recovered - data.table::shift(recovered)), by = c("region","state")]
#f[,resolved := (new_deaths + new_recovered)]
f = data.frame(f)

f$growth = (f$new_cases) / f$active * 100

f$growth[is.infinite(f$growth)] = NA

return(f)

}
