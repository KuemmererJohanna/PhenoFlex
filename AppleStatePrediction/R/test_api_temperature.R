library(httr)
library(jsonlite)

api_url <- "https://geoservices.buergernetz.bz.it/services/meteo/v1/timeseries"

response <- GET(
  api_url,
  query = list(
    station_code = "86600MS",
    sensor_code = "LT",
    output_format = "JSON",
    date_from = "202610010000",
    date_to = "202610092359"
  ),
  timeout(60)
)

#cat("HTTP-Status:", status_code(response), "\n")
#cat("Antwort der API:\n")
#cat(content(response, as = "text", encoding = "UTF-8"), "\n")