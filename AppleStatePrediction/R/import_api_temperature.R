
# import_api_temperature.R

library(DBI)
library(RSQLite)
library(httr)
library(jsonlite)
library(dplyr)
library(tibble)

# --------------------------------------------------
# Configuration
# --------------------------------------------------

db_path <- "data/GoldenDeliciousDB.db"

api_url <- paste0(
  "https://geoservices.buergernetz.bz.it",
  "/services/meteo/v1/timeseries"
)

# Correct station and air-temperature sensor
station_code <- "86600MS"
sensor_code  <- "LT"

data_source <- "API"

initial_import_date <- as.Date(
  paste0(format(Sys.Date(), "%Y"), "-01-01")
)


# --------------------------------------------------
# Database connection
# --------------------------------------------------

get_db_connection <- function() {
  dbConnect(SQLite(), dbname = db_path)
}


# --------------------------------------------------
# Find latest imported timestamp
# --------------------------------------------------

get_last_api_timestamp <- function(con) {
  result <- dbGetQuery(
    con,
    paste(
      "SELECT MAX(ts) AS last_ts",
      "FROM temperature_timeseries",
      "WHERE source = ?"
    ),
    params = list(data_source)
  )
  
  result$last_ts[[1]]
}


# --------------------------------------------------
# Download measurements for a date range
# --------------------------------------------------


download_api_temperature <- function(date_from, date_to) {
  
  message("  Request: ", date_from, " to ", date_to)
  
  response <- httr::GET(
    api_url,
    query = list(
      station_code = station_code,
      sensor_code = sensor_code,
      output_format = "JSON",
      date_from = paste0(
        format(date_from, "%Y%m%d"), "0000"
      ),
      date_to = paste0(
        format(date_to, "%Y%m%d"), "2359"
      )
    ),
    httr::timeout(120)
  )
  
  httr::stop_for_status(response)
  
  response_text <- httr::content(
    response,
    as = "text",
    encoding = "UTF-8"
  )
  
  # Handle empty API responses
  if (
    !nzchar(trimws(response_text)) ||
    trimws(response_text) %in% c("[]", "null")
  ) {
    return(tibble::tibble(
      ts = character(),
      value = numeric(),
      source = character()
    ))
  }
  
  records <- jsonlite::fromJSON(
    response_text,
    simplifyDataFrame = TRUE
  )
  
  if (is.null(records) || NROW(records) == 0) {
    return(tibble::tibble(
      ts = character(),
      value = numeric(),
      source = character()
    ))
  }
  
  records <- tibble::as_tibble(records)
  
  if (!all(c("DATE", "VALUE") %in% names(records))) {
    stop(
      "Unexpected API response. Columns received: ",
      paste(names(records), collapse = ", ")
    )
  }
  
  # Preserve the API's local clock time.
  # Remove timezone labels without converting to UTC.
  api_timestamps <- trimws(as.character(records$DATE))
  
  api_timestamps <- sub(
    "(CEST|CET|UTC|GMT)$",
    "",
    api_timestamps,
    ignore.case = TRUE
  )
  
  api_timestamps <- sub(
    "T",
    " ",
    api_timestamps,
    fixed = TRUE
  )
  
  result <- tibble::tibble(
    ts = api_timestamps,
    value = as.numeric(records$VALUE),
    source = data_source
  ) %>%
    dplyr::filter(
      !is.na(ts),
      nzchar(ts),
      !is.na(value),
      is.finite(value)
    ) %>%
    dplyr::distinct(ts, source, .keep_all = TRUE)
  
  result
}



# --------------------------------------------------
# Import measurements into SQLite
# --------------------------------------------------


import_api_temperature <- function(
    date_from = Sys.Date(),
    date_to = Sys.Date()
) {
  
  date_from <- as.Date(date_from)
  date_to   <- as.Date(date_to)
  
  if (is.na(date_from) || is.na(date_to) || date_from > date_to) {
    stop("Invalid date range.")
  }
  
  con <- get_db_connection()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  if (!DBI::dbExistsTable(con, "temperature_timeseries")) {
    stop("Table 'temperature_timeseries' does not exist.")
  }
  
  message(
    "Importing air temperature for station ", station_code,
    ", sensor ", sensor_code
  )
  message("Date range: ", date_from, " to ", date_to)
  
  # Check for duplicate timestamps before creating the index.
  duplicate_check <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT COUNT(*) AS n FROM (",
      "SELECT ts, source",
      "FROM temperature_timeseries",
      "GROUP BY ts, source",
      "HAVING COUNT(*) > 1",
      ")"
    )
  )
  
  if (duplicate_check$n[1] > 0) {
    stop(
      "Duplicate (ts, source) pairs exist in the database. ",
      "Resolve them before importing."
    )
  }
  
  DBI::dbExecute(
    con,
    paste(
      "CREATE UNIQUE INDEX IF NOT EXISTS",
      "idx_temperature_ts_source",
      "ON temperature_timeseries(ts, source)"
    )
  )
  
  total_received <- 0L
  total_written  <- 0L
  current_start  <- date_from
  
  while (current_start <= date_to) {
    
    # Restrict each API request to one calendar month at most.
    month_start <- as.Date(
      paste0(format(current_start, "%Y-%m"), "-01")
    )
    
    next_month <- seq.Date(
      month_start,
      by = "month",
      length.out = 2
    )[2]
    
    current_end <- min(next_month - 1, date_to)
    
    records <- download_api_temperature(
      current_start,
      current_end
    )
    
    total_received <- total_received + nrow(records)
    
    if (nrow(records) > 0) {
      
      DBI::dbBegin(con)
      
      tryCatch({
        
        written <- 0L
        
        for (i in seq_len(nrow(records))) {
          written <- written + DBI::dbExecute(
            con,
            paste(
              "INSERT INTO temperature_timeseries",
              "(ts, value, source)",
              "VALUES (?, ?, ?)",
              "ON CONFLICT(ts, source)",
              "DO UPDATE SET value = excluded.value"
            ),
            params = list(
              records$ts[i],
              records$value[i],
              records$source[i]
            )
          )
        }
        
        DBI::dbCommit(con)
        total_written <- total_written + written
        
        message(
          "  Received: ", nrow(records),
          "; inserted or updated: ", written
        )
        
      }, error = function(e) {
        DBI::dbRollback(con)
        stop(e)
      })
      
    } else {
      message("  No measurements in this period.")
    }
    
    current_start <- current_end + 1
  }
  
  message("Import completed.")
  message("Measurements received: ", total_received)
  message("Database writes: ", total_written)
  
  invisible(NULL)
}
