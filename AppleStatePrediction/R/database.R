# R/database.R


library(DBI)
library(RSQLite)


# connection to SQLight database

db_path <- "data/GoldenDeliciousDB.db"


get_db_connection <- function() {
  
  dbConnect(
    SQLite(),
    dbname = db_path
  )
}


# extract raw temperature data 

get_raw_temperature_data <- function(
    start_date = NULL,
    end_date = NULL,
    source = NULL
) {
  
  con <- get_db_connection()
  
  on.exit(
    DBI::dbDisconnect(con),
    add = TRUE
  )
  
  query <- "
    SELECT
      ts,
      value,
      source
    FROM temperature_timeseries
    WHERE 1 = 1
  "
  
  params <- list()
  
  if (!is.null(start_date)) {
    query <- paste0(query, " AND ts >= ?")
    params <- c(params, list(as.character(start_date)))
  }
  
  if (!is.null(end_date)) {
    query <- paste0(query, " AND ts <= ?")
    params <- c(params, list(as.character(end_date)))
  }
  
  if (!is.null(source) && length(source) == 1 &&
      !is.na(source) && source != "All") {
    query <- paste0(query, " AND source = ?")
    params <- c(params, list(source))
  }
  
  query <- paste0(query, " ORDER BY ts")
  
  if (length(params) == 0) {
    DBI::dbGetQuery(con, query)
  } else {
    DBI::dbGetQuery(con, query, params = params)
  }
}



# get hourly_temperature view

get_hourly_temperature <- function() {
  
  con <- get_db_connection()
  
  on.exit(
    dbDisconnect(con),
    add = TRUE
  )
  
  dbGetQuery(
    con,
    "
    SELECT
      date,
      tmin,
      tmax,
      source
    FROM hourly_temperature
    ORDER BY date
    "
  )
}



get_daily_temperature <- function() {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  
  dbGetQuery(con, "
    SELECT
      date(ts) AS date,
      MIN(value) AS tmin,
      MAX(value) AS tmax,
      'daily' AS source
    FROM temperature_timeseries
    GROUP BY date(ts)
    ORDER BY date
  ")
}



# get historical average_hourly_temperature

get_average_hourly_temperature <- function() {
  
  con <- get_db_connection()
  
  on.exit(
    dbDisconnect(con),
    add = TRUE
  )
  
  dbGetQuery(
    con,
    "
    SELECT
      date,
      tmin,
      tmax,
      source
    FROM average_hourly_temp
    ORDER BY date
    "
  )
}



# get min and max temperatures from temperature_timeseries

get_temperature_date_range <- function() {
  
  con <- get_db_connection()
  
  on.exit(
    dbDisconnect(con),
    add = TRUE
  )
  
  dbGetQuery(
    con,
    "
    SELECT
      MIN(ts) AS min_ts,
      MAX(ts) AS max_ts
    FROM temperature_timeseries
    "
  )
}




# retrieve phenological data information

get_phenology_data <- function() {
  
  con <- get_db_connection()
  
  on.exit(
    dbDisconnect(con),
    add = TRUE
  )
  
  dbGetQuery(
    con,
    "
    SELECT *
    FROM phenology
    ORDER BY year
    "
  )
}


# get a specific year with its phenology

get_phenology_year <- function(year) {
  
  con <- get_db_connection()
  
  on.exit(
    dbDisconnect(con),
    add = TRUE
  )
  
  dbGetQuery(
    con,
    "
    SELECT *
    FROM phenology
    WHERE year = ?
    ",
    params = list(as.integer(year))
  )
}