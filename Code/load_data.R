if (file.exists("Data/Clean/data_sqf.R") &
  file.exists("Data/Clean/data_arrest.R") &
  file.exists("Data/Clean/nyc_shape_ll.R") &
  file.exists("Data/Clean/n_sqf_all.R") &
  file.exists("Data/Clean/nyc_zip_shape_ll.R")) {
  load("Data/Clean/data_sqf.R")
  load("Data/Clean/data_arrest.R")
  load("Data/Clean/nyc_shape_ll.R")
  load("Data/Clean/n_sqf_all.R")
  load("Data/Clean/nyc_zip_shape_ll.R")
} else {
  X2006 <- read_csv("Data/Raw/SQF Data/2006.csv")
  X2006 <- X2006 |>
    mutate(
      datestop = as.character(datestop),
      datestop = paste0(substr(datestop, 6, 7), substr(datestop, 9, 10), substr(datestop, 1, 4)),
      datestop = if_else(datestop == "NANANA", NA_character_, datestop)
    )

  X2007 <- read_csv("Data/Raw/SQF Data/2007.csv")

  X2008 <- read_csv("Data/Raw/SQF Data/2008.csv")

  X2009 <- read_csv("Data/Raw/SQF Data/2009.csv")

  X2010 <- read_csv("Data/Raw/SQF Data/2010.csv")

  X2011 <- read_csv("Data/Raw/SQF Data/2011.csv")

  filter_sqf <- function(data) {
    data <- data |>
      filter(
        # !is.na(datestop),
        !is.na(xcoord),
        !is.na(ycoord)
      ) |>
      select(year, datestop, timestop, inout, crimsusp, frisked, searched, contrabn, sex, race, age, xcoord, ycoord) |>
      mutate(
        datestop = mdy(datestop),
        timestop = as.numeric(timestop),
        age = as.numeric(age),
        xcoord = as.numeric(xcoord),
        ycoord = as.numeric(ycoord)
      )

    return(data)
  }

  data_sqf <-
    bind_rows(
      filter_sqf(X2006),
      filter_sqf(X2007),
      filter_sqf(X2008),
      filter_sqf(X2009),
      filter_sqf(X2010),
      filter_sqf(X2011)
    )

  n_sqf_all <- data.frame(
    year = c(2006:2011),
    n_all = c(nrow(X2006), nrow(X2007), nrow(X2008), nrow(X2009), nrow(X2010), nrow(X2011))
  )

  data_arrest <-
    read_csv_arrow("Data/Raw/NYPD_Arrests_Data__Historic_.csv") |>
    mutate(ARREST_DATE = mdy(ARREST_DATE)) |>
    filter(ARREST_DATE < "2012-01-01") |>
    collect()

  nyc_shape_ll <- read_sf("Data/Raw/nyc.GeoJSON")
  
  nyc_zip_shape_ll <- read_sf("Data/Raw/ZIP_CODE_040114/ZIP_CODE_040114.shp") |>
    st_transform(crs = st_crs("WGS84"))

  save(data_sqf, file = "Data/Clean/data_sqf.R")
  save(data_arrest, file = "Data/Clean/data_arrest.R")
  save(nyc_shape_ll, file = "Data/Clean/nyc_shape_ll.R")
  save(n_sqf_all, file = "Data/Clean/n_sqf_all.R")
  save(nyc_zip_shape_ll, file = "Data/Clean/nyc_zip_shape_ll.R")
}