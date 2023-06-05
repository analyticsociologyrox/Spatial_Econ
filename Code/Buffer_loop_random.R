# source("Code/Makefile.R")

if(file.exists("Data/Clean/data_non_sqf.R")) {
  load("Data/Clean/data_non_sqf.R")
} else {
  yes_no <- askYesNo(msg = "This code takes a very long time to compute. It was run on the Scientific Computation Server of the University. Are you sure you want to run this code now?")

  if (yes_no == TRUE) {
    data_arrest <-
      data_arrest |>
      filter(
        !is.na(ARREST_DATE),
        !is.na(Lon_Lat)
      )

    data_sqf <-
      data_sqf |>
      filter(
        !is.na(datestop),
        !is.na(xcoord),
        !is.na(ycoord),
        year(ymd(datestop)) %in% c(2006:2011),
        datestop >= "2006-01-05",
        datestop <= "2011-12-26"
      ) |>
      mutate(across(.cols = ends_with("coord"), .fns = as.numeric)) |>
      as_tibble()

    data_sqf_sf <-
      data_sqf |>
      st_as_sf(coords = c("xcoord", "ycoord"), crs = st_crs("EPSG:2263")) |>
      st_transform(crs = st_crs("WGS84"))

    arrests_before_fct <- function(day, buffer) {
      y <- data_arrest |>
        filter(
          ARREST_DATE < day,
          ARREST_DATE > day - 5
        ) |>
        st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs("WGS84"))

      st_contains(x = buffer, y = y)
    }
    arrests_before_fct <- Vectorize(FUN = arrests_before_fct, SIMPLIFY = FALSE)
    arrests_before_fct <- compiler::cmpfun(arrests_before_fct)

    arrests_after_fct <- function(day, buffer) {
      y <- data_arrest |>
        filter(
          ARREST_DATE > day,
          ARREST_DATE < day + 5
        ) |>
        st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs("WGS84"))

      st_contains(x = buffer, y = y)
    }
    arrests_after_fct <- Vectorize(FUN = arrests_after_fct, SIMPLIFY = FALSE)
    arrests_after_fct <- compiler::cmpfun(arrests_after_fct)

    other_sqfs_fct <- function(day, buffer) {
      y <- data_sqf_sf |>
        filter(
          datestop >= day - 5,
          datestop <= day + 5
        )

      st_contains(x = buffer, y = y)
    }
    other_sqfs_fct <- Vectorize(FUN = other_sqfs_fct, SIMPLIFY = FALSE)
    other_sqfs_fct <- compiler::cmpfun(other_sqfs_fct)

    count_arrests_fct <- function(x) {
      x |>
        as.data.frame() |>
        nrow()
    }
    count_arrests_fct <- Vectorize(FUN = count_arrests_fct)
    count_arrests_fct <- compiler::cmpfun(count_arrests_fct)

    nyc_shape <-
      nyc_zip_shape_ll |>
      st_simplify(dTolerance = 50) |>
      st_union()

    days <- unique(data_sqf$datestop)
    n_days <- length(days)
    n_per_day <- 500

    data_non_sqf <- tibble(.rows = n_days * n_per_day)
    data_non_sqf$datestop <- Date(nrow(data_non_sqf))
    data_non_sqf$is_sqf <- 0L
    data_non_sqf$xcoord <- numeric(nrow(data_non_sqf))
    data_non_sqf$ycoord <- numeric(nrow(data_non_sqf))
    data_non_sqf$arrests_before <- numeric(nrow(data_non_sqf))
    data_non_sqf$arrests_after <- numeric(nrow(data_non_sqf))
    data_non_sqf$other_sqfs <- numeric(nrow(data_non_sqf))

    # load("Data/Clean/data_sqf_looped.R")

    message(paste(Sys.time(), ": beginning for-loop"))
    for (i in 1952:length(days)) {
      day <- days[i]

      non_sqfs <- st_sample(x = nyc_shape, size = n_per_day, type = "random", exact = TRUE)

      for (j in seq_along(non_sqfs)) {
        buffer_sqf <-
          non_sqfs[j] |>
          st_buffer(dist = 1000)

        data_non_sqf$xcoord[(i - 1) * 500 + j] <-
          st_coordinates(non_sqfs[j])[, 1]

        data_non_sqf$ycoord[(i - 1) * 500 + j] <-
          st_coordinates(non_sqfs[j])[, 2]

        arrests_before <-
          arrests_before_fct(
            day = day,
            buffer = buffer_sqf
          )

        arrests_after <-
          arrests_after_fct(
            day = day,
            buffer = buffer_sqf
          )

        other_sqfs <-
          other_sqfs_fct(
            day = day,
            buffer = buffer_sqf
          )

        data_non_sqf$datestop[(i - 1) * 500 + j] <- day

        data_non_sqf$arrests_before[(i - 1) * 500 + j] <- length(arrests_before[[1]][[1]])

        data_non_sqf$arrests_after[(i - 1) * 500 + j] <- length(arrests_after[[1]][[1]])

        data_non_sqf$other_sqfs[(i - 1) * 500 + j] <- length(other_sqfs[[1]][[1]])
      }
      message(paste(Sys.time(), "day", i, "of", length(days), ": ", round(i / length(days) * 100, 4), " % done"))

      if (!exists("last_save_i")) {
        last_save_i <- i
        time_last_save <- Sys.time()
        time_this_save <- Sys.time()
        save(data_non_sqf, file = "Data/Clean/data_non_sqf.R")
        message(paste0(Sys.time(), " saved at iteration ", i, " of ", nrow(data_sqf), ": ", round(i / nrow(data_sqf) * 100, 4), " % \n
                 Time since last save (200 Iterations): ", time_this_save - time_last_save, " min"))
      } else if (i - last_save_i >= 10) {
        last_save_i <- i
        time_this_save <- Sys.time()
        save(data_non_sqf, last_save_i, file = "Data/Clean/data_non_sqf.R")
        message(paste(Sys.time(), "Saved at iteration", i))
        message(paste0(Sys.time(), " saved at iteration ", i, " of ", length(days), ": ", round(i / length(days) * 100, 4), " % \n
                   Time since last save (10 Iterations):", time_this_save - time_last_save, " min"))

        time_last_save <- Sys.time()
      }
    }

    message(paste(Sys.time(), ": ended for-loop"))
  }
}
