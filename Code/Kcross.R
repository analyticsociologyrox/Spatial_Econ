# source("Code/Makefile.R")

if (file.exists("Data/Clean/Kcross_save.R")) {
  load("Data/Clean/Kcross_save.R")
} else {
  yes_no <- askYesNo(msg = "This code takes a very long time to compute. It was run on the Scientific Computation Server of the University. Are you sure you want to run this code now?")

  if (yes_no == TRUE) {
    nyc_shape_owin <-
      nyc_zip_shape_ll |>
      st_transform(crs = 6345) |>
      st_simplify(dTolerance = 5) |>
      as.data.frame()

    r <- seq(0, 5000, length.out = 20)

    marked.ppp <- vector("list", 6)
    Kcross <- vector("list", 6)

    message(paste0(Sys.time(), ": Beginning for loop"))
    for (year in 2006:2011) {
      data_arrest_test <-
        data_arrest |>
        filter(ARREST_DATE < paste0(year + 1, "-01-01")) |>
        select(Longitude, Latitude) |>
        na.omit() |>
        sample_n(10000) |>
        as.matrix() |>
        st_multipoint() |>
        st_sfc(crs = st_crs("WGS84")) |>
        st_transform(crs = 6345) |>
        st_coordinates() |>
        as.data.frame() |>
        mutate(marks = "arrest") |>
        select(-L1)

      data_sqf_test <-
        data_sqf |>
        filter(year == as.numeric(year)) |>
        select(xcoord, ycoord) |>
        na.omit() |>
        sample_n(10000) |>
        as.matrix() |>
        st_multipoint() |>
        st_sfc(crs = st_crs("EPSG:2263")) |>
        st_transform(crs = 6345) |>
        st_coordinates() |>
        as.data.frame() |>
        mutate(marks = "sqf") |>
        select(-L1)

      data_marked_test <- rbind(data_arrest_test, data_sqf_test)

      marked.ppp[[year - 2005]] <-
        ppp(
          x = data_marked_test$X,
          y = data_marked_test$Y,
          window = as.owin(nyc_shape_owin$geometry),
          marks = as.factor(data_marked_test$marks)
        )

      Kcross[[year - 2005]] <-
        Kcross(
          X = marked.ppp[[year - 2005]],
          i = "sqf",
          j = "arrest",
          r = r,
          correction = "Ripley"
        )

      save(marked.ppp, Kcross, file = "Data/Clean/Kcross_save.R")
      message(paste0(Sys.time(), ": Saved Kcross of year", year))
    }
  }
}

# for (i in 1:6) {
#   K1 <- Kcross[[i]]
#   year <- 2005 + i
# 
#   gg[[i]] <-
#     ggplot(mapping = aes(x = r)) +
#     geom_line(mapping = aes(y = K1$iso, color = "Kcross")) +
#     geom_line(mapping = aes(y = K1$theo, color = "pois."), linetype = "dashed") +
#     scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
#     xlab("Distance in meters") +
#     ylab("K-hat") +
#     scale_color_manual(values = c("Kcross" = "blue", "SQFs" = "red", "pois." = "black"), name = "") +
#     theme_bw() +
#     labs(title = paste0("Cluster Analysis ", year)) +
#     theme(legend.position = c(0.3, 0.8), legend.title = element_blank())
# }
# 
# clustering_plot <-
#   plot_grid(
#     gg[[1]],
#     gg[[2]],
#     gg[[3]],
#     gg[[4]],
#     gg[[5]],
#     gg[[6]]
#   )
# 
# plot(Kcross[[1]])
