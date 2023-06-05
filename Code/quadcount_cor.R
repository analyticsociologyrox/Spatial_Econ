# source("Code/Makefile.R")

if (file.exists("Data/Clean/quadcounts.R")) {
  load("Data/Clean/quadcounts.R")
} else {
  yes_no <- askYesNo(msg = "This code takes a very long time to compute. It was run on the Scientific Computation Server of the University. Are you sure you want to run this code now?")

  if (yes_no == TRUE) {
    nyc_shape_owin <-
      nyc_zip_shape_ll |>
      st_transform(crs = 6345) |>
      st_simplify(dTolerance = 5) |>
      as.data.frame()

    arrests.ppp <- list()
    quadcount_arrests <- list()
    sqf.ppp <- list()
    quadcount_sqf <- list()


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
        select(-L1)

      arrests.ppp[[year - 2005]] <- as.ppp(data_arrest_test, W = nyc_shape_owin$geometry)

      quadcount_arrests[[year - 2005]] <- quadratcount(arrests.ppp[[year - 2005]], nx = 100, ny = 100)

      message(paste(Sys.time(), ": Arrests", year, "done"))

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
        select(-L1)

      sqf.ppp[[year - 2005]] <- as.ppp(data_sqf_test, W = nyc_shape_owin$geometry)
      quadcount_sqf[[year - 2005]] <- quadratcount(sqf.ppp[[year - 2005]], nx = 100, ny = 100)

      message(paste(Sys.time(), ": SQFs", year, "done"))
    }

    save(arrests.ppp,
      quadcount_arrests,
      sqf.ppp,
      quadcount_sqf,
      file = "Data/Clean/quadcounts.R"
    )
  }
}

# plot(quadcount_sqf_2006)

# df_quadcount_sqf_2006 <- as.data.frame(quadcount_sqf_2006) |>
#   rename(Freq.sqf = Freq)
#
# df_quadcount_arrests_2006 <- as.data.frame(quadcount_arrests_2006) |>
#   rename(Freq.arrests = Freq)
#
# df_quadcounts_2006 <-
#   left_join(df_quadcount_sqf_2006, df_quadcount_arrests_2006) |>
#   filter(Freq.arrests > 0 | Freq.sqf > 0)
#
# cor.test(df_quadcounts_2006$Freq.sqf, df_quadcounts_2006$Freq.arrests)
# density_arrests_2006 <- density.ppp(arrests.ppp)
# plot(density_arrests_2006)

data_cor_matrix <- 
  cbind(
    quadcount_arrests[[1]] |> as.data.frame() |> transmute(`A2006` = Freq),
    quadcount_arrests[[2]] |> as.data.frame() |> transmute(`A2007` = Freq),
    quadcount_arrests[[3]] |> as.data.frame() |> transmute(`A2008` = Freq),
    quadcount_arrests[[4]] |> as.data.frame() |> transmute(`A2009` = Freq),
    quadcount_arrests[[5]] |> as.data.frame() |> transmute(`A2010` = Freq),
    quadcount_arrests[[6]] |> as.data.frame() |> transmute(`A2011` = Freq),
    quadcount_sqf[[1]] |> as.data.frame() |> transmute(`S2006` = Freq),
    quadcount_sqf[[2]] |> as.data.frame() |> transmute(`S2007` = Freq),
    quadcount_sqf[[3]] |> as.data.frame() |> transmute(`S2008` = Freq),
    quadcount_sqf[[4]] |> as.data.frame() |> transmute(`S2009` = Freq),
    quadcount_sqf[[5]] |> as.data.frame() |> transmute(`S2010` = Freq),
    quadcount_sqf[[6]] |> as.data.frame() |> transmute(`S2011` = Freq)
    )

cor_mat <- data_cor_matrix |>
  as.matrix() |>
  rcorr()

tab_cor_mat <-
  cor_mat$r |>
  as.data.frame() |>
  round(digits = 4) |>
  gt(rownames_to_stub = TRUE) |>
  tab_options(
    # column_labels.padding.horizontal = 15,
    #data_row.padding.horizontal = 15,
    stub.border.width = 2,
    table_body.hlines.color = "black",
    stub.border.color = "black",
    table_body.border.bottom.color = "black",
    heading.border.bottom.color = "black",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table.border.top.color = "black"
  ) |>
  tab_header("Correlation Matrix") |>
  data_color(columns = everything(),
             color = scales::col_numeric(palette = c("#FEF0D9", "#990000"),
                                           domain = c(min(cor_mat$r), max(cor_mat$r))))
