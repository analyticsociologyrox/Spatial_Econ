if (!file.exists("Data/Clean/regressions_simple.Rda")) {

    lmer_dat <-
    data_final_long |>
    mutate(
      pos = if_else(neg == 0, 1, 0) |> as.factor(),
      not_sqf = if_else(is_sqf == 0, 1, 0) |> as.factor()
    )
  
  lmer_dat_simple <-
    lmer_dat |>
    group_by(id, pos, dist) |>
    mutate(arrests = sum(arrests),
           other_sqfs = sum(other_sqfs)) |>
    distinct(id, pos, dist, .keep_all = TRUE)
  
  lmer_simple_1 <-
    lmer_dat_simple |>
    filter(dist == 1) |>
    lmer(
      formula = arrests ~ other_sqfs + pos * is_sqf + (1 | reg / id),
      data = _
    )
  
  lmer_simple_5<-
    lmer_dat_simple |>
    filter(dist == 5) |>
    lmer(
      formula = arrests ~ other_sqfs + pos * is_sqf + (1 | reg / id),
      data = _
    )
  
  lmer_simple_10 <-
    lmer_dat_simple |>
    filter(dist == 10) |>
    lmer(
      formula = arrests ~ other_sqfs + pos * is_sqf + (1 | reg / id),
      data = _
    )
  
  lmer_simple_all <-
    lmer_dat_simple |>
    lmer(
      formula = arrests ~ other_sqfs + pos * is_sqf + (1 | reg / id),
      data = _
    )
  

  lmer_fit_1 <-
    lmer_dat |>
    filter(dist == 1) |>
    lmer(
      formula = arrests ~ time_to_sqf + other_sqfs + pos * is_sqf + (1 | reg / id),
      data = _
    )

  lmer_fit_5 <-
    lmer_dat |>
    filter(dist == 5) |>
    lmer(
      formula = arrests ~ time_to_sqf + other_sqfs + pos * is_sqf + (1 | reg / id),
      data = _
    )

  lmer_fit_10 <-
    lmer_dat |>
    filter(dist == 10) |>
    lmer(
      formula = arrests ~ time_to_sqf + other_sqfs + pos * is_sqf + (1 | reg / id),
      data = _
    )

  lmer_fit_all <-
    lmer(
      formula = arrests ~ time_to_sqf + other_sqfs + pos * is_sqf + (1 | reg / id),
      data = lmer_dat
    )

  save(lmer_simple_1, lmer_simple_5, lmer_simple_10, lmer_simple_all,
       file = "Data/Clean/regressions_simple.Rda")
  
  stargazer(
    lmer_fit_1,
    lmer_fit_5,
    lmer_fit_10,
    lmer_fit_all,
    header = FALSE,
    title = "Regressions with seperate days",
    digit.separator = " ",
    intercept.top = TRUE,
    intercept.bottom = FALSE,
    no.space = FALSE,
    column.sep.width = "3pt",
    omit.stat = "ll",
    font.size = "footnotesize",
    dep.var.labels = "Number of Arrests \\vspace*{0.2cm}",
    covariate.labels = c("Intercept", "Time to Event", "Other SQFs", "After Event", "SQF or Control", "SQF*After"),
    notes.append = TRUE,
    notes = c("All models are fitted using OLS fixed-effects for the 5 boroughs of New York."),
    notes.align = "l",
    column.labels = c("in 1km radius", "in 5km radius", "in 10km radius", "in any radius"),
    model.names = FALSE,
    model.numbers = FALSE,
    out = "Writing/Term_Paper/regression_tab_2.tex",
    out.header = FALSE
  )
  
  library("plm")
  
  p.dat_1 <- 
    lmer_dat |>
    filter(dist == 1) |>
    pdata.frame(index = c("id", "time_to_sqf"))
  
  plm_fit_1 <-
    p.dat_1 |>
    plm(formula = arrests ~ other_sqfs + pos * is_sqf,
        effect = "twoways",
        model = "random",
        data = _)
  
  p.dat_5 <- 
    lmer_dat |>
    filter(dist == 5) |>
    pdata.frame(index = c("id", "time_to_sqf"))
  
  plm_fit_5 <-
    p.dat_5 |>
    plm(formula = arrests ~ other_sqfs + pos * is_sqf,
        effect = "twoways",
        model = "random",
        data = _)
  
  p.dat_10 <- 
    lmer_dat |>
    filter(dist == 10) |>
    pdata.frame(index = c("id", "time_to_sqf"))
  
  plm_fit_10 <-
    p.dat_10 |>
    plm(formula = arrests ~ other_sqfs + pos * is_sqf,
        effect = "twoways",
        model = "random",
        data = _)
  
  library(stargazer)
  
  relations <- 
    c(
      plm_fit_1$coefficients["pos1:is_sqf1"] / plm_fit_1$coefficients["(Intercept)"],
      plm_fit_5$coefficients["pos1:is_sqf1"] / plm_fit_5$coefficients["(Intercept)"],
      plm_fit_10$coefficients["pos1:is_sqf1"] / plm_fit_10$coefficients["(Intercept)"]
    ) |>
    round(3)
  
  stargazer(
    plm_fit_1,
    plm_fit_5,
    plm_fit_10,
    header = FALSE,
    title = "Random-effects regression models",
    digit.separator = " ",
    intercept.top = TRUE,
    intercept.bottom = FALSE,
    no.space = FALSE,
    column.sep.width = "4pt",
    omit.stat = "ll",
    font.size = "footnotesize",
    dep.var.labels = "Number of Arrests \\vspace*{0.2cm}",
    covariate.labels = c("Intercept", "Other SQFs", "After Event", "SQF or Control", "SQF*After"),
    notes.append = TRUE,
    notes = c("All models are fitted using OLS random-effects", "accounting for the clustering of considered", "days per event."),
    notes.align = "l",
    column.labels = c("in 1km radius", "in 5km radius", "in 10km radius", "in any radius"),
    model.names = FALSE,
    model.numbers = FALSE,
    out = "Writing/Term_Paper/regression_tab.tex",
    out.header = FALSE,
    add.lines = list(c("Interaction/Intercept", relations))
  )
  
  lmer_dat <-
    lmer_dat |>
    filter(time_to_sqf >= -5 &
             time_to_sqf <= 5)
  
  p.dat_1 <- 
    lmer_dat |>
    filter(dist == 1) |>
    pdata.frame(index = c("id", "time_to_sqf"))
  
  plm_fit_1_5 <-
    p.dat_1 |>
    plm(formula = arrests ~ other_sqfs + pos * is_sqf,
        effect = "twoways",
        model = "random",
        data = _)
  
  p.dat_5 <- 
    lmer_dat |>
    filter(dist == 5) |>
    pdata.frame(index = c("id", "time_to_sqf"))
  
  plm_fit_5_5 <-
    p.dat_5 |>
    plm(formula = arrests ~ other_sqfs + pos * is_sqf,
        effect = "twoways",
        model = "random",
        data = _)
  
  p.dat_10 <- 
    lmer_dat |>
    filter(dist == 10) |>
    pdata.frame(index = c("id", "time_to_sqf"))
  
  plm_fit_10_5 <-
    p.dat_10 |>
    plm(formula = arrests ~ other_sqfs + pos * is_sqf,
        effect = "twoways",
        model = "random",
        data = _)
  
  relations_5 <- 
    c(
      plm_fit_1_5$coefficients["pos1:is_sqf1"] / plm_fit_1_5$coefficients["(Intercept)"],
      plm_fit_5_5$coefficients["pos1:is_sqf1"] / plm_fit_5_5$coefficients["(Intercept)"],
      plm_fit_10_5$coefficients["pos1:is_sqf1"] / plm_fit_10_5$coefficients["(Intercept)"]
    ) |>
    round(3)
  
  stargazer(
    plm_fit_1_5,
    plm_fit_5_5,
    plm_fit_10_5,
    header = FALSE,
    title = "Random-effects regression models with only 5 days before and after",
    digit.separator = " ",
    intercept.top = TRUE,
    intercept.bottom = FALSE,
    no.space = FALSE,
    column.sep.width = "4pt",
    omit.stat = "ll",
    font.size = "footnotesize",
    dep.var.labels = "Number of Arrests \\vspace*{0.2cm}",
    covariate.labels = c("Intercept", "Other SQFs", "After Event", "SQF or Control", "SQF*After"),
    notes.append = TRUE,
    notes = c("All models are fitted using OLS random-effects", "accounting for the clustering of considered", "days per event."),
    notes.align = "l",
    column.labels = c("in 1km radius", "in 5km radius", "in 10km radius", "in any radius"),
    model.names = FALSE,
    model.numbers = FALSE,
    out = "Writing/Term_Paper/regression_tab_5.tex",
    out.header = FALSE,
    add.lines = list(c("Interaction/Intercept", relations_5))
  )
  
  boros <- unique(data_final_long$reg)
  
  regress_boro <- function(boro){
    lmer_dat |>
      filter(dist == 1,
             reg == boro) |>
      pdata.frame(index = c("id", "time_to_sqf")) |>
      plm(formula = arrests ~ other_sqfs + pos * is_sqf,
          effect = "twoways",
          model = "random",
          data = _)
  }
  
  boro_fits <- lapply(boros, regress_boro)
  
  relations_boros <- 
    c(
      boro_fits[[1]]$coefficients["pos1:is_sqf1"] / boro_fits[[1]]$coefficients["(Intercept)"],
      boro_fits[[2]]$coefficients["pos1:is_sqf1"] / boro_fits[[2]]$coefficients["(Intercept)"],
      boro_fits[[3]]$coefficients["pos1:is_sqf1"] / boro_fits[[3]]$coefficients["(Intercept)"],
      boro_fits[[4]]$coefficients["pos1:is_sqf1"] / boro_fits[[4]]$coefficients["(Intercept)"],
      boro_fits[[5]]$coefficients["pos1:is_sqf1"] / boro_fits[[5]]$coefficients["(Intercept)"]
    ) |>
    round(3)
  
  
  stargazer(
    boro_fits,
    header = FALSE,
    title = "Random-effects regression models for boroughs",
    digit.separator = " ",
    intercept.top = TRUE,
    intercept.bottom = FALSE,
    no.space = FALSE,
    column.sep.width = "2pt",
    omit.stat = "ll",
    font.size = "footnotesize",
    dep.var.labels = "Number of Arrests \\vspace*{0.2cm}",
    covariate.labels = c("Intercept", "Other SQFs", "After Event", "SQF or Control", "SQF*After"),
    notes.append = TRUE,
    notes = c("All models are fitted using OLS random-effects", "accounting for the clustering of considered", "days per event."),
    notes.align = "l",
    column.labels = boros,
    model.names = FALSE,
    model.numbers = FALSE,
    out = "regression_tab_boros.tex",
    out.header = FALSE,
    add.lines = list(c("Interaction/Intercept", relations_boros))
  )
  
} else {
  load("Data/Clean/regressions_simple.Rda")
}
