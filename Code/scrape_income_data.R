if (file.exists("Data/Clean/data_income_zipcode.R")) {
  load("Data/Clean/data_income_zipcode.R")
} else {
  require(needs)
  needs(rvest, RSelenium, tidyverse, wdman)

  zip_table <- list()
  for (i in 1:17) {
    url <- paste0("http://zipatlas.com/us/ny/zip-code-comparison/median-household-income.", i, ".htm")

    page <- read_html(url)

    zip_table[[i]] <- html_table(page)[[13]]
  }

  zip_table_df <- data.frame()

  for (i in 1:17) {
    df <- zip_table[i] |>
      as.data.frame()
    names(df) <- df %>%
      slice(1) %>%
      unlist()
    df <- df %>% slice(-1)

    zip_table_df <- bind_rows(zip_table_df, df)
  }

  data_income_zipcode <-
    zip_table_df |>
    mutate(
      `National Rank` = str_remove(`National Rank`, fixed("#")),
      `National Rank` = str_remove(`National Rank`, ","),
      `National Rank` = as.numeric(`National Rank`),
      ID = str_remove(`#`, fixed(".")),
      Income = str_remove(`Avg. Income/H/hold`, fixed("$")),
      Income = str_remove(Income, ","),
      Income = as.numeric(Income),
      Population = str_remove(Population, ","),
      Population = as.numeric(Population)) |>
    select(-c(`#`, `Avg. Income/H/hold`))

  save(data_income_zipcode, file = "Data/Clean/data_income_zipcode.R")
}
