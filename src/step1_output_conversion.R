library(tidyverse)
library(hubverse)
library(idforecastutils)

source_mo <- "../FluSight-forecast-hub/model-output"
target_mo <- "model-output"

location_meta <- readr::read_csv("auxiliary-data/locations.csv")

convert_mo_to_cat <- function(model, submission) {
  ref_date <- substr(submission, 1, 10)
  data_file_date <- as.Date(ref_date) - 7L
  target_ts <- readr::read_csv(
    file.path(
      "auxiliary-data/target-data-archive",
      paste0("target-hospital-admissions_", data_file_date, ".csv")
    ),
    col_types = readr::cols(
      date = readr::col_date(format = ""),
      location = readr::col_character(),
      location_name = readr::col_character(),
      value = readr::col_double(),
      weekly_rate = readr::col_double()
    )
  )

  q_mo <- readr::read_csv(
    file.path(source_mo, model, submission),
    col_types = readr::cols(
      reference_date = readr::col_date(format = ""),
      target = readr::col_character(),
      horizon = readr::col_double(),
      target_end_date = readr::col_date(format = ""),
      location = readr::col_character(),
      output_type = readr::col_character(),
      output_type_id = readr::col_character(),
      value = readr::col_double()
    )
  ) |>
    dplyr::filter(output_type == "quantile", horizon >= 0) |>
    dplyr::mutate(
      model_id = model
    )

  if (nrow(q_mo) == 0) {
    return(NULL)
  }

  bin_endpoints <- get_flusight_bin_endpoints(
    target_ts, location_meta, season = "2023/24"
  )
  cat_mo <- transform_quantile_to_pmf(
    q_mo |> dplyr::mutate(output_type_id = as.numeric(output_type_id)),
    bin_endpoints
  ) |>
    dplyr::mutate(target = "wk flu hosp rate change")

  return(dplyr::bind_rows(q_mo, cat_mo))
}

models <- list.dirs(source_mo, full.names = FALSE, recursive = FALSE)
model <- models[1]
for (model in models) {
  message(model)
  submissions <- list.files(file.path(source_mo, model))
  submission <- submissions[1]
  for (submission in submissions) {
    message(submission)
    cat_mo <- convert_mo_to_cat(model, submission)
    if (!is.null(cat_mo)) {
      save_dir <- file.path(target_mo, model)
      if (!dir.exists(save_dir)) {
        dir.create(save_dir, recursive = TRUE)
      }
      readr::write_csv(
        cat_mo |> dplyr::select(-model_id),
        file = file.path(target_mo, model, submission)
      )
    }
  }
}
