library(tidyverse)
library(hubverse)

orig_hub <- "../FluSight-forecast-hub"
new_hub <- "."

orig_outputs <- hubData::connect_hub(orig_hub) |>
  dplyr::collect()

new_outputs <- hubData::connect_hub(new_hub) |>
  dplyr::collect()

combined_outputs <- orig_outputs |>
  dplyr::filter(output_type == "pmf") |>
  dplyr::inner_join(
    new_outputs |> dplyr::filter(output_type == "pmf"),
    by = join_by(reference_date, target, horizon, target_end_date, location,
                 output_type, output_type_id, model_id)
  ) |>
  dplyr::rename(value_orig = value.x, value_new = value.y) |>
  dplyr::mutate(
    value_diff = value_orig - value_new
  )

ggplot(data = combined_outputs) +
  geom_boxplot(mapping = aes(x = model_id, y = value_diff)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))

combined_outputs |>
  dplyr::arrange(model_id, location, reference_date, horizon, output_type_id)

combined_outputs |>
  dplyr::group_by(model_id) |>
  dplyr::summarize(
    prop_small_diff = mean(abs(value_diff) < 1e-2)
  ) |>
  as.data.frame()

combined_outputs |>
  dplyr::filter(model_id == "PSI-PROF") |>
  dplyr::slice_max(abs(value_diff), n = 10)

