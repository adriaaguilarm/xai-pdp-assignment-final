library(dplyr)
library(ggplot2)
library(readr)
library(ranger)
library(scales)
library(tidyr)

set.seed(2024)

data_dir <- "data"
figures_dir <- "outputs/figures"
tables_dir <- "outputs/tables"

dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

rf_settings <- list(
  num_trees = 500,
  min_node_size = 5
)

factor_columns <- function(data, columns) {
  data %>%
    mutate(across(all_of(columns), as.factor))
}

model_metrics <- function(model, dataset, target, training_n, pdp_n, mtry) {
  tibble(
    dataset = dataset,
    target = target,
    training_n = training_n,
    pdp_n = pdp_n,
    num_trees = rf_settings$num_trees,
    mtry = mtry,
    min_node_size = rf_settings$min_node_size,
    oob_rmse = sqrt(model$prediction.error),
    oob_r_squared = model$r.squared
  )
}

feature_importance <- function(model, dataset) {
  tibble(
    dataset = dataset,
    feature = names(model$variable.importance),
    importance = as.numeric(model$variable.importance)
  ) %>%
    arrange(dataset, desc(importance))
}

numeric_grid <- function(data, feature, grid_size = 50) {
  observed_range <- range(data[[feature]], na.rm = TRUE)
  seq(observed_range[1], observed_range[2], length.out = grid_size)
}

pdp_1d <- function(model, sample_data, feature, grid_values, dataset) {
  bind_rows(lapply(grid_values, function(value) {
    modified_data <- sample_data
    modified_data[[feature]] <- value

    tibble(
      dataset = dataset,
      feature = feature,
      feature_value = value,
      prediction = mean(predict(model, data = modified_data)$predictions),
      pdp_n = nrow(sample_data)
    )
  }))
}

pdp_2d <- function(model, sample_data, feature_x, feature_y, grid_x, grid_y, dataset) {
  grid <- expand_grid(
    feature_x_value = grid_x,
    feature_y_value = grid_y
  )

  bind_rows(lapply(seq_len(nrow(grid)), function(row_id) {
    modified_data <- sample_data
    modified_data[[feature_x]] <- grid$feature_x_value[row_id]
    modified_data[[feature_y]] <- grid$feature_y_value[row_id]

    tibble(
      dataset = dataset,
      feature_x = feature_x,
      feature_y = feature_y,
      feature_x_value = grid$feature_x_value[row_id],
      feature_y_value = grid$feature_y_value[row_id],
      prediction = mean(predict(model, data = modified_data)$predictions),
      pdp_n = nrow(sample_data)
    )
  }))
}

pdp_summary_1d <- function(pdp_data) {
  pdp_data %>%
    group_by(dataset, feature) %>%
    summarise(
      analysis = "one-dimensional",
      feature_x = first(feature),
      feature_y = NA_character_,
      grid_points = n(),
      pdp_n = max(pdp_n),
      min_x = min(feature_value),
      max_x = max(feature_value),
      min_y = NA_real_,
      max_y = NA_real_,
      min_prediction = min(prediction),
      max_prediction = max(prediction),
      prediction_range = max(prediction) - min(prediction),
      .groups = "drop"
    ) %>%
    select(dataset, analysis, feature_x, feature_y, grid_points, pdp_n, everything(), -feature)
}

pdp_summary_2d <- function(pdp_data) {
  pdp_data %>%
    group_by(dataset, feature_x, feature_y) %>%
    summarise(
      analysis = "two-dimensional",
      grid_points = n(),
      pdp_n = max(pdp_n),
      min_x = min(feature_x_value),
      max_x = max(feature_x_value),
      min_y = min(feature_y_value),
      max_y = max(feature_y_value),
      min_prediction = min(prediction),
      max_prediction = max(prediction),
      prediction_range = max(prediction) - min(prediction),
      .groups = "drop"
    ) %>%
    select(dataset, analysis, feature_x, feature_y, grid_points, pdp_n, everything())
}

theme_pdp <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "#4b5563"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#e5e7eb", linewidth = 0.35),
      axis.title = element_text(color = "#111827"),
      axis.text = element_text(color = "#374151"),
      legend.title = element_text(color = "#111827"),
      legend.text = element_text(color = "#374151")
    )
}

save_plot <- function(plot, filename, width = 7.2, height = 4.8) {
  ggsave(
    filename = file.path(figures_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}

plot_pdp_1d <- function(pdp_data, sample_data, feature, title, x_label, y_label, filename) {
  feature_data <- pdp_data %>%
    filter(.data$feature == !!feature)

  plot <- ggplot(feature_data, aes(x = feature_value, y = prediction)) +
    geom_line(color = "#1f5fbf", linewidth = 1.1) +
    geom_point(color = "#123f7a", size = 1.6) +
    geom_rug(
      data = sample_data,
      aes(x = .data[[feature]]),
      inherit.aes = FALSE,
      sides = "b",
      color = "#7a5c3f",
      alpha = 0.65,
      length = unit(0.035, "npc")
    ) +
    scale_y_continuous(labels = comma) +
    labs(
      title = title,
      subtitle = "Partial dependence from the fitted random forest model",
      x = x_label,
      y = y_label
    ) +
    theme_pdp()

  save_plot(plot, filename)
}

plot_pdp_2d <- function(pdp_data, sample_data, filename) {
  tile_width <- min(diff(sort(unique(pdp_data$feature_x_value))))
  tile_height <- min(diff(sort(unique(pdp_data$feature_y_value))))

  plot <- ggplot(pdp_data, aes(x = feature_x_value, y = feature_y_value, fill = prediction)) +
    geom_tile(width = tile_width, height = tile_height) +
    geom_rug(
      data = sample_data,
      aes(x = temp, y = hum),
      inherit.aes = FALSE,
      sides = "bl",
      color = "#7a5c3f",
      alpha = 0.75,
      length = unit(0.035, "npc")
    ) +
    scale_fill_gradientn(
      colours = c("#101820", "#1f3b73", "#3d73c7", "#7fb3ff"),
      labels = comma,
      name = "Predicted\nrentals"
    ) +
    scale_x_continuous(expand = expansion(mult = 0.01)) +
    scale_y_continuous(expand = expansion(mult = 0.01)) +
    labs(
      title = "Bike Rentals 2D PDP: Normalized Temperature and Humidity",
      subtitle = "Rugs show the 50 sampled observations used for PDP calculation",
      x = "Normalized temperature (dataset scale)",
      y = "Normalized humidity (dataset scale)"
    ) +
    theme_pdp() +
    theme(
      panel.grid = element_blank(),
      legend.position = "right"
    )

  save_plot(plot, filename, width = 8.2, height = 5.3)
}

# Exercise 1: one-dimensional PDPs for bike rental demand
bike_raw <- read_csv(file.path(data_dir, "day.csv"), show_col_types = FALSE)

bike_model_data <- bike_raw %>%
  mutate(
    dteday = as.Date(dteday),
    days_since_2011 = as.integer(dteday - as.Date("2011-01-01"))
  ) %>%
  factor_columns(c("season", "yr", "mnth", "holiday", "weekday", "workingday", "weathersit")) %>%
  select(
    cnt,
    days_since_2011,
    season,
    yr,
    mnth,
    holiday,
    weekday,
    workingday,
    weathersit,
    temp,
    hum,
    windspeed
  ) %>%
  drop_na()

bike_features <- setdiff(names(bike_model_data), "cnt")
bike_mtry <- max(1, floor(sqrt(length(bike_features))))

bike_rf <- ranger(
  formula = cnt ~ .,
  data = bike_model_data,
  num.trees = rf_settings$num_trees,
  mtry = bike_mtry,
  min.node.size = rf_settings$min_node_size,
  importance = "permutation",
  respect.unordered.factors = "order",
  seed = 2024
)

set.seed(2024)
bike_pdp_sample <- bike_model_data %>%
  slice_sample(n = 50)

bike_pdp_features <- c("days_since_2011", "temp", "hum", "windspeed")
bike_pdp_1d <- bind_rows(lapply(bike_pdp_features, function(feature) {
  pdp_1d(
    model = bike_rf,
    sample_data = bike_pdp_sample,
    feature = feature,
    grid_values = numeric_grid(bike_model_data, feature),
    dataset = "Daily bike rentals"
  )
}))

bike_metrics <- model_metrics(
  model = bike_rf,
  dataset = "Daily bike rentals",
  target = "cnt",
  training_n = nrow(bike_model_data),
  pdp_n = nrow(bike_pdp_sample),
  mtry = bike_mtry
)

bike_importance <- feature_importance(bike_rf, "Daily bike rentals")
bike_pdp_summary <- pdp_summary_1d(bike_pdp_1d)

write_csv(bike_metrics, file.path(tables_dir, "model_metrics.csv"))
write_csv(bike_importance, file.path(tables_dir, "feature_importance.csv"))
write_csv(bike_pdp_summary, file.path(tables_dir, "pdp_summary.csv"))
write_csv(bike_pdp_1d, file.path(tables_dir, "bike_pdp_1d.csv"))

plot_pdp_1d(
  pdp_data = bike_pdp_1d,
  sample_data = bike_pdp_sample,
  feature = "days_since_2011",
  title = "Bike Rentals PDP: Days Since 2011",
  x_label = "Days since 2011-01-01",
  y_label = "Predicted daily rentals",
  filename = "bike_pdp_days_since_2011.png"
)

plot_pdp_1d(
  pdp_data = bike_pdp_1d,
  sample_data = bike_pdp_sample,
  feature = "temp",
  title = "Bike Rentals PDP: Normalized Temperature",
  x_label = "Normalized temperature (dataset scale)",
  y_label = "Predicted daily rentals",
  filename = "bike_pdp_temperature.png"
)

plot_pdp_1d(
  pdp_data = bike_pdp_1d,
  sample_data = bike_pdp_sample,
  feature = "hum",
  title = "Bike Rentals PDP: Normalized Humidity",
  x_label = "Normalized humidity (dataset scale)",
  y_label = "Predicted daily rentals",
  filename = "bike_pdp_humidity.png"
)

plot_pdp_1d(
  pdp_data = bike_pdp_1d,
  sample_data = bike_pdp_sample,
  feature = "windspeed",
  title = "Bike Rentals PDP: Normalized Windspeed",
  x_label = "Normalized windspeed (dataset scale)",
  y_label = "Predicted daily rentals",
  filename = "bike_pdp_windspeed.png"
)

# Exercise 2: two-dimensional PDP for normalized temperature and humidity
bike_pdp_temp_hum_2d <- pdp_2d(
  model = bike_rf,
  sample_data = bike_pdp_sample,
  feature_x = "temp",
  feature_y = "hum",
  grid_x = numeric_grid(bike_model_data, "temp", grid_size = 25),
  grid_y = numeric_grid(bike_model_data, "hum", grid_size = 25),
  dataset = "Daily bike rentals"
)

pdp_summary <- bind_rows(
  bike_pdp_summary,
  pdp_summary_2d(bike_pdp_temp_hum_2d)
)

write_csv(pdp_summary, file.path(tables_dir, "pdp_summary.csv"))
write_csv(bike_pdp_temp_hum_2d, file.path(tables_dir, "bike_pdp_temp_hum_2d.csv"))

plot_pdp_2d(
  pdp_data = bike_pdp_temp_hum_2d,
  sample_data = bike_pdp_sample,
  filename = "bike_pdp_2d_temperature_humidity_heatmap.png"
)

message("Exercises 1 and 2 complete.")
