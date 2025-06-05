# update_all_data.R -- run from pipeline repo root (includes explicit library calls)

# 0. Explicitly load required packages for scripts/functions and CI/CD workflow
# Libraries -----
#install.packages(".", repos = NULL, type = "source")
library(nflAnalysisPipeline)
#ls("package:nflAnalysisPipeline")

library(arrow)
library(qs)
library(dplyr)
library(tibble)
library(purrr)
library(nflfastR)
library(nflreadr)
library(nflseedR)
library(readr)
library(stringr)
library(tidyr)
library(glue)
library(slider)

# 1. Load all R/ functions
# if (interactive()) {
#   if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
#   devtools::load_all()
# }

# 2. Create artifacts/data and artifacts/data/models if they don't exist
dir.create("artifacts/data", recursive = TRUE, showWarnings = FALSE)
#dir.create("artifacts/data/models", recursive = TRUE, showWarnings = FALSE)

# 3. Universal variables
all_seasons <- 2006:nflreadr::most_recent_season()

id_cols <- c("game_id", "season", "week", "week_seq", "team", "opponent")

# 4. Data generation steps
# game_data --------
cat("%%%% Generating game_data %%%%
")
game_data <- compute_game_data(seasons = all_seasons)
arrow::write_parquet(game_data, "artifacts/data/game_data.parquet")

# game_data_long --------
cat("%%%% Generating game_data_long %%%%
")
game_data_long <- compute_game_data_long(game_df = game_data)
arrow::write_parquet(game_data_long, "artifacts/data/game_data_long.parquet")

# pbp_data --------
cat("%%%% Generating pbp_data %%%%
")
pbp_data <- compute_pbp_data(seasons = all_seasons)
#arrow::write_parquet(pbp_data, "artifacts/data/pbp_data.parquet")

# player_offense_data --------
cat("%%%% Generating player_offense_data %%%%
")
player_offense_data <- compute_player_data(
  seasons = all_seasons,
  game_long_df = game_data_long,
  stat = "offense"
)
arrow::write_parquet(player_offense_data, "artifacts/data/player_offense_data.parquet")

# season_standings_data --------
cat("%%%% Generating season_standings_data %%%%
")
season_standings_data <- compute_season_standings_data(
  game_df = game_data,
  tol = 1e-3,
  max_iter = 200,
  print_message = TRUE
)
arrow::write_dataset(
  season_standings_data,
  "artifacts/data/season_standings_data",
  format = "parquet",
  partitioning = "season",
  hive_style = TRUE
)

# weekly_standings_data --------
cat("%%%% Generating weekly_standings_data %%%%
")
weekly_standings_data <- compute_weekly_standings_data(
  game_data,
  tol = 1e-3,
  max_iter = 200,
  reset = TRUE,
  recompute_all = FALSE,
  cache_file = "artifacts/data/weekly_standings_data.rda" # for incremental update logic, if needed
)
save(weekly_standings_data, file = "artifacts/data/weekly_standings_data.rda")
arrow::write_dataset(
  weekly_standings_data,
  "artifacts/data/weekly_standings_data",
  format = "parquet",
  partitioning = "season",
  hive_style = TRUE
)



# elo_data --------
cat("%%%% Generating elo_data %%%%
")
elo_data <- compute_elo_data(
  game_df = game_data,
  initial_elo = 1500,
  K = 20,
  home_advantage = 0,
  d = 400,
  apply_margin_multiplier = TRUE,
  recompute_all = FALSE,
  cache_file = "artifacts/data/elo_data.rda",
  season_factor = 0.6
)
save(elo_data, file = "artifacts/data/elo_data.rda")
arrow::write_dataset(
  elo_data,
  "artifacts/data/elo_data",
  format = "parquet",
  partitioning = "season",
  hive_style = TRUE
)

# srs_data --------
cat("%%%% Generating srs_data %%%%
")
srs_data <- compute_srs_data(
  game_data,
  #resets = list(TRUE, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20),
  resets = c(TRUE, as.list(5:20)),
  tol = 1e-3,
  max_iter = 200,
  recompute_all = FALSE,
  cache_file = "artifacts/data/srs_data.rda"
)
save(srs_data, file = "artifacts/data/srs_data.rda")
arrow::write_dataset(
  srs_data,
  "artifacts/data/srs_data",
  format = "parquet",
  partitioning = "season",
  hive_style = TRUE
)

# epa_data --------
cat("%%%% Generating epa_data %%%%
")
epa_data <- compute_epa_data(
  pbp_df = pbp_data,
  scaled_wp = FALSE
)
save(epa_data, file = "artifacts/data/epa_data.rda")
arrow::write_dataset(
  epa_data,
  "artifacts/data/epa_data",
  format = "parquet",
  partitioning = "season",
  hive_style = TRUE
)

# scores_data --------
cat("%%%% Generating scores_data %%%%
")
scores_data <- compute_scores_data(
  game_long_df = game_data_long,
  pbp_df = pbp_data,
  seasons = all_seasons,
  sum_level = "week",
  stat_level = "team",
  season_level = "REG+POST",
  stats_loc = "artifacts/data/nflStatsWeek.rda",
  recompute_all = FALSE
)
save(scores_data, file = "artifacts/data/scores_data.rda")
arrow::write_dataset(
  scores_data,
  "artifacts/data/scores_data",
  format = "parquet",
  partitioning = "season",
  hive_style = TRUE
)

# series_data --------
cat("%%%% Generating series_data %%%%
")
series_data <- compute_series_data(
  game_data_long,
  pbp_data,
  series_loc = "artifacts/data/nflSeriesWeek.rda",
  recompute_all = FALSE
)
save(series_data, file = "artifacts/data/series_data.rda")
arrow::write_dataset(
  series_data,
  "artifacts/data/series_data",
  format = "parquet",
  partitioning = "season",
  hive_style = TRUE
)

# turnover_data --------
cat("%%%% Generating turnover_data %%%%
")
turnover_data <- compute_turnover_data(
  game_long_df = game_data_long,
  pbp_df = pbp_data
)
save(turnover_data, file = "artifacts/data/turnover_data.rda")
arrow::write_dataset(
  turnover_data,
  "artifacts/data/turnover_data",
  format = "parquet",
  partitioning = "season",
  hive_style = TRUE
)

# redzone_data --------
cat("%%%% Generating redzone_data %%%%
")
redzone_data <- compute_redzone_data(
  game_long_df = game_data_long,
  pbp_df = pbp_data
)
save(redzone_data, file = "artifacts/data/redzone_data.rda")
arrow::write_dataset(
  redzone_data,
  "artifacts/data/redzone_data",
  format = "parquet",
  partitioning = "season",
  hive_style = TRUE
)

# model_data_long --------
cat("%%%% Generating model_data_long %%%%
")
model_data_long <- compute_model_data_long(window = 5, span = 5)
arrow::write_dataset(
  model_data_long,
  "artifacts/data/model_data_long",
  format = "parquet",
  partitioning = "season",
  hive_style = TRUE
)

# model_data --------
cat("%%%% Generating model_data %%%%
")
model_data <- compute_model_data(model_data_long = model_data_long, game_data = game_data)
arrow::write_dataset(
  model_data,
  "artifacts/data/model_data",
  format = "parquet",
  partitioning = "season",
  hive_style = TRUE
)

cat("\n%%%% DATA UPDATE COMPLETE %%%%
")
