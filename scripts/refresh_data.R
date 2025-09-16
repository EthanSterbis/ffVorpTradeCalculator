#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(nflreadr)
  library(dplyr)
  library(rlang)
})

SEASON_RANGE <- 2021:2025
REG_WEEKS    <- 1:18

dir.create("data", showWarnings = FALSE, recursive = TRUE)

ff <- nflreadr::load_ff_opportunity(seasons = SEASON_RANGE) %>%
  dplyr::rename(
    game_id   = dplyr::any_of("game_id"),
    player_id = dplyr::any_of("player_id"),
    full_name = dplyr::any_of(c("full_name","player_name")),
    posteam   = dplyr::any_of(c("posteam","team")),
    position  = dplyr::any_of(c("position","pos")),
    season    = dplyr::any_of("season"),
    week      = dplyr::any_of("week")
  ) %>%
  dplyr::filter(position %in% c("QB","RB","WR","TE"), week %in% REG_WEEKS)

saveRDS(ff, file = "data/ffopp.rds")
cat(sprintf("Wrote %s rows to data/ffopp.rds\n", nrow(ff)))
