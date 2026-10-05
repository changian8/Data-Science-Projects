# NAME: Ian Chang | NUMBER: 8040
# Load in required packages
library(readr)
library(tidyr)
library(dplyr)
library(Metrics)
library(sf)
library(ggplot2)
library(mgcv)
library(ranger)
library(data.table)
library(hexbin)
library(lightgbm)

setwd("~/Desktop/OKC_Proj_2027")

##### Helper Functions

# Load in data
train_df <- read_csv("training.csv.gz")
test_df <- read_csv("testing.csv.gz")

# Refactor data columns
train_df <- train_df |> mutate(
  outcome = factor(outcome, levels = c(TRUE, FALSE), labels = c("Make", "Miss")),
  contested = factor(contested),
  gamestate = as.factor(gamestate),
  shottype = as.factor(shottype),
  three = as.factor(three)
)

test_df <- test_df |> mutate(
  contested = factor(contested),
  gamestate = as.factor(gamestate),
  shottype = as.factor(shottype),
  three = as.factor(three)
)

##### Feature Engineering
# Add in angle feature, turn outcome column into binary 0/1
train_df <- train_df |> 
  mutate(
  # Arctan angle from location to hoop center in degrees
  angle_deg = atan2(locationy - 0, locationx - -41.75) * 180 / pi,
  
  # Make outcome column into binary 
  outcome = ifelse(outcome == "Make", 1, 0)
)

test_df <- test_df |> 
  mutate(
    angle_deg = atan2(locationy - 0, locationx - -41.75) * 180 / pi
  )

# Group shot locations into hexagonal bins using hexbin package
hb <- hexbin(train_df$locationx, train_df$locationy, xbins = 30, IDs = TRUE)

# Make hex group number as data column to make shot group identifiable
train_df$hex_number <- hb@cID

# Establish weights for each shot based on player shot volume 
train_df <- train_df |> 
  group_by(hex_number, shooter_id) |> 
  mutate(
    n_shots = n(),
    
    # Establish raw weight (n^0.4/n+6)
    raw_weight = (n_shots^0.4)/(n_shots+6)
  ) |> 
  ungroup() |> 
  group_by(hex_number) |> 
  mutate(
    # normalize the raw weight across each zone
    weight = raw_weight / mean(raw_weight)
  ) |> 
  ungroup()

# Add in shot region based on shot location
# 1 if paint shot, 2 if midrange, 3 if three pointer
train_df <- train_df |> 
  mutate(
    shot_region = case_when(
      distance <= 16 ~ 1, # paint
      distance <= 23.75 & three == FALSE ~ 2, # middy
      three == TRUE ~ 3, # three
      .default = NA
    )
  )

test_df <- test_df |> 
  mutate(
    shot_region = case_when(
      distance <= 16 ~ 1, # paint
      distance <= 23.75 & three == FALSE ~ 2, # middy
      three == TRUE ~ 3, # three
      .default = NA
    )
  )

##### Split into train test sets
# Partition 70/15/15 of shooters from each season into train/val/test sets
shooter_partitions <- train_df |>
  distinct(season_id, shooter_id) |>
  group_by(season_id) |>
  mutate(
    partition = sample(
      c("train", "val", "test"),
      size = n(),
      replace = TRUE,
      prob = c(0.70, 0.15, 0.15)
    )
  ) |>
  ungroup()

train_df <- train_df |>
  left_join(shooter_partitions, by = c("season_id", "shooter_id"))

train <- train_df |> filter(partition == "train")
val   <- train_df |> filter(partition == "val")
test  <- train_df |> filter(partition == "test")

# Get zone shooting percentages for each shooter
player_zone_summary <- train |> 
  group_by(season_id, shot_region, shooter_id) |> 
  summarise(
    makes = sum(outcome),
    shots = n(),
    pct = makes / shots,
    .groups = "drop"
  )

##### Bayesian Hierarchical Model
zone_priors <- player_zone_summary |> 
  # Filter to qualified shooters who have shot at least 30 shots in region
  filter(shots >= 30) |>
  group_by(season_id, shot_region) |> 
  summarise(
    mu = mean(pct),
    v  = var(pct),
    # Method of Moments Beta parameter derivation
    common_factor = (mu * (1 - mu) / v) - 1,
    alpha_0 = mu * common_factor,
    beta_0  = (1 - mu) * common_factor,
    .groups = "drop"
  )

# Calculate bayesian shooting percentage shrunk to zone league avg
train <- train |> 
  left_join(
    player_zone_summary  |>  select(season_id, shot_region, shooter_id, makes, shots),
    by = c("season_id", "shot_region", "shooter_id")
  ) |> 
  left_join(
    zone_priors |> select(season_id, shot_region, alpha_0, beta_0),
    by = c("season_id", "shot_region")
  ) |> 
  mutate(
    # Jackknife (LOO) method removing current shot
    makes_jk = makes - outcome,
    shots_jk = shots - 1,
    shrunk_skill = (makes_jk + alpha_0) / (shots_jk + alpha_0 + beta_0)
  ) |> 
  select(-makes, -shots, -alpha_0, -beta_0, -makes_jk, -shots_jk)

# Calculate bayesian shooting percentage shrunk on validation set using league zone average
val <- val |> 
  left_join(
    zone_priors |> select(season_id, shot_region, alpha_0, beta_0),
    by = c("season_id", "shot_region")
  ) |> 
  mutate(shrunk_skill = alpha_0 / (alpha_0 + beta_0)) |> 
  select(-alpha_0, -beta_0)

# Calculate bayesian shooting percentage shrunk on test set using league zone average
test <- test |> 
  left_join(
    zone_priors |> select(season_id, shot_region, alpha_0, beta_0),
    by = c("season_id", "shot_region")
  ) |> 
  mutate(shrunk_skill = alpha_0 / (alpha_0 + beta_0)) |> 
  select(-alpha_0, -beta_0)

test_df <- test_df |> 
  left_join(
    zone_priors |> 
      filter(season_id == "fe055") |> 
      select(shot_region, alpha_0, beta_0),
    by = "shot_region"
  ) |> 
  mutate(shrunk_skill = alpha_0 / (alpha_0 + beta_0)) |> 
  select(-alpha_0, -beta_0)

##### Model Training
# Establish feature columns
feature_cols = c(
  'num_contesters', 'contested', 'gamestate', 'shottype', 
  'distance', 'locationx', 'locationy', 'dribblesbefore', 'shotclock', 'closestdefdist', 
  'shooterspeed', 'angle_deg', 'shot_region', 'shrunk_skill')

# Establish monotonic constraints
monotone_constraints <- case_when(
  feature_cols == "closestdefdist" ~ 1L,
  feature_cols == "contested"      ~ -1L,
  feature_cols == "shrunk_skill"   ~ 1L,
  TRUE ~ 0L
)

dtrain <- lgb.Dataset(
  data = as.matrix(train[, feature_cols]),
  label = as.numeric(train$outcome),
  weight = as.numeric(train$weight),
  free_raw_data = FALSE
)

dval <- lgb.Dataset(
  data      = as.matrix(val[, feature_cols]),
  label     = as.numeric(val$outcome),
  reference = dtrain,
  free_raw_data = FALSE
)

params <- list(
  objective = "binary",
  metric = "binary_logloss",
  boosting_type = "gbdt",
  learning_rate = 0.05,
  num_leaves = 31L,
  min_data_in_leaf = 50L,
  monotone_constraints = monotone_constraints,
  verbose = -1L
)

valids <- list(train = dtrain, val = dval)

bst <- lgb.train(
  params = params,
  data = dtrain,
  nrounds = 1000L,
  valids = valids,
  early_stopping_rounds = 30L
)

##### Model Evaluation
# Test model on test set
test$pred_prob <- predict(bst, as.matrix(test[, feature_cols]))

# Test model on test DATAFRAME
test_df$make_prob <- predict(bst, as.matrix(test_df[, feature_cols]))

# Save test df as submission csv
test_df <- test_df |> select(shot_id, make_prob)
write.csv(test_df, "submission.csv", row.names = FALSE)

# Calculate Shot Skill
test <- test |> mutate(quality = outcome - pred_prob)

# Log loss on test set
final_test_loss <- logLoss(test$outcome, test$pred_prob)

# Log loss on test set if we guessed 50% on each shot
naive_baseline  <- logLoss(test$outcome, rep(mean(test$outcome), nrow(test)))

cat(sprintf("Test Log Loss: %.5f\n", final_test_loss))
cat(sprintf("Log Loss Delta: %.5f\n", naive_baseline - final_test_loss))

# Plot training log loss to see if our model overfits
train_logloss <- unlist(bst$record_evals$train$binary_logloss$eval)
plot(train_logloss)    

# Feature Importance plot
importance <- lgb.importance(bst, percentage = TRUE)
lgb.plot.importance(importance, top_n = 15, measure = "Gain")

# Add defender variable for evaluation
# Defender < 4ft is contested, > 4ft is open
test <- test |> mutate(defender = case_when(
  closestdefdist > 4 ~ 'Open',
  .default = 'Contested'))

# Calculate average shot skill gained and subtracted for each shooter, region, and defense
result <- test |> 
  group_by(season_id, shooter_id, shot_region, defender) |> 
  summarise(
    shot_skill_pos = mean(quality[quality > 0], na.rm = TRUE),
    shot_skill_neg = mean(quality[quality < 0], na.rm = TRUE),
    shots = n(),
    pct = mean(outcome),
    .groups = "drop"
  ) |> 
  arrange(desc(shot_skill_pos))

# Resulting dataframe of sufficient top/worst 3 point shooters
result_threes <- result |> filter(shot_region == 3, shots > 30, defender == 'Open') |> arrange(shot_skill_neg)

# Look at individual player shots
player_shots <- test |> 
  filter(season_id == "fe055", shooter_id == 'd804f7f166', shot_region == 3, defender == 'Open') |> 
  mutate(outcome = ifelse(outcome == 0, 'Miss', 'Make'))

# Plot shot charts
plot_court(player_shots)
hex_plot(player_shots, quality = T, 'Shot Quality Chart')
hex_plot(player_shots, quality = F, 'FG% Shot Chart')

##### Plotting Helper Functions
# Helper function to plot shooting % per hex bin
# Quality = T plots shot skill quality chart, if false it will plot FG% by zone
hex_plot <- function(test, quality = T, title){
  pct_by_hex <- test |> 
    group_by(season_id, shooter_id, hex_number) |> 
    summarise(shot_skill = mean(quality), shots = n(), .groups = "drop") |> 
    arrange(desc(shot_skill))
  
  hex_data <- data.frame(
    id = hb@cell, 
    x = hb@xcm,
    y = hb@ycm,
    count = hb@count
  ) |>
    left_join(pct_by_hex, by = c("id" = "hex_number")) |>
    filter(!is.na(shot_skill)) 
  
  make_hex <- function(cx, cy, size) {
    angles <- seq(0, 2 * pi, length.out = 7)[-7]
    data.frame(x = cx + size * cos(angles), y = cy + size * sin(angles))
  }
  
  max_size <- diff(range(test$locationx, na.rm = TRUE)) / 20
  hex_data$size <- max_size * sqrt(hex_data$shots / max(hex_data$shots, na.rm = TRUE))
  
  hex_polys <- hex_data |>
    rowwise() |>
    reframe(make_hex(x, y, size), id = id, shot_skill = shot_skill, .groups = "drop")
  
  hex_polys <- hex_polys |>
    mutate(
      skill_category = case_when(
        shot_skill > 0.5  ~ "High",
        shot_skill < 0.3  ~ "Low",
        TRUE              ~ "Mid"
      )
    )
  
  if (quality)
  {
    ggplot() +
      geom_polygon(
        data = hex_polys,
        aes(x = x, y = y, group = id, fill = shot_skill),
        color = "black", linewidth = 0, alpha = 1
      ) +
      scale_fill_gradient2(
        low = "#2166ac",
        mid = "white",
        high = "#b2182b",
        midpoint = 0,
        limits = c(-1, 1),
        name = 'Shot Skill'
      ) +
      geom_sf(data = court_sf, fill = NA, color = 'white') +
      theme_void() +
      labs(title = title) +
      theme(
        plot.title = element_text(
          size = 18,
          face = "bold",
          hjust = 0.5,
          margin = margin(b = 10),
          color = "white"          # single color value, not nested duplicate calls
        ),
        plot.background = element_rect(fill = "black", color = NA),
        panel.background = element_rect(fill = "black", color = NA),
        legend.background = element_rect(fill = "black", color = NA),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white")
      )
  }
  
  else{
    test <- test |> mutate(outcome = ifelse(outcome == 'Make', 1, 0))
    ggplot() +
      stat_summary_hex(
        data = test,
        aes(x = locationx, y = locationy, z = outcome),
        fun = mean,
        bins = 20, color = "black", linewidth = 0.1, alpha = 0.6
      ) +
      scale_fill_gradient(
        low = "red", high = "green",
        labels = scales::percent,
        name = "FG%"
      ) +
      geom_sf(data = court_sf, fill = NA) +
      theme_void() +
      labs(title = title) +
      theme(
        plot.title = element_text(
          size = 18,
          face = "bold",
          hjust = 0.5, 
          margin = margin(b = 10),
          color = "gray20"
        )
      )
  }
}

##### Court Plot
line_thick = 0.1 
backboard_thick = 0.3
rim_thick = 0.03

half_court_int <- rbind(
  c(0, -25),
  c(0, 25),
  c(-47, 25),
  c(-47, -25),
  c(0, -25)
)

half_court_ext <- rbind(
  c(0 + line_thick, -25 - line_thick),
  c(0 + line_thick, 25 + line_thick),
  c(-47 - line_thick, 25 + line_thick),
  c(-47 - line_thick, -25 - line_thick),
  c(0 + line_thick, -25 - line_thick)
)

half_court <- st_polygon(list(half_court_ext, half_court_int))

key_int <- rbind(
  c(-47, 8 - line_thick),
  c(-28 - line_thick, 8 - line_thick),
  c(-28 - line_thick, -8 + line_thick),
  c(-47 - line_thick, -8 + line_thick),
  c(-47, 8 - line_thick)
)

key_ext <- rbind(
  c(-47, 8),
  c(-28, 8),
  c(-28, -8),
  c(-47, -8),
  c(-47, 8)
)

key <- st_polygon(list(key_ext, key_int))

backboard_points <- rbind(
  c(-43, 3),
  c(-43 + backboard_thick, 3),
  c(-43 + backboard_thick, -3),
  c(-43, -3),
  c(-43, 3)
)

backboard <- st_polygon(list(backboard_points))

hoop_center <- st_point(c(-41.75, 0))

hoop_int <- hoop_center %>%
  st_buffer(dist = 0.75)

hoop_ext <- hoop_center %>%
  st_buffer(dist = 0.8)

hoop <- st_polygon(list(
  st_coordinates(hoop_ext)[ , 1:2],
  st_coordinates(hoop_int)[ , 1:2]
))

key_center <- st_point(c(-28, 0))

key_circle_int <- st_crop(
  st_sfc(st_buffer(key_center, dist = 6 - line_thick)),
  xmin = -28, ymin = -6, xmax = -22, ymax = 6
)

key_circle_ext <- st_crop(
  st_sfc(st_buffer(key_center, dist = 6)),
  xmin = -28, ymin = -6, xmax = -22, ymax = 6
)

key_circle <- st_polygon(list(
  st_coordinates(key_circle_ext)[ , 1:2],
  st_coordinates(key_circle_int)[ , 1:2]
))

half_center <- st_point(c(0, 0))

half_circle_int <- st_crop(
  st_sfc(st_buffer(half_center, dist = 6 - line_thick)),
  xmin = -6, ymin = -6, xmax = 0, ymax = 6
)

half_circle_ext <- st_crop(
  st_sfc(st_buffer(half_center, dist = 6)),
  xmin = -6, ymin = -6, xmax = 0, ymax = 6
)

half_circle <- st_polygon(list(
  st_coordinates(half_circle_ext)[ , 1:2],
  st_coordinates(half_circle_int)[ , 1:2]
))

cx    <- -41.75
r_out <- 23.75
r_in  <- r_out - line_thick

a_out <- atan2(22, -32.8 - cx) 
a_in  <- asin((22 - line_thick) / r_in) 

theta_out <- seq(a_out, -a_out, length.out = 100) 
theta_in  <- seq(-a_in,  a_in,  length.out = 100) 

outer_arc <- cbind(cx + r_out * cos(theta_out), r_out * sin(theta_out))
inner_arc <- cbind(cx + r_in  * cos(theta_in),  r_in  * sin(theta_in))

three_point_line <- st_polygon(list(rbind(
  c(-47, 22),
  outer_arc, 
  c(-47, -22),
  c(-47, -22 + line_thick),
  inner_arc, 
  c(-47, 22 - line_thick),
  c(-47, 22)  
)))

court_sf <- st_sf(
  geom = c(st_geometry(half_court), st_geometry(key), st_geometry(hoop),
           st_geometry(backboard), st_geometry(key_circle), 
           st_geometry(three_point_line), st_geometry(half_circle))
)

plot_court <- function(shots = NULL) {
  p <- ggplot() +
    geom_sf(data = court_sf, fill = NA)
  
  if (!is.null(shots)) {
    p <- p +
      geom_point(
        data = shots,
        aes(x = locationx, y = locationy, color = outcome),
        size = 2, alpha = 0.2
      ) +
      scale_color_manual(
        values = c("Make" = "green", "Miss" = "red"),
        name = NULL
      )
  }
  
  p + theme_void()
}