library(ggplot2)
library(plotly)
library(corrplot)
library(xgboost)
library(caret)

# TO DO:
# Reconsider calculated strike zone?
# Get rid of outliers in incorrect strike/ball calls *BUG*
# Are inside strike calls that are strikes count as strike added frame?
# Take into account left on left?
# Model calc probability that strike is called
# Check outputs to make sure they make sense

data <- read.csv('/Users/ianchang/Downloads/ML_TAKES_ENCODED.csv')
View(data)


plot_strike_zone <- function(dr){
  bottom = dr$BOT_ZONE
  top = dr$TOP_ZONE
  # Width of strike zone is area over home plate (17 inch plate)
  left = -0.7083
  right = 0.7083
  ball_x = dr$PLATELOCSIDE
  ball_y = dr$PLATELOCHEIGHT
  
  ggplot(dr) +
    geom_rect(aes(
      xmin = left,
      xmax = right,
      ymin = bottom,
      ymax = top
    ),
    fill = NA, color = "blue", linewidth = 1.1
    ) +
    
    # Ball location
    geom_point(aes(
      x = ball_x,
      y = ball_y,
      color = PITCHCALL),
      size = 16
      ) +
    geom_point(aes(
      x = prior_x,
      y = prior_y),
      size = 16, alpha = 0.5
      ) +
    scale_color_manual(
      values = c(
        "StrikeCalled" = "red",
        "BallCalled"   = "blue"
      )
    ) +
    coord_fixed() + 
    labs(
      title = paste0("Pitch ", dr$PITCH_ID, " Location"),
      x = "Width (ft)",
      y = "Height (ft)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      axis.title = element_blank(), 
      legend.title = element_blank()
    )
}

plot_strike_zone(data[606437,])

#Identify pitch outliers
grep(max(data$PLATELOCHEIGHT), data$PLATELOCHEIGHT)
grep(min(data$PLATELOCHEIGHT), data$PLATELOCHEIGHT)
grep(max(data$PLATELOCSIDE), data$PLATELOCSIDE)
grep(min(data$PLATELOCSIDE), data$PLATELOCSIDE)
plot_strike_zone(data[1108610,])

grep(max(data$VERTAPPRANGLE), data$VERTAPPRANGLE)

p <- ggplot(df_merged, aes(x = pitch_location, y = pred_prob)) +
  geom_boxplot()

ggplotly(p)

colSums(is.na(data))

# Clean data
# Add dist from center, calculated call, binary cols, strikes added
# Create pitch loc col to filter pitches within shadow strike zone borders
# Ball Radius is 0.125 ft, implementing if edge of ball hits strike zone
data_cleaned <- data %>%
  filter(!is.na(TOP_ZONE), !is.na(BOT_ZONE), !is.na(EXTENSION), 
         STRIKES < 3, INDUCEDVERTBREAK < 1000) %>% 
  mutate(
    pitch_location = ifelse(
      (between(PLATELOCSIDE, -0.7083 - 0.5, 0.7083 + 0.5) &
         between(PLATELOCHEIGHT, BOT_ZONE - 0.5, TOP_ZONE + 0.5)) &
        !(between(PLATELOCSIDE, -0.7083 + 0.25, 0.7083 - 0.25) &
            between(PLATELOCHEIGHT, BOT_ZONE + 0.25, TOP_ZONE - 0.25)),
      'Shadow', 
      'Other'
    ),
    calculated_call = ifelse(
      (PLATELOCSIDE >= -0.7083 - 0.125) & 
        (PLATELOCSIDE <= 0.7083 + 0.125) &
        (PLATELOCHEIGHT >= BOT_ZONE - 0.125) &
        (PLATELOCHEIGHT <= TOP_ZONE + 0.125), 
      'StrikeCalled', 
      'BallCalled'
    ),
    prior_x = PLATELOCSIDE - (HORZBREAK / 12),
    prior_y = PLATELOCHEIGHT + (INDUCEDVERTBREAK / 12),
    movement_dist = sqrt((PLATELOCSIDE-prior_x)^2+(PLATELOCHEIGHT-prior_y)^2),
    dist_from_center = sqrt((PLATELOCSIDE-0)^2+(PLATELOCHEIGHT-(TOP_ZONE+BOT_ZONE)/2)^2),
    PitchCall_Binary = ifelse(PITCHCALL == 'StrikeCalled', 1, 0),
    Calc_Call_Binary = ifelse(calculated_call == 'StrikeCalled', 1, 0),
    StrikesAdded = PitchCall_Binary - Calc_Call_Binary
    ) %>% 
  arrange(dist_from_center)
# RELHEIGHT < 8, 
# INDUCEDVERTBREAK < 50, INDUCEDVERTBREAK > -30,
# HORZBREAK < 50, HORZAPPRANGLE < 50, VERTAPPRANGLE < 2) 
#%>% select(-c(EXTENSION))

# Gather output
new_output <- data_cleaned %>% 
  group_by(CATCHER_ID, GAME_YEAR) %>% 
  summarize(OPPORTUNITIES = n(),
            ACTUAL_CALLED_STRIKES = sum(PITCHCALL == 'StrikeCalled'),
            CALLED_STRIKES_ADDED = sum(StrikesAdded[pitch_location == 'Shadow']),
            CALLED_STRIKES_ADDED_PER_100 = sum(StrikesAdded[pitch_location == 'Shadow'])/n()*100) %>% 
  arrange(CATCHER_ID, GAME_YEAR)

# Verify results
test_catcher <- data_cleaned %>% 
  filter(CATCHER_ID == 2234, GAME_YEAR == 2022,
         PitchCall_Binary != Calc_Call_Binary,
         pitch_location == 'Shadow'
         ) %>% 
  arrange(dist_from_center)
plot_strike_zone(test_catcher[2,])

# Called strike but should be a ball
incorrect_strikes_called <- data_cleaned %>% 
  filter(PITCHCALL == 'StrikeCalled', calculated_call == 'BallCalled') %>% 
  arrange(dist_from_center)

# Called ball but should be a strike
incorrect_balls_called <- data_cleaned %>% 
  filter(PITCHCALL == 'BallCalled', calculated_call == 'StrikeCalled') %>% 
  arrange(dist_from_center)

catcher_1253_2023 <- data_borders %>% 
  filter(CATCHER_ID == 1253, GAME_YEAR == 2023) %>% 
  arrange(dist_from_center)
 
# Strikes added = calculated ball but called strike
# Strikes subtracted = calculated strike but called ball - SHOULD BE ON BORDER STRIKE BUT BALL
catcher_1253_2023frames <- catcher_1253_2023 %>% 
  filter(PITCHCALL != calculated_call) %>% 
  arrange(PITCHCALL, dist_from_center)
catcher_1253_2023frames <- catcher_1253_2023frames %>% 
  mutate(
    PitchCall_Binary = ifelse(PITCHCALL == 'StrikeCalled', 1, 0),
    Calc_Call_Binary = ifelse(calculated_call == 'StrikeCalled', 1, 0),
    StrikesAdded = PitchCall_Binary - Calc_Call_Binary
  )
  
grep(max(data_borders$PLATELOCSIDE), data_borders$PLATELOCSIDE)
plot_strike_zone(data_borders[294609,])

# Border called balls but should be strikes (can't really sub strike when framing within the box? only on border?)
# should include ball radius or nah
bug <- data_cleaned %>% 
  filter(PitchCall_Binary == 0, Calc_Call_Binary == 1) %>%
  arrange(dist_from_center)     

shadow <- data_cleaned %>% filter(pitch_location == 'Shadow') %>% 
  arrange(dist_from_center)

plot(
  shadow$PLATELOCSIDE,
  shadow$PLATELOCHEIGHT,
  asp = 1,
  xlab = "PLATELOCSIDE",
  ylab = "PLATELOCHEIGHT"
)

# Add strike-zone rectangle
rect(
  xleft  = -0.7083,
  ybottom = 1.5,
  xright =  0.7083,
  ytop   = 3.5,
  border = "blue",
  lwd = 5
)

ggplot(catcher1253_2023) +
  annotate(
    "rect",
    xmin = -0.7083 - 0.125,
    xmax = 0.7083 + 0.125,
    ymin = min(catcher1253_2023$PLATELOCHEIGHT),
    ymax = max(catcher1253_2023$PLATELOCHEIGHT),
    fill = '#DCE6F1',
    color = 'black',
    linewidth = 0
  ) +
  geom_rect(aes(
    xmin = -0.7083,
    xmax = 0.7083,
    ymin = 1.75,
    ymax = 3.42),
    fill = '#E8E8E8',
    color = '#555555',
    alpha = 0.5,
    linewidth = 1.1) +
  geom_point(
    aes(
      x = PLATELOCSIDE,
      y = PLATELOCHEIGHT,
      color = factor(StrikesAdded,
                     levels = c(-1, 1),
                     labels = c("Bad Frame (Ball Called)", 
                                "Good Frame (Strike Called)"))
    ),
    size = 3,
    alpha = 0.5
  ) +
  
  scale_color_manual(
    values = c(
      "Bad Frame (Ball Called)"  = '#5B9BD5',
      "Good Frame (Strike Called)" = '#FF4C4C'
    )
  ) +
  
  coord_fixed() +
  labs(
    title = "Catcher 2234 (2022) Plate Opportunities",
    color = "Framing Outcome"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_blank(), 
    legend.title = element_blank(),
    legend.position = "top"
  )

## XGBoost 

# Called strike but should be a ball
incorrect_strikes_called <- data_cleaned %>% 
  filter(StrikesAdded == 1) %>% 
  arrange(dist_from_center)

# Called ball but should be a strike
incorrect_balls_called <- data_cleaned %>% 
  filter(StrikesAdded == -1) %>% 
  arrange(dist_from_center)

bug_incrballs <- incorrect_balls_called %>% 
  filter(pitch_location == 'Other')
bug_incrstrikes <- incorrect_strikes_called %>% 
  filter(pitch_location == 'Other')

library(xgboost)
library(caret)
library(dplyr)
library(Matrix)

# Clean data
data_model <- data_cleaned %>%
  mutate(
    PitchCall_Binary = as.integer(PitchCall_Binary),
    AUTOPITCHTYPE = as.integer(as.factor(AUTOPITCHTYPE)),
    PI_PITCH_TYPE = as.integer(as.factor(PI_PITCH_TYPE)),
    BATTERSIDE = as.integer(as.factor(BATTERSIDE)),
    PITCHERTHROWS = as.integer(as.factor(PITCHERTHROWS)),
    # pitch_location = case_when(
    #   (between(PLATELOCSIDE, -0.7083 - 0.5, 0.7083 + 0.5) &
    #      between(PLATELOCHEIGHT, BOT_ZONE - 0.5, TOP_ZONE + 0.5)) &
    #     !(between(PLATELOCSIDE, -0.7083 + 0.25, 0.7083 - 0.25) &
    #         between(PLATELOCHEIGHT, BOT_ZONE + 0.25, TOP_ZONE - 0.25)) ~ 1, #Shadow
    #   (between(PLATELOCSIDE, -0.7083 + 0.25, 0.7083 - 0.25) &
    #      between(PLATELOCHEIGHT, BOT_ZONE + 0.25, TOP_ZONE - 0.25)) ~ 2, #Inside
    #   .default = 3 #Outside
    # )
  ) %>% 
  # filter(!(StrikesAdded %in% c(1, -1) & pitch_location == 0)) %>% 
  select(-c(PITCHCALL, calculated_call, RUNNER_ON_FIRST_ID, RUNNER_ON_SECOND_ID,
            RUNNER_ON_THIRD_ID, HOME_TEAM_ID, PITCHER_TEAM_ID, STADIUM_ID,
            GAMEID, PITCHER_ID, UMPIRE_ID,
            Calc_Call_Binary, StrikesAdded))

train_idx <- createDataPartition(data_model$PitchCall_Binary, p = 0.8, list = FALSE)
train <- data_model[train_idx, ]
test  <- data_model[-train_idx, ]

train <- data_model %>% filter(GAME_YEAR %in% c(2021, 2022)) %>% 
  select(-c(GAME_YEAR, PITCH_ID, CATCHER_ID, pitch_location))
test <- data_model %>% filter(GAME_YEAR == 2023) %>% 
  select(-c(GAME_YEAR, PITCH_ID, CATCHER_ID, pitch_location))


# GLM
X_train <- as.matrix(train %>% select(-PitchCall_Binary))
y_train <- train$PitchCall_Binary

X_test  <- as.matrix(test %>% select(-PitchCall_Binary))
y_test  <- test$PitchCall_Binary

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test,  label = y_test)

params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  gamma = 0
)

nrounds <- 300

set.seed(0)
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = nrounds,
  watchlist = list(train = dtrain),
  early_stopping_rounds = 20,
  print_every_n = 10
)

# Best iteration automatically tracked
best_iter <- xgb_model$best_iteration

importance <- xgb.importance(model = xgb_model)
xgb.plot.importance(importance)

pred_prob <- predict(xgb_model, newdata = dtest)
pred_class <- ifelse(pred_prob > 0.5, 1, 0)

rmse <- sqrt(mean((y_test - pred_prob)^2))
rmse

cm <- confusionMatrix(
  factor(pred_class, levels = c(0,1)),
  factor(y_test, levels = c(0,1))
)

precision <- cm$byClass["Precision"]
recall    <- cm$byClass["Recall"]

F1 <- 2 * (precision * recall) / (precision + recall)
cm_mtx <- as.data.frame(cm$table)

ggplot(cm_mtx, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 6, fontface = "bold") +
  scale_fill_gradient(low = "#D2B48C", high = "#FF4500") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Confusion Matrix",
    x = "Actual",
    y = "Predicted",
    fill = "Count"
  )

test$pred_prob <- pred_prob
test <- test %>% mutate(PITCHCALL = ifelse(PitchCall_Binary == 1, 'StrikeCalled', 'BallCalled'))
View(test)

df_merged <- merge(test, data_model)

df_merged <- df_merged %>% 
  mutate(IS_STRIKE = PitchCall_Binary,
         CS_PROB = pred_prob) %>% 
  select(PITCH_ID, IS_STRIKE, CS_PROB)

BOT_ZONE = 1.5
TOP_ZONE = 3.5
# Define key boundaries
left_shadow   <- -0.7083 - 0.5
right_shadow  <-  0.7083 + 0.5
bottom_shadow <- BOT_ZONE - 0.5
top_shadow    <- TOP_ZONE + 0.5

left_inside   <- -0.7083 + 0.25
right_inside  <-  0.7083 - 0.25
bottom_inside <- BOT_ZONE + 0.25
top_inside    <- TOP_ZONE - 0.25

# Plot rectangles in layers (inside on top)
ggplot() +
  
  # Shadow zone (bigger rectangle)
  geom_rect(
    aes(xmin = left_shadow, xmax = right_shadow,
        ymin = bottom_shadow, ymax = top_shadow),
    fill = "tan", alpha = 0.5
  ) +
  
  # Inside zone (smaller rectangle)
  geom_rect(
    aes(xmin = left_inside, xmax = right_inside,
        ymin = bottom_inside, ymax = top_inside),
    fill = "orangered3", alpha = 0.7
  ) +
  
  # Strike zone reference (optional)
  geom_rect(
    aes(xmin = -0.7083, xmax = 0.7083,
        ymin = BOT_ZONE, ymax = TOP_ZONE),
    color = "black", fill = NA, linewidth = 1.2
  ) +
  
  coord_fixed() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Inside (Red), Shadow (Tan), and Strike Zone Outline",
    x = "Plate Side (ft)",
    y = "Plate Height (ft)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

pitch_predictions <- pitch_predictions %>% 
  mutate(score = IS_STRIKE - CS_PROB)

strike_pitches <- pitch_predictions %>% filter(score > 0) %>% arrange(score)
ball_pitches <- pitch_predictions %>% filter(score < 0) %>% arrange

# Catcher probability plot
catcher1253_2023 <- data_model %>%
  filter(CATCHER_ID == 1167, GAME_YEAR == 2023)#,
         #pitch_location == 'Shadow', dist_from_center < 1)
catcher1253_2023 <- catcher1253_2023 %>% 
  left_join(., pitch_predictions %>% 
              select(PITCH_ID, CS_PROB, score), by = c('PITCH_ID'))
catcher1253_2023 <- catcher1253_2023 %>% filter(PitchCall_Binary == 0)

ggplot(catcher1253_2023) +
  
  # Background "shadow" rectangle
  annotate(
    "rect",
    xmin = -0.7083 - 0.125,
    xmax = 0.7083 + 0.125,
    ymin = min(catcher1253_2023$PLATELOCHEIGHT),
    ymax = max(catcher1253_2023$PLATELOCHEIGHT),
    fill = '#DCE6F1',
    color = 'black',
    linewidth = 0
  ) +
  
  # Strike zone rectangle
  geom_rect(
    aes(xmin = -0.7083, xmax = 0.7083, ymin = 1.75, ymax = 3.42),
    fill = '#E8E8E8',
    color = '#555555',
    alpha = 0.5,
    linewidth = 1.1
  ) +
  
  # Points colored by CS_PROB
  geom_point(
    aes(
      x = PLATELOCSIDE,
      y = PLATELOCHEIGHT,
      color = CS_PROB
    ),
    size = 3,
    alpha = 0.7
  ) +
  
  # Gradient color scale
  scale_color_gradient(low = "tan", high = "orangered3", limits = c(0,1)) +
  
  coord_fixed() +
  labs(
    title = "Catcher 1167 (2023) Plate Opportunities",
    color = "Called Strike Probability"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_blank(),
    legend.position = "top"
  )

new_data <- data %>% filter(GAME_YEAR == 2021)
# Convert to matrix
new_matrix <- xgb.DMatrix(as.matrix(new_data))

# Predict strike probability
new_pred <- predict(xgb_model, new_matrix)