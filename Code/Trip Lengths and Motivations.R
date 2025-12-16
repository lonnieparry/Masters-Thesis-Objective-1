library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(lubridate)
library(broom)


mode_colors <- c(
  "Boat"  = "#4B8BC8",  # blue
  "Shore" = "#3BAA36"   # green
)

creel <- read.csv("cleanedcreeltable.csv") %>%
  dplyr::mutate(
    dt_return = suppressWarnings(lubridate::ymd_hm(dateTimeReturnInterview)),
    dt_left   = suppressWarnings(lubridate::ymd_hm(dateTimeLeftLanding)),
    
    # make sure this is numeric hours (if it's already hours, great)
    timeNotFishing_num = suppressWarnings(as.numeric(timeNotFishing)),
    
    trip_hours_total   = as.numeric(difftime(dt_return, dt_left, units = "hours")),
    trip_hours_fishing = trip_hours_total - timeNotFishing_num,
    
    valid_trip = !is.na(trip_hours_fishing) &
      trip_hours_fishing >= 0 &
      trip_hours_fishing <= 24)

urban <- creel %>%
  dplyr::filter(
    Region %in% c("Great Lakes", "Inland"),
    FishingMode %in% c("Boat", "Shore")
  ) %>%
  dplyr::mutate(
    Region = factor(Region, levels = c("Great Lakes", "Inland")),
    FishingMode = factor(FishingMode, levels = c("Boat", "Shore")))


#TRIP LENGTH: model + plot
-
trip_mod <- lm(trip_hours_fishing ~ Region * FishingMode,
               data = urban %>% dplyr::filter(valid_trip))

trip_results <- broom::tidy(trip_mod, conf.int = TRUE)

trip_plot_df <- urban %>%
  dplyr::filter(valid_trip, !is.na(site_method)) %>%
  dplyr::mutate(
    trip_bin = cut(
      trip_hours_fishing,
      breaks = 0:11,
      labels = paste0(0:10, "–", 1:11, " hr"),
      include.lowest = TRUE,
      right = FALSE
    ),
    site_method = factor(
      site_method,
      levels = c("Inland Boat", "Inland Shore", "Great Lakes Boat", "Great Lakes Shore")
    )
  ) %>%
  dplyr::count(site_method, FishingMode, trip_bin) %>%
  dplyr::group_by(site_method) %>%
  dplyr::mutate(prop = n / sum(n)) %>%
  dplyr::ungroup()

mean_trips <- urban %>%
  dplyr::filter(valid_trip, !is.na(site_method)) %>%
  dplyr::group_by(site_method) %>%
  dplyr::summarise(mean_hours = mean(trip_hours_fishing, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    mean_label = sprintf("%02d:%02d", floor(mean_hours), round((mean_hours %% 1) * 60)),
    trip_bin = factor("0–1 hr", levels = paste0(0:10, "–", 1:11, " hr")) # anchor label at first bin)

trip_fig <- ggplot(trip_plot_df, aes(trip_bin, prop, fill = FishingMode)) +
  geom_col() +
  facet_wrap(~ site_method, nrow = 1) +
  geom_text(
    data = mean_trips,
    aes(x = trip_bin, y = Inf, label = mean_label),
    inherit.aes = FALSE,
    vjust = 1.4, hjust = -0.1,
    fontface = "bold", size = 5
  ) +
  scale_fill_manual(values = mode_colors) +
  labs(
    title = "Distribution of Fishing Trip Lengths by Fishing Mode Across Urban Systems",
    x = "Trip Length (hours)",
    y = "Proportion of Anglers",
    fill = "Fishing Mode"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ASSUMPTION CHECKS: Trip length


# Fit the model (adjust variable names if yours differ)
trip_mod <- lm(trip_hours_fishing ~ Region * FishingMode, data = urban)

# 1) Diagnostic plots (Residuals vs Fitted, QQ, Scale-Location, Leverage)
pdf("TripLength_AssumptionChecks.pdf", width = 11, height = 8.5)
par(mfrow = c(2, 2))
plot(trip_mod)
mtext("Trip Length Model Diagnostics: trip_hours_fishing ~ Region * FishingMode",
      outer = TRUE, line = -1, cex = 1.1)
dev.off()

# 2) Normality test on residuals (note: very sensitive with large n)
shapiro.test(residuals(trip_mod))
#Shapiro-Wilk normality test data:  residuals(trip_mod) W = 0.90799, p-value < 2.2e-16

# 3) Homoscedasticity (Breusch-Pagan test)
if (!requireNamespace("lmtest", quietly = TRUE)) install.packages("lmtest")
lmtest::bptest(trip_mod)
# Breusch-Pagan test data:  trip_mod BP = 17.673, df = 3, p-value = 0.0005137

# 4) Influential points (Cook's distance)
cooks <- cooks.distance(trip_mod)
summary(cooks)

# Optional: list most influential rows
order(cooks, decreasing = TRUE)[1:10]


# 3) MOTIVATIONS MADE INTO CATEGORIES
urban <- urban %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    centrality_nonNA = sum(!is.na(c_across(c(
      c1LifeRevolves, c3MostEnjoyable, c4SignificantIncome, c6NoTimeHobbies,
      c8NoAlternative, c9FriendsFish, c11LoseFriends, c13OtherLeisure
    )))),
    Centrality = dplyr::if_else(
      centrality_nonNA >= 5,
      mean(c_across(c(
        c1LifeRevolves, c3MostEnjoyable, c4SignificantIncome, c6NoTimeHobbies,
        c8NoAlternative, c9FriendsFish, c11LoseFriends, c13OtherLeisure
      )), na.rm = TRUE),
      NA_real_
    ),
    
    catch_nonNA = sum(!is.na(c_across(c(c5Enjoyable, c7MaximizeNumbers, c14KeepFishing)))),
    CatchOrientation = dplyr::if_else(
      catch_nonNA >= 2,
      mean(c_across(c(c5Enjoyable, c7MaximizeNumbers, c14KeepFishing)), na.rm = TRUE),
      NA_real_
    ),
    
    trophy_nonNA = sum(!is.na(c_across(c(c2TrophySpots, c15SpecialGear, c12BigSmall)))),
    TrophyOrientation = dplyr::if_else(
      trophy_nonNA >= 2,
      mean(c_across(c(c2TrophySpots, c15SpecialGear, c12BigSmall)), na.rm = TRUE),
      NA_real_
    ),
    
    HarvestOrientation = dplyr::if_else(!is.na(c10ReleaseFish), c10ReleaseFish, NA_real_)
  ) %>%
  dplyr::ungroup()

mot_long <- urban %>%
  dplyr::select(
    Region, FishingMode,
    Centrality, CatchOrientation, TrophyOrientation, HarvestOrientation
  ) %>%
  tidyr::pivot_longer(
    cols = c(Centrality, CatchOrientation, TrophyOrientation, HarvestOrientation),
    names_to = "MotivationCategory",
    values_to = "Score"
  ) %>%
  dplyr::filter(!is.na(Score)) %>%
  dplyr::mutate(
    MotivationCategory = factor(
      MotivationCategory,
      levels = c("Centrality","CatchOrientation","TrophyOrientation","HarvestOrientation"),
      labels = c("Centrality","Catch Orientation","Trophy Orientation","Harvest Orientation")))

# Models per category
mot_models <- mot_long %>%
  dplyr::group_by(MotivationCategory) %>%
  dplyr::do(broom::tidy(lm(Score ~ Region * FishingMode, data = .), conf.int = TRUE)) %>%
  dplyr::ungroup()

# Predictions for interaction figure
preds <- mot_long %>%
  dplyr::group_by(MotivationCategory) %>%
  dplyr::do({
    m <- lm(Score ~ Region * FishingMode, data = .)
    nd <- expand.grid(
      Region = levels(mot_long$Region),
      FishingMode = levels(mot_long$FishingMode))
    cbind(nd, predict(m, nd, interval = "confidence"))
  }) %>%
  dplyr::ungroup()

mot_interaction_fig <- ggplot(preds, aes(Region, fit, group = FishingMode, color = FishingMode)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.1) +
  scale_color_manual(values = mode_colors) +
  facet_wrap(~ MotivationCategory, ncol = 2) +
  labs(
    title = "Region × Fishing Mode Effects on Motivation Scores",
    x = "Urban System",
    y = "Predicted Motivation Score",
    color = "Fishing Mode") +
  theme_minimal(base_size = 14)

# 4) ASSUMPTION CHECKS
# Saves ALL diagnostic plots into one PDF.

pdf("Objective1_AssumptionChecks.pdf", width = 11, height = 8.5)

# --- Trip length diagnostics ---
par(mfrow = c(2,2))
plot(trip_mod, main = "Trip Length Model: Region * FishingMode")

-
check_mot_model <- function(cat_name) {
  dat <- dplyr::filter(mot_long, MotivationCategory == cat_name)
  m <- lm(Score ~ Region * FishingMode, data = dat)
  
  par(mfrow = c(2,2))
  plot(m, main = paste("Motivation:", cat_name))
  
  cat("\n=============================\n")
  cat("Motivation category:", as.character(cat_name), "\n")
  cat("=============================\n")
  print(broom::tidy(m, conf.int = TRUE))
  
  cat("\nShapiro test on residuals (sensitive w/ large n):\n")
  print(shapiro.test(residuals(m)))
}

for (cat in levels(mot_long$MotivationCategory)) check_mot_model(cat)

dev.off()

