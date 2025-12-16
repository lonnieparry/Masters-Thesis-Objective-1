#DEMOGRAPHICS
library(dplyr)
library(ggplot2)
library(tidyr)
library(tigris)
library(viridis)
library(lubridate)


urbandemo<-read.csv("cleanedcreeltable.csv")%>%
  dplyr::filter(
    Region %in% c("Great Lakes", "Inland"),
    FishingMode %in% c("Boat", "Shore"),
    !is.na(income_group),
    !is.na(race_collapsed))
#FUNCTION TO CALCULATE CRAMER'S V
cramers_v <- function(tab) {
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  n <- sum(tab)
  k <- min(nrow(tab), ncol(tab))
  sqrt(as.numeric(chi$statistic) / (n * (k - 1)))
}

#INCOMExFISHING MODE TESTS

income_mode_tab <- table(urbandemo$income_group, urbandemo$FishingMode)

chisq_income_mode <- chisq.test(income_mode_tab)

cramers_v(income_mode_tab)
#.2655469

#RACExFISHING MODE TEST
race_mode_tab <- table(urbandemo$race_collapsed, urbandemo$FishingMode)

chisq_race_mode <- chisq.test(race_mode_tab)

cramers_v(race_mode_tab)
#.4100105

#INCOMExREGION TEST
income_region_tab <- table(urbandemo$income_group, urbandemo$Region)

chisq_income_region <- chisq.test(income_region_tab)

cramers_v(income_region_tab)
#.08620969

#RACExREGION TEST
race_region_tab <- table(urbandemo$race_collapsed, urbandemo$Region)

chisq_race_region <- chisq.test(race_region_tab)

cramers_v(race_region_tab)
#.2070225

#stacked bar chart for race by fishing mode in the areas
library(dplyr)
library(ggplot2)
library(scales)


race_plot_df <- urbandemo %>%
  filter(
    !is.na(race_collapsed),
    !is.na(FishingMode),
    !is.na(Region)
  ) %>%
  count(Region, FishingMode, race_collapsed) %>%
  group_by(Region, FishingMode) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

race_colors <- c(
  "White" = "grey85",
  "Black" = "grey35",
  "Hispanic/Latino" = "grey55",
  "Asian" = "grey65",
  "Other/Multiracial/Small N" = "grey20")

ggplot(race_plot_df, aes(x = FishingMode, y = pct, fill = race_collapsed)) +
  geom_col(color = "white", linewidth = 0.3) +
  facet_wrap(~ Region) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(
    values = race_colors,
    breaks = c(
      "White",
      "Black",
      "Hispanic/Latino",
      "Asian",
      "Other/Multiracial/Small N"
    )
  ) +
  labs(
    x = NULL,
    y = "Percent of anglers",
    fill = "Race (collapsed)",
    title = "Racial composition by fishing mode within urban fisheries"
  ) +
  theme_minimal()


