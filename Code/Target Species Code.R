##TARGET SPECIES CHI SQUARE TEST
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)

urbanCatch<-read.csv("cleanedCatchTable.csv") %>%
  dplyr::filter(
    Region %in% c("Great Lakes", "Inland"),
    FishingMode %in% c("Boat", "Shore"))

table(urbanCatch$Region)
table(urbanCatch$FishingMode)
table(urbanCatch$species_group)

species_mode_tab <- table(
  urbanCatch$species_group,
  urbanCatch$FishingMode)

chisq_species_mode <- chisq.test(species_mode_tab)
chisq_species_mode

cramers_v(species_mode_tab)

species_plot_df <- urbanCatch %>%
  dplyr::count(Region, FishingMode, species_group) %>%
  dplyr::group_by(Region, FishingMode) %>%
  dplyr::mutate(pct = n / sum(n)) %>%
  dplyr::ungroup()

ggplot(
  species_plot_df,
  aes(x = FishingMode, y = pct, fill = species_group)
) +
  geom_col(color = "white", linewidth = 0.3) +
  facet_wrap(~ Region) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(
    values = c(
      "Gamefish" = "grey30",
      "Panfish" = "grey60",
      "Other / Non-game" = "grey85"
    )
  ) +
  labs(
    x = NULL,
    y = "Percent of species mentions",
    fill = "Target species group",
    title = "Target species mentions by fishing mode within urban fisheries"
  ) +
  theme_minimal(base_size = 14)
