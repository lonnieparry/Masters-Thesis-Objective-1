catchTable<-catchTable%>%
  filter(!is.na(species))

catch_merged <- catchTable %>%
  left_join(
    cleancreel %>% select(creelID, partyActivity, angler_group),
    by = "creelID" )


cleancreel<-read.csv ("cleanedcreeltable.csv")
##STATS FOR DEMOGRAPHY
cleancreel <- cleancreel %>% filter(!is.na(race))
#removing any refusal responses in the race variable 
cleancreel <- cleancreel[cleancreel$race != "refusal", ]

#race vector
all_races <- c(
  "American Indian or Alaska Native",
  "Arabic",
  "Asian",
  "Black",
  "Hispanic or Latino (any race)",
  "Hmong",
  "Indian",
  "Multiracial",
  "Other",
  "White")

cleancreel <- cleancreel %>%
  filter(!is.na(race)) %>%         
  filter(race != "refusal")        
cleancreel <- cleancreel %>%
  mutate(
    race_clean = case_when(
      hispanic == "yes" ~ "Hispanic or Latino (any race)",
      TRUE ~ race))
cleancreel <- cleancreel %>%
  filter(!is.na(income_group)) %>%   
  filter(income_group != "refusal")  
cleancreel$income_group <- factor(
  cleancreel$income_group,
  levels = c("Low", "Middle", "High"))

cleancreel <- cleancreel %>%
  mutate(
    Region = case_when(
      angler_group == "Milwaukee"   ~ "Great Lakes",
      angler_group == "Dane County" ~ "Inland",
      TRUE ~ NA_character_),
    FishingMode = case_when(
      partyActivity == "boat_fishing"  ~ "Boat Anglers",
      partyActivity == "shore_fishing" ~ "Shore Anglers",
      TRUE ~ NA_character_))

#is the distribution of race independent of angler_group (DANE COUNTY V MILWAUKEE)
chisq.test(table(cleancreel$angler_group, cleancreel$race_collapsed))
##Results say X-squared = 67.612, df = 18,
#p-value = 1.141e-07 sorace distrubutions differ significantly betweek the two urban areas.
#However because some of me race groups are very small, my chi-squared approximations may be incorrect. So, I am going to collapse my races for statistical validity.

cleancreel <- cleancreel %>%
  mutate(
    race_collapsed = case_when(
      race_clean == "White" ~ "White",
      race_clean == "Black" ~ "Black",
      race_clean == "Hispanic or Latino (any race)" ~ "Hispanic/Latino",
      race_clean == "Asian" ~ "Asian",
      race_clean %in% c(
        "American Indian or Alaska Native",
        "Arabic",
        "Hmong",
        "Indian",
        "Multiracial",
        "Other"
      ) ~ "Other / Multiracial / Indigenous",
      TRUE ~ NA_character_))

#Making collapsed races a factor for later data visualization
cleancreel$race_collapsed <- factor(
  cleancreel$race_collapsed,
  levels = c("White", "Black", "Hispanic/Latino", "Asian", "Other"))

#splitting to make sure its just urban anglers
urban_creel <- cleancreel %>%
  filter(angler_group %in% c("Milwaukee", "Dane County"))
