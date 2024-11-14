library(tidyverse)
library(readxl)

raw_data <- readxl::read_excel("raw_data/Food sample complete data ver 2.xlsx")
data <- read.csv("inputs/food_sample_complete_data_cleaned.csv")

# Generate notes for each quantitative measurement data
names(raw_data) # check the position of quantitative calculations (column 10 to 79)

cleaned_data <- raw_data %>%
  dplyr::mutate(species_dc_ver = case_when(
    str_detect(Species, "^Begonia isoptera Dryand ex Sm\\.$") ~ "Begonia isoptera Dryand. ex Sm.",
    str_detect(Species, "^Canna indica L\\.$") ~ "Canna indica L.",
    str_detect(str_trim(Species), "^Crassocephalum crepidioides") ~ "Crassocephalum crepidioides (Benth.) S.Moore",
    str_detect(Species, "^Dioscorea esculenta") ~ "Dioscorea esculenta (Lour.) Burkill",
    TRUE ~ Species  # Keep other species unchanged
  ),
  species_part = paste(species_dc_ver, 'Edible part analysed', sep = "_"),
  species_location = paste(species_dc_ver, Location, sep = "_"),
  species_part_location = paste(species_dc_ver, 'Edible part analysed', Location, sep = "_"),
  Maintenance = case_when(
    Code == "Domesticated" ~ "Domesticated",
    T ~ "Wild"
  ))

cleaned_data[, 11:80] <- sapply(cleaned_data[, 11:80], as.numeric)

# 3.1. Scatter Plot! ###########################################################
dat_sel_long <- cleaned_data %>%
  dplyr::select(species_part_location, Maintenance, "Edible part simplified",
                contains("AVERAGE"),
                -contains("notes_"),
                -contains(" FW")) %>%
  pivot_longer(
    cols = contains("AVERAGE"),
    names_to = "Nutrient",
    values_to = "Average"
  ) %>%
  left_join(
    cleaned_data %>%
      dplyr::select(species_part_location, Maintenance, "Edible part simplified",
                    contains("STDEV"),
                    -contains("notes_"),
                    -contains(" FW")) %>%
      pivot_longer(
        cols = contains("STDEV"),
        names_to = "Nutrient",
        values_to = "STDEV"
      ) %>% 
      dplyr::mutate(Nutrient = case_when(
        str_detect(Nutrient, "STDEV ") ~ str_replace(Nutrient, "STDEV ", "AVERAGE "))),
    by = c("species_part_location", "Maintenance", "Edible part simplified", "Nutrient")
  ) %>%
  # Replace "AVERAGE_" and "_DW_" in the Nutrient column
  dplyr::mutate(Nutrient = case_when(
    str_detect(Nutrient, "AVERAGE ") ~ str_replace(Nutrient, "AVERAGE ", ""),  # Remove "AVERAGE "
    # gsub("_DW_", "_", Nutrient),
    str_detect(Nutrient, "DW") ~ str_replace(Nutrient, "DW", "")  # Replace "_DW_" with "_"
    # TRUE ~ Nutrient  # Leave other names unchanged
  ))

dat_sel_long <- dat_sel_long %>%
  dplyr::mutate(Nutrient = str_replace(Nutrient, " DW", ""))

# Now, plot using ggplot!!!
png("pictures/data_distribution_per_edible_parts.png", width = 24, height = 12, unit = "cm", res = 1200)
ggplot(dat_sel_long, aes(x = `Edible part simplified`, y = Average, color = Maintenance)) +
  geom_jitter(aes(color = Maintenance),
              width = 0.2, size = 0.5, alpha = 0.6) +  # Jitter plot for individual points
  geom_errorbar(
    aes(ymin = Average - STDEV, ymax = Average + STDEV),
    width = 0.1,  # Error bar width
    size = 0.1  # Error bar size
  ) +
  facet_wrap(~ Nutrient, scales = "free_y") +
  labs(title = "Nutritional Composition (Dry Weight) by Edible Part (with Error Bars)",
       x = "Edible Part",
       y = "Nutrient Value (Average ± STDEV)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 7, angle = 0, hjust = 0.5))
dev.off()

# Additional graph using y-axis = log10 transformationed
png("pictures/data_distribution_per_edible_parts_log10.png", width = 24, height = 12, unit = "cm", res = 1200)
ggplot(dat_sel_long, aes(x = `Edible part simplified`, y = Average, color = Maintenance)) +
  geom_jitter(aes(color = Maintenance),
              width = 0.2, size = 0.5, alpha = 0.6) +  # Jitter plot for individual points
  geom_errorbar(
    aes(ymin = Average - STDEV, ymax = Average + STDEV),
    width = 0.1,  # Error bar width
    size = 0.1  # Error bar size
  ) +
  facet_wrap(~ Nutrient, scales = "free_y") +
  scale_y_continuous(trans = 'log10', labels = scales::label_log()) +
  labs(title = "Nutritional Composition log10(Dry Weight) by Edible Part (with Error Bars)",
       x = "Edible Part",
       y = "Nutrient Value log10(Average ± STDEV)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 7, angle = 0, hjust = 0.5))
dev.off()


# 3.2. Data viz specified to Mann-Whitney U (Maintenance)! #####################
# In R, Mann-Whitney U test is performed as wilcox.test() function,
# as Mann-Whitney U is a direct implementation of Wilcoxon rank-sum test approach.
all_results_parts_domesticated <- read.csv("outputs/wilcox_results_per_parts_domesticated_group_categories.csv")

# According to Mann-Whitney U test, statistically significance groups only occur when we compare Protein, Zn and Oxalate
# In seedc=s between domesticated and wild plants

dat_p_values <- all_results_parts_domesticated %>%
  dplyr::select(value_column, statistic_seed, p_value_seed, significance_seed) %>% 
  dplyr::filter(significance_seed == "significant") %>% 
  dplyr::mutate(significance = case_when(
    p_value_seed < 0.001 ~ "***",
    p_value_seed < 0.01 ~ "**",
    p_value_seed < 0.05 ~ "*",
    TRUE ~ "ns"  # "ns" for not significant
  )) %>% 
  dplyr::mutate(value_column = dplyr::recode(value_column,
                                             "AVERAGE_Protein_DW_percent" = "Protein (%)",
                                             "AVERAGE_Zn_DW_mg_per_100_g" = "Zn (mg/100 g)",
                                             "AVERAGE_Oxalate_DW_percent_wb" = "Oxalate (% wb)")) %>% 
  glimpse()

# create new df coz' y-axes are widely differ!
max_values <- dat_wilcox_filtered_long %>%
  group_by(Nutrient) %>%
  summarise(max_average = max(Average, na.rm = TRUE))

glimpse(dat_sel_long)
dat_wilcox_filtered_long <- dat_sel_long %>% 
  dplyr::filter(`Edible part simplified` == "Seed",
                Nutrient %in% c("Protein (%)", "Zn (mg/100 g)", "Oxalate (% wb)")) %>% 
  dplyr::left_join(dat_p_values, by = c("Nutrient" = "value_column")) %>% 
  dplyr::left_join(max_values, by = "Nutrient")

# Weird factor rearrangement caused by weird package - _-)'
dat_wilcox_filtered_long$Maintenance <- factor(dat_wilcox_filtered_long$Maintenance,
                                               levels = c("Domesticated", "Wild"))

# Modified from:
# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/
# coz' I've calculated the p-values before!

# Create the boxplot with p-values from 'dat_wilcox_filtered_long'
p <- ggpubr::ggboxplot(dat_wilcox_filtered_long, 
                       x = "Maintenance", y = "Average",
                       color = "Maintenance", palette = "jco",
                       add = "jitter", facet.by = "Nutrient", 
                       scales = "free_y", short.panel.labs = T) +
  # Remove that weird "Nutrient:" label prefix in facet titles -_-)
  theme(strip.text = element_text(size = 12)) +  # Optional: Customize size of facet labels
  facet_wrap(~ Nutrient, scales = "free_y", labeller = labeller(Nutrient = label_value)) +
  theme(legend.position = "none") +
  labs(title = "Nutritional Differences (Dry Weight) by Maintenance in Seed Group",
       x = "Maintenance",
       y = "Nutrient Value of Seed")


# Add p-value significance labels using the 'significance' column from merged data
png("pictures/data_wilcox_positive.png", width = 24, height = 12, unit = "cm", res = 1200)
p + geom_text(data = dat_wilcox_filtered_long, 
              aes(x = 1.5, y = max_average + 10/100*max_average, label = paste0("p-value = ", round(p_value_seed, 3))), # either significance or p_value_seed
              inherit.aes = FALSE, size = 5, color = "black")
dev.off()

