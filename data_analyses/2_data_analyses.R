library(tidyverse)
library(readxl)

data <- read.csv("inputs/food_sample_complete_data_cleaned.csv")

# 2. Trial data analyses #######################################################
names(data) # recall the position of quantitative calculations (column 10 to 79)
value_columns <- colnames(data)[10:79]

# Analysing troublematic data (I heard it should be Vit. D???)
for (value_col in value_columns) {
  n_unique_groups <- length(unique(data$Edible_part_analysed[!is.na(data[[value_col]])]))
  cat("Number of unique groups for", value_col, ":", unique_groups, "\n")
  
  if (n_unique_groups <= 1) {
    cat("TROUBLEMATIC ", value_col, "- only one or no group available.\n\n")
  }
}

# Vit. D analyses omitted from Kruskal-Wallis Test
dat_sel <- data %>% 
  dplyr::select(-AVERAGE_Vit_D_DW_ug_per_100_gr,
                -STDEV_Vit_D_DW_ug_per_100_gr,
                -AVERAGE_Vit_D_FW_ug_per_100_gr,
                -STDEV_Vit_D_FW_ug_per_100_gr)


# Check column positions again:
names(dat_sel)
value_columns <- colnames(dat_sel)[10:75]


# 2.1. Parts vs. all quantitative data #########################################
# R's annoying dimension described as df!
kruskal_results <- data.frame(
  value_column = character(),
  statistic = numeric(),
  p_value = numeric(),
  stringsAsFactors = F)

for (value_col in value_columns) {
  kruskal_test_result <- kruskal.test(dat_sel[[value_col]] ~ dat_sel$Edible_part_analysed)
  
  kruskal_results <- rbind(kruskal_results,
                           data.frame(
                             value_column = value_col,
                             statistic_edible_parts = kruskal_test_result$statistic,
                             p_value_edible_parts = kruskal_test_result$p.value
                             ))
}

# Rewrite results in new df!
kruskal_edible_parts <- kruskal_results %>% 
  dplyr::mutate(significance_edible_parts = case_when(
    p_value_edible_parts < 0.05 ~ "significant",
    p_value_edible_parts >= 0.05 ~ "not statistically significant"
  ))


# 2.2. Location vs. all quantitative data ######################################
# R's annoying dimension described as df!
kruskal_results <- data.frame(
  value_column = character(),
  statistic = numeric(),
  p_value = numeric(),
  stringsAsFactors = F)

for (value_col in value_columns) {
  kruskal_test_result <- kruskal.test(dat_sel[[value_col]] ~ dat_sel$Location)
  
  kruskal_results <- rbind(kruskal_results,
                           data.frame(
                             value_column = value_col,
                             statistic_location = kruskal_test_result$statistic,
                             p_value_location = kruskal_test_result$p.value
                           ))
}

# Rewrite results in new df!
kruskal_location <- kruskal_results %>% 
  dplyr::mutate(significance_location = case_when(
    p_value_location < 0.05 ~ "significant",
    p_value_location >= 0.05 ~ "not statistically significant"
  ))


# 2.3. species_part_location vs. all quantitative data #########################
# R's annoying dimension described as df!
kruskal_results <- data.frame(
  value_column = character(),
  statistic = numeric(),
  p_value = numeric(),
  stringsAsFactors = F)

for (value_col in value_columns) {
  kruskal_test_result <- kruskal.test(dat_sel[[value_col]] ~ dat_sel$species_part_location)
  
  kruskal_results <- rbind(kruskal_results,
                           data.frame(
                             value_column = value_col,
                             statistic_species_part_location = kruskal_test_result$statistic,
                             p_value_species_part_location = kruskal_test_result$p.value
                           ))
}

# Rewrite results in new df!
kruskal_species_part_location <- kruskal_results %>% 
  dplyr::mutate(significance_species_part_location = case_when(
    p_value_species_part_location < 0.05 ~ "significant",
    p_value_species_part_location >= 0.05 ~ "not statistically significant"
  ))


# 2.4. Additional grouped per domesticated OR wild? ############################
# R's annoying dimension described as df!
wilcox_results <- data.frame(
  value_column = character(),
  statistic = numeric(),
  p_value = numeric(),
  stringsAsFactors = F)

for (value_col in value_columns) {
  wilcox_test_result <- wilcox.test(dat_sel[[value_col]] ~ dat_sel$domesticated_or_wild, exact = F)
  
  wilcox_results <- rbind(wilcox_results,
                          data.frame(
                            value_column = value_col,
                            statistic_domesticated_wilcox = wilcox_test_result$statistic,
                            p_value_domesticated_wilcox = wilcox_test_result$p.value
                          ))
}

# Rewrite results in new df!
wilcox_domesticated <- wilcox_results %>% 
  dplyr::mutate(significance_domesticated_wilcox = case_when(
    p_value_domesticated < 0.05 ~ "significant",
    p_value_domesticated >= 0.05 ~ "not statistically significant"
  ))

# Bind all results into one huge df
all_results <- kruskal_edible_parts %>% 
  dplyr::left_join(kruskal_location, by = "value_column") %>% 
  dplyr::left_join(kruskal_species_part_location, by = "value_column") %>% 
  dplyr::left_join(wilcox_domesticated, by = "value_column")

write.csv(all_results, "outputs/kruskal_wilcox_results_four_group_categories.csv", row.names = F)



# Additional grouped per species? ##############################################
# R's annoying dimension described as df!
kruskal_results <- data.frame(
  value_column = character(),
  statistic = numeric(),
  p_value = numeric(),
  stringsAsFactors = F)

for (value_col in value_columns) {
  kruskal_test_result <- kruskal.test(dat_sel[[value_col]] ~ dat_sel$species_dc_ver)
  
  kruskal_results <- rbind(kruskal_results,
                           data.frame(
                             value_column = value_col,
                             statistic_species = kruskal_test_result$statistic,
                             p_value_species = kruskal_test_result$p.value
                           ))
}

# Rewrite results in new df!
kruskal_species_only <- kruskal_results %>% 
  dplyr::mutate(significance_species_only = case_when(
    p_value_species < 0.05 ~ "significant",
    p_value_species >= 0.05 ~ "not statistically significant"
  ))

# All of the calculations resulted in, "not statistically significant" :/
# (Similar to kruskal_species_part_location; when all data is different entity even some of the plants belong to a similar species)




