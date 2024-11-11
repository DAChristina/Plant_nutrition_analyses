library(tidyverse)

data <- read.csv("inputs/food_sample_complete_data_cleaned.csv")

# 2. Trial data analyses #######################################################
names(data) # recall the position of quantitative calculations (column 10 to 79)
value_columns <- colnames(data)[11:80]

# Analysing troublematic data (I heard it should be Vit. D???)
for (value_col in value_columns) {
  n_unique_groups <- length(unique(data$Edible_part_analysed[!is.na(data[[value_col]])]))
  cat("Number of unique groups for", value_col, ":", n_unique_groups, "\n")
  
  if (n_unique_groups <= 1) {
    cat("TROUBLEMATIC ", value_col, "- only one or no group available.\n\n")
  }
}

# Vit. D analyses omitted from Kruskal-Wallis Test
dat_sel <- data %>% 
  dplyr::select(-AVERAGE_Vit_D_DW_ug_per_100_gr,
                -STDEV_Vit_D_DW_ug_per_100_gr,
                -AVERAGE_Vit_D_FW_ug_per_100_gr,
                -STDEV_Vit_D_FW_ug_per_100_gr) %>% 
  # STDEV omitted from Kruskal-Wallis Test
  dplyr::select(-contains("STDEV")) %>% 
  # Fresh Weight (FW) omitted from Kruskal-Wallis Test
  dplyr::select(-contains("_FW"))



# Check column positions again:
names(dat_sel)
value_columns <- colnames(dat_sel)[11:26]

# Filtered data preparation
filter_fruit <- dat_sel %>% 
  dplyr::filter(Edible_part_simplified == "Fruit") %>% 
  # Omit column that only has 1 data
  dplyr::select(-AVERAGE_Vit_B1_DW_mg_per_kg, -AVERAGE_Phytate_DW_mg_per_g)

filter_leaf <- dat_sel %>% 
  dplyr::filter(Edible_part_simplified == "Leaf") %>% 
  # Omit column that only has 1 data
  dplyr::select(-AVERAGE_HCN_DWppm,
                -AVERAGE_Phytate_DW_mg_per_g, -AVERAGE_Oxalate_DW_percent_wb,
                -AVERAGE_Tanin_DW_mg_as._Tanic_per_g)

filter_seed <- dat_sel %>% 
  dplyr::filter(Edible_part_simplified == "Seed")

filter_tuber <- dat_sel %>% 
  dplyr::filter(Edible_part_simplified == "Tuber") %>% 
  dplyr::select(-AVERAGE_Carotenoid_total_DW_mg_per_kg)


# Re-run troublematic data check for each filtered files
for (value_col in value_columns) {
  n_unique_groups_fruit <- length(unique(filter_fruit$species_part_location[!is.na(filter_fruit[[value_col]])]))
  # cat("Number of unique groups for", value_col, ":", n_unique_groups, "\n")
  
  if (n_unique_groups_fruit <= 1) {
    cat("TROUBLEMATIC fruit ", value_col, "- only one or no group available.\n\n")
    
  }
  
  n_unique_groups_leaf <- length(unique(filter_leaf$species_part_location[!is.na(filter_leaf[[value_col]])]))
  # cat("Number of unique groups for", value_col, ":", n_unique_groups, "\n")
  
  if (n_unique_groups_leaf <= 1) {
    cat("TROUBLEMATIC leaf ", value_col, "- only one or no group available.\n\n")
    
  }
  
  n_unique_groups_seed <- length(unique(filter_seed$species_part_location[!is.na(filter_seed[[value_col]])]))
  # cat("Number of unique groups for", value_col, ":", n_unique_groups, "\n")
  
  if (n_unique_groups_seed <= 1) {
    cat("TROUBLEMATIC seed ", value_col, "- only one or no group available.\n\n")
    
  }
  
  n_unique_groups_tuber <- length(unique(filter_tuber$species_part_location[!is.na(filter_tuber[[value_col]])]))
  # cat("Number of unique groups for", value_col, ":", n_unique_groups, "\n")
  
  if (n_unique_groups_tuber <= 1) {
    cat("TROUBLEMATIC tuber", value_col, "- only one or no group available.\n\n")
    
  }
  
}


# 2.1. Parts vs. all quantitative data #########################################
# 2.1.1. Fruit
# R's annoying dimension described as df!
kruskal_results <- data.frame(
  value_column = character(),
  statistic = numeric(),
  p_value = numeric(),
  stringsAsFactors = F)

# value_columns unique to filter_fruit
names(filter_fruit)
value_columns <- colnames(filter_fruit)[11:24]

for (value_col in value_columns) {
  kruskal_test_result_fruit <- kruskal.test(filter_fruit[[value_col]] ~ filter_fruit$species_part_location)
  
  kruskal_results <- rbind(kruskal_results,
                           data.frame(
                             value_column = value_col,
                             statistic_fruit = kruskal_test_result_fruit$statistic,
                             p_value_fruit = kruskal_test_result_fruit$p.value
                             ))
}

# Rewrite results in new df!
kruskal_fruit <- kruskal_results %>% 
  dplyr::mutate(significance_fruit = case_when(
    p_value_fruit < 0.05 ~ "significant",
    p_value_fruit >= 0.05 ~ "not statistically significant"
  )) %>% 
  glimpse()

# 2.1.2. Leaf
# R's annoying dimension described as df!
kruskal_results <- data.frame(
  value_column = character(),
  statistic = numeric(),
  p_value = numeric(),
  stringsAsFactors = F)

# value_columns unique to filter_fruit
names(filter_leaf)
value_columns <- colnames(filter_leaf)[11:22]

for (value_col in value_columns) {
  kruskal_test_result_leaf <- kruskal.test(filter_leaf[[value_col]] ~ filter_leaf$species_part_location)
  
  kruskal_results <- rbind(kruskal_results,
                                data.frame(
                                  value_column = value_col,
                                  statistic_leaf = kruskal_test_result_leaf$statistic,
                                  p_value_leaf = kruskal_test_result_leaf$p.value
                                ))
}

# Rewrite results in new df!
kruskal_leaf <- kruskal_results %>% 
  dplyr::mutate(significance_leaf = case_when(
    p_value_leaf < 0.05 ~ "significant",
    p_value_leaf >= 0.05 ~ "not statistically significant"
  )) %>% 
  glimpse()

# 2.1.3. Seed
# R's annoying dimension described as df!
kruskal_results <- data.frame(
  value_column = character(),
  statistic = numeric(),
  p_value = numeric(),
  stringsAsFactors = F)

# value_columns unique to filter_fruit
names(filter_seed)
value_columns <- colnames(filter_seed)[11:22]

for (value_col in value_columns) {
  kruskal_test_result_seed <- kruskal.test(filter_seed[[value_col]] ~ filter_seed$species_part_location)
  
  kruskal_results <- rbind(kruskal_results,
                                data.frame(
                                  value_column = value_col,
                                  statistic_seed = kruskal_test_result_seed$statistic,
                                  p_value_seed = kruskal_test_result_seed$p.value
                                ))
}

# Rewrite results in new df!
kruskal_seed <- kruskal_results %>% 
  dplyr::mutate(significance_seed = case_when(
    p_value_seed < 0.05 ~ "significant",
    p_value_seed >= 0.05 ~ "not statistically significant"
  )) %>% 
  glimpse()

# 2.1.4. Tuber
# R's annoying dimension described as df!
kruskal_results <- data.frame(
  value_column = character(),
  statistic = numeric(),
  p_value = numeric(),
  stringsAsFactors = F)

# value_columns unique to filter_fruit
names(filter_tuber)
value_columns <- colnames(filter_tuber)[11:25]

for (value_col in value_columns) {
  kruskal_test_result_tuber <- kruskal.test(filter_tuber[[value_col]] ~ filter_tuber$species_part_location)
  
  kruskal_results <- rbind(kruskal_results,
                                 data.frame(
                                   value_column = value_col,
                                   statistic_tuber = kruskal_test_result_tuber$statistic,
                                   p_value_tuber = kruskal_test_result_tuber$p.value
                                 ))
}

# Rewrite results in new df!
kruskal_tuber <- kruskal_results %>% 
  dplyr::mutate(significance_tuber = case_when(
    p_value_tuber < 0.05 ~ "significant",
    p_value_tuber >= 0.05 ~ "not statistically significant"
  )) %>% 
  glimpse()



# Bind all results into one huge df
all_results_parts <- kruskal_fruit %>% 
  dplyr::full_join(kruskal_leaf, by = "value_column") %>% 
  dplyr::full_join(kruskal_seed, by = "value_column") %>% 
  dplyr::full_join(kruskal_tuber, by = "value_column")

write.csv(all_results_parts, "outputs/kruskal_results_per_parts_group_categories.csv", row.names = F)


# Trial Dunn's Test with Bonferroni's correction ###############################
filtered_data <- dat_sel %>% 
  dplyr::filter(Edible_part_simplified == "Tuber",
                !is.na(AVERAGE_Carbohydrate_DW_g_per_100_g))

dunn_test_result <- FSA::dunnTest(filtered_data$AVERAGE_Carbohydrate_DW_g_per_100_g, filtered_data$species_part_location, method = "none")
dunn_df <- as.data.frame(dunn_test_result$res)

# 2.2. Location vs. all quantitative data ######################################
# Check column positions again:
names(dat_sel)
value_columns <- colnames(dat_sel)[11:26]

# R's annoying dimension described as df!
kruskal_results <- data.frame(
  value_column = character(),
  statistic = numeric(),
  p_value = numeric(),
  stringsAsFactors = F)

# value_columns unique to fruit


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
    p_value_domesticated_wilcox < 0.05 ~ "significant",
    p_value_domesticated_wilcox >= 0.05 ~ "not statistically significant"
  ))


# 2.5. Additional grouped per edible parts--domesticated OR wild? ##############
# R's annoying dimension described as df!
kruskal_results <- data.frame(
  value_column = character(),
  statistic = numeric(),
  p_value = numeric(),
  stringsAsFactors = F)

for (value_col in value_columns) {
  kruskal_test_result <- kruskal.test(dat_sel[[value_col]] ~ dat_sel$part_domesticated_or_wild)
  
  kruskal_results <- rbind(kruskal_results,
                          data.frame(
                            value_column = value_col,
                            statistic_edible_domesticated = kruskal_test_result$statistic,
                            p_value_edible_domesticated = kruskal_test_result$p.value
                          ))
}

# Rewrite results in new df!
kruskal_edible_domesticated <- kruskal_results %>% 
  dplyr::mutate(significance_edible_domesticated = case_when(
    p_value_edible_domesticated < 0.05 ~ "significant",
    p_value_edible_domesticated >= 0.05 ~ "not statistically significant"
  ))

# Bind all results into one huge df
all_results <- kruskal_edible_parts %>% 
  dplyr::left_join(kruskal_location, by = "value_column") %>% 
  dplyr::left_join(kruskal_species_part_location, by = "value_column") %>% 
  dplyr::left_join(wilcox_domesticated, by = "value_column") %>% 
  dplyr::left_join(kruskal_edible_domesticated, by = "value_column")


write.csv(all_results, "outputs/kruskal_wilcox_results_five_group_categories.csv", row.names = F)



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


# Trial Dunn's Test with Bonferroni's correction ###############################
dunn_test_result <- FSA::dunnTest(dat_sel[[value_col]], dat_sel$part_domesticated_or_wild, method = "bonferroni")
dunn_df <- as.data.frame(dunn_test_result$res)
