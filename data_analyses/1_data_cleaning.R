library(tidyverse)
library(readxl)

raw_data <- readxl::read_excel("raw_data/Food sample complete data.xlsx")
sheet_names <- readxl::excel_sheets("raw_data/Food sample complete data.xlsx")
# File contains 1 sheet only

# view(raw_data)
names(raw_data)

# 1. Conduct data cleaning #####################################################
replacements <- c(" " = "_", "%" = "percent", "/" = "_per_", "(" = "", ")" = "")
for (pattern in names(replacements)) {
  names(raw_data) <- gsub(pattern, replacements[[pattern]], names(raw_data), fixed = T)
}
names(raw_data)
glimpse(raw_data)
unique(raw_data$Species)
# unique(raw_data$`Edible part analysed`) # quite clean
# unique(raw_data$`Sample condition`) # quite clean
# unique(raw_data$`Local name`) # quite clean

cleaned_data <- raw_data %>%
  dplyr::mutate(species_dc_ver = case_when(
    str_detect(Species, "^Begonia isoptera Dryand ex Sm\\.$") ~ "Begonia isoptera Dryand. ex Sm.",
    str_detect(Species, "^Canna indica L\\.$") ~ "Canna indica L.",
    str_detect(str_trim(Species), "^Crassocephalum crepidioides") ~ "Crassocephalum crepidioides (Benth.) S.Moore",
    str_detect(Species, "^Dioscorea esculenta") ~ "Dioscorea esculenta (Lour.) Burkill",
    TRUE ~ Species  # Keep other species unchanged
  ),
  species_part = paste(species_dc_ver, Edible_part_analysed, sep = "_"),
  species_location = paste(species_dc_ver, Location, sep = "_"),
  species_part_location = paste(species_dc_ver, Edible_part_analysed, Location, sep = "_"),
  domesticated_or_wild = case_when(
    Code == "Domesticated" ~ "Domesticated",
    T ~ "Wild"
  ))
unique(cleaned_data$species_dc_ver)

# Generate notes for each quantitative measurement data
names(cleaned_data) # check the position of quantitative calculations (column 10 to 79)

for (i in 10:79) {
  col_name <- names(cleaned_data)[i]
  notes_col_name <- paste0("notes_", col_name)
  
  # Basically I copy all of the original data into new columns called "notes_";
  # Then I convert all quantitative measurement data to numeric
  cleaned_data <- cleaned_data %>% 
    dplyr::mutate(!!notes_col_name := !!sym(col_name),   # Create notes columns
                  !!col_name := as.numeric(as.character(!!sym(col_name))))  # Convert to numeric
}

glimpse(cleaned_data)

write.csv(cleaned_data, "inputs/food_sample_complete_data_cleaned.csv", row.names = F)

# Phew.

