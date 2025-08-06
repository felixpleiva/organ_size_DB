#-------------------------------------------------------------------------------
# Script for the Random Allocation and Assignment of Studies from a Database
#-------------------------------------------------------------------------------
# Purpose: Randomly allocates studies from a database for review by different researchers.
# Each researcher receives their assigned entries as a separate file.

#-------------------------------------------------------------------------------
# - Data are part of an ECR grant project consolidating organ size information in animal species.
# - Only entries extracted from tables and text are included, to simplify instructions and facilitate review by data checkers.
# - Data were extracted from scientific articles via systematic search, following PRISMA workflow criteria (see protocol).
# - All concepts and implementation by Félix P. Leiva; currently under preparation for publication.
#
# For questions: felixpleiva@gmail.com

#-------------------------------------------------------------------------------

# Clear workspace and set seed for reproducibility
rm(list = ls())
set.seed(6955)  # Ensures reproducible randomisation

# Load required libraries
library(readxl)
library(dplyr)
library(openxlsx)
library(knitr)
#-------------------------------------------------------------------------------
# 1. Data Loading and Initial Checks
#-------------------------------------------------------------------------------

# Load data from Excel files
dat <- read_excel("../organ_size_DB_20250602.xlsx", sheet = "data")
val_data <- read_excel("../organ_size_DB_20250602.xlsx", sheet = "validation_data")
metadata <- read_excel("../organ_size_DB_20250602.xlsx", sheet = "metadata")

# ---
# 1.1 Check total number of rows in the main dataset
nrow(dat)

# ---
# 1.2 Check number of unique studies (by key)
length(unique(dat$key))

# ---
# 1.3 Display number of rows per study, sorted in descending order
dat %>%
  group_by(key) %>%
  summarise(n_rows = n(), .groups = "drop") %>%
  arrange(desc(n_rows))

# ---
# 1.4 Filter studies: only those with data extracted exclusively from tables or text
# Excludes studies with any figures, supplementary tables, or mixed sources
studies_from_tables <- dat %>%
  group_by(key) %>%
  filter(all(grepl("^Table\\s\\d+$|^Text$", data_source, ignore.case = TRUE))) %>%
  ungroup()

# Check unique data sources in filtered dataset to confirm selection
unique(studies_from_tables$data_source)
# Now we are certain that the data have been extracted exclusively from both tables and text.
# This makes double-checking more straightforward.

#-------------------------------------------------------------------------------
# 2. Assign Large Studies to Félix and Prepare for Balanced Allocation
#-------------------------------------------------------------------------------

# ---
# 2.1 Identify studies with more than 150 rows for Félix
studies_for_felix <- studies_from_tables %>%
  group_by(key) %>%
  summarise(n_rows = n(), .groups = "drop") %>%
  filter(n_rows > 150) %>%
  pull(key)

# ---
# 2.2 Exclude Félix's studies from main allocation process
dat_filtered <- studies_from_tables %>%
  filter(!(key %in% studies_for_felix))

# ---
# 2.3 Check remaining number of rows and studies
nrow(dat_filtered)
length(unique(dat_filtered$key))

# ---
# 2.4 Calculate percentage of database being rechecked
cat("Percentage of database being rechecked:", round(nrow(studies_from_tables) * 100 / nrow(dat), 2), "%\n")

#-------------------------------------------------------------------------------
# 3. Prepare Study Assignments for Balanced Allocation
#-------------------------------------------------------------------------------

# ---
# 3.1 Group by study key, count rows, randomise order, and prepare for assignment
study_assignments <- dat_filtered %>%
  group_by(key) %>%
  summarise(total_rows = n(), .groups = "drop") %>%
  arrange(sample(n())) %>% # Randomise study order
  mutate(checker_id = NA_integer_)

# ---
# 3.2 Initialise workload vector for each checker (five checkers)
checker_loads <- rep(0, 5)

# ---
# 3.3 Assign studies to checkers using a greedy algorithm (minimise workload imbalance)
for(i in 1:nrow(study_assignments)) {
  # Assign to checker with lowest current workload
  target_checker <- which.min(checker_loads)
  study_assignments$checker_id[i] <- target_checker
  checker_loads[target_checker] <- checker_loads[target_checker] + study_assignments$total_rows[i]
}

#-------------------------------------------------------------------------------
# 4. Join Assignments with Original Data and Map Checker Names
#-------------------------------------------------------------------------------

assigned_data <- dat_filtered %>%
  left_join(study_assignments %>% 
              select(key, checker_id), by = "key")

#-------------------------------------------------------------------------------
# 5. Export Excel Files for Each Reviewer (with Three Tabs)
#-------------------------------------------------------------------------------

# ---
# 5.1 Export files for the five main reviewers
checker_names <- c("Luke", "Jasmijn", "Louise", "Jessy", "Jan")

for(i in 1:5) {
  # Filter data assigned to reviewer i
  df_checker <- assigned_data %>%
    filter(checker_id == i) %>%
    mutate(
      checked = NA,
      notes_checking = NA_character_
    ) %>%
    select(checked, notes_checking, everything()) %>%
    select(-checker_id)

  
  # Create a workbook
  wb <- createWorkbook()
  
  # Add 'data' sheet, apply autofilter and freeze top row
  addWorksheet(wb, "data")
  writeData(wb, "data", df_checker)
  addFilter(wb, "data", row = 1, cols = 1:ncol(df_checker))
  freezePane(wb, "data", firstActiveRow = 2, firstActiveCol = 1)
  
  # Add 'validation_data' and 'metadata' sheets (no filter, no freeze)
  addWorksheet(wb, "validation_data")
  writeData(wb, "validation_data", val_data)
  
  addWorksheet(wb, "metadata")
  writeData(wb, "metadata", metadata)
  
  # Save the workbook
  saveWorkbook(wb, paste0("organ_size_DB_to_check_", checker_names[i], ".xlsx"), overwrite = TRUE)
}

checker_stats <- assigned_data %>%
  group_by(checker_id) %>%
  summarise(
    unique_studies = n_distinct(key),
    total_rows = n()
  ) %>%
  mutate(
    percent_rows = 100 * total_rows / 10712
  ) %>%
  ungroup()


# Add checker names
checker_stats <- checker_stats %>%
  mutate(checker_name = checker_names[checker_id]) %>%
  select(checker_name, unique_studies, total_rows)
# Display results as formatted table
kable(checker_stats, caption = "Distribution of studies and rows by checker")

# 5.2 Export file for Felix (studies with >150 rows)
df_felix <- studies_from_tables %>%
  filter(key %in% studies_for_felix) %>%
  mutate(
    checked = NA,
    notes_checking = NA_character_
  ) %>%
  select(checked, notes_checking, everything())

wb_felix <- createWorkbook()

# Add 'data' sheet, apply autofilter and freeze top row
addWorksheet(wb_felix, "data")
writeData(wb_felix, "data", df_felix)
addFilter(wb_felix, "data", row = 1, cols = 1:ncol(df_felix))
freezePane(wb_felix, "data", firstActiveRow = 2, firstActiveCol = 1)

# Add 'validation_data' and 'metadata' sheets (no filter, no freeze)
addWorksheet(wb_felix, "validation_data")
writeData(wb_felix, "validation_data", val_data)

addWorksheet(wb_felix, "metadata")
writeData(wb_felix, "metadata", metadata)

saveWorkbook(wb_felix, "organ_size_DB_to_check_Felix.xlsx", overwrite = TRUE)

#-------------------------------------------------------------------------------
# End of script
#-------------------------------------------------------------------------------