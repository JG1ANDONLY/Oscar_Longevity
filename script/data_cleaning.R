library(dplyr)
library(readxl)
library(writexl)

###In Class Week 1###
# Upload the data into R & Rename columns
data <- read_excel("../data/academy_award_winners.xls") |> 
  setNames(c("ID", "sex", "birth_country", "race", "name_change", "genre", 
             "birth_year", "final_year", "status", "films_total", 
             "num_4star_films", "num_win", "num_nom", "first_film_year", 
             "first_win_year", "first_nom_year"))

# Create 'lifespan' column
# Recode the status as 1=event (death/uncensored observation) and 0=censored
data <- data |>
  mutate(lifespan = final_year - birth_year,
         status = 1 - status)

# Recode variables containing "Yes" or "No" to binary (1 for "Yes", 0 for "No")
data <- data |>
  mutate(across(where(~ any(. %in% c('Yes', 'No'))), ~ ifelse(. == 'Yes', 1, 0)))

###In Class Week 2###
# List possible inconsistencies in the data, such as: a person who was first 
# nominated for an Oscar after they first won an Oscar.

# Filter for inconsistencies
# flag1 <- num_win > num_nom → not exist
# flag2 <- first_win_year < first_nom_year 
# flag3 <- first_film_year > first_nom_year 
# flag4 <- num_4star_films > films_total
# flag5 <- final_year < first_nom_year

# Add flags to inconsistent winner(s)
data <- data |>
  mutate(flag1 = ifelse(num_win > num_nom, 1, 0),
         flag2 = ifelse(first_win_year < first_nom_year, 1, 0),
         flag3 = ifelse(first_film_year > first_nom_year, 1, 0),
         flag4 = ifelse(num_4star_films > films_total, 1, 0),
         flag5 = ifelse(final_year < first_nom_year, 1, 0)) |>
  # Replace NA values of all flags with 0 (no flag)
  mutate(across(starts_with("flag"), ~coalesce(., 0))) |>
  mutate(flag_general = ifelse(flag1 == 1 | flag2 == 1 | flag3 == 1 |flag4 == 1, 1, 0))

# Print out IDs of inconsistent winner(s)
inconsistent_id <- data |>
  filter(flag_general == 1) |>
  select(ID)

print(inconsistent_id)

# Remove inconsistent winner(s) and flags
data <- data |>
  filter(!(ID %in% unlist(inconsistent_id)))|>
  select(-starts_with("flag"))

# Save the results
write_xlsx(data, '../data/cleaned_data.xlsx')
