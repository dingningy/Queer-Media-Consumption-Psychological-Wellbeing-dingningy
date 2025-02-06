### Data Cleaning of Two Raw Dataset

## Load the required libraries
install.packages("tidyverse")
library(tidyverse)


## Read in the raw data
raw.queer.label <- read_csv("data/raw.queer.label.csv")  # This dataset displays labels for each data cell, which clearly shows qualitative data for factor and character variables (e.g., most demographic variables, frequency, media types)
raw.queer.value <- read_csv("data/raw.queer.value.csv")  # This dataset displays the actual data values for each cell, which clearly shows quantitative data for integer and numeric variables (e.g., age, ratings for measurement scales)


##Delete unnecessary columns
queer.label <- raw.queer.label %>% 
  select(-c(EndDate, Status, IPAddress, Progress, `Duration (in seconds)`, Finished, RecordedDate, ResponseId,RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference, LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage, Consent, `Queer Character`))

queer.value <- raw.queer.value %>%
  select(-c(EndDate, Status, IPAddress, Progress, `Duration (in seconds)`, Finished, RecordedDate, ResponseId,RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference, LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage, Consent, `Queer Character`))


## Delete unnecessary rows of exact question and import IDs
queer.label <- queer.label %>%
  slice(-1:-2)

queer.value <- queer.value %>%
  slice(-1:-2)

## Detect duplicated participants (if any)
# Find duplicated participants
duplicates.queer.label <- queer.label %>% group_by(ID) %>% tally() %>% filter(n > 1)
duplicates.queer.value <- queer.value %>% group_by(ID) %>% tally() %>% filter(n > 1)

# Remain the rows of the first response for each duplicated participant
queer.label <- queer.label %>%
  arrange(ID, StartDate) %>%
  distinct(ID, .keep_all = TRUE)

queer.value <- queer.value %>%
  arrange(ID, StartDate) %>%
  distinct(ID, .keep_all = TRUE)

# Delete StartDate column
queer.label <- queer.label %>%
  select(-StartDate)

queer.value <- queer.value %>%
  select(-StartDate)


## Merge the two datasets into one combining with qualitative and quantitative data

# Create subsets for each dataset
queer.label.subset <- queer.label %>% select(1:2, 3:25)
queer.value.subset <- queer.value %>% select(1:2, 26:57)

# Merge the two datasets
queer.merge <- merge(queer.label.subset, queer.value.subset, by = c("ID","Age")) # There are duplicate rows after merging


## Combine several columns into one column
queer.merge <- queer.merge %>%
  mutate(Country = if_else(Country == "United States of America", paste(Country, `State (US)`, sep = ", "), Country)) %>% 
  select(-`State (US)`) %>% 
  mutate(`Gender Identity` = if_else(`Gender Identity` == "Others, please specify:", 
                                   `Gender Identity_4_TEXT`, 
                                   `Gender Identity`)) %>% 
  select(-`Gender Identity_4_TEXT`) %>% 
  mutate(`Sexual Orientation` = if_else(`Sexual Orientation` == "Other (please specify):", 
                                       `Sexual Orientation_5_TEXT`, 
                                       `Sexual Orientation`)) %>% 
  select(-`Sexual Orientation_5_TEXT`)


## Selecting and Renaming Columns
# Rename columns to lowercase
queer.merge <- queer.merge %>%
  rename_with(~ tolower(.), -ID)

# Combine column names with two or more words with "_"
queer.merge <- queer.merge %>%
  rename_with(~ gsub(" ", "_", .)) 

## Rename columns to more informative names
# Rename columns for media types
queer.merge <- queer.merge %>%
  select (
    type_1_text,
    type_2_text,
    type_3_text,
    type_4_text,
    type_5_text,
    type_6_text,
    type_7_text,
    type_8_text,
    everything() # Remain all other columns
  ) %>%
  rename(
    "drama_name" = type_1_text,
    "comedy_name" = type_2_text,
    "documentary_name" = type_3_text,
    "romance_name" = type_4_text,
    "action_name" = type_5_text,
    "scifi_name" = type_6_text,
    "reality_name" = type_7_text,
    "other_name" = type_8_text
  ) %>% 
  relocate(drama_name, comedy_name, documentary_name, romance_name, action_name, scifi_name, reality_name, other_name, .after = "type")

# Specify question number for each numeric measurement
queer.merge <- queer.merge %>%
  rename_with(~ gsub("(\\d+)", "q\\1", .))  # Add 'q' before each number


## Change Data Type
# Change data type for demographic variables
queer.merge <- queer.merge %>% 
  mutate(
    age = as.integer(age),  # Change data type to integer
    gender_identity = as.factor(gender_identity),  # Change data type to factor
    transgender = fct_recode(as.factor(transgender), 
                             "1" = "Yes, I am transgender", 
                             "0" = "No, I am not transgender"),  # Recode transgender
    transgender = as.factor(transgender),  
    sexual_orientation = as.factor(sexual_orientation),  
    race = as.factor(race),  
    education = as.factor(education)  
  ) %>% 
  select(-ethnicity) 

# Change data type for queer media consumption habits
unique(queer.merge$frequency)
queer.merge$frequency <- factor(queer.merge$frequency,
                                               levels = c("Never", "A few times per year", "A few times per month", "A few times per week", "Daily"),
                                               ordered = TRUE) 

unique(queer.merge$actively_seek_out)
queer.merge$actively_seek_out <- factor(queer.merge$actively_seek_out,
                                levels = c("Never", "Rarely", "Sometimes", "Often", "Always"),
                                ordered = TRUE)

unique(queer.merge$discussion)
queer.merge$discussion <- factor(queer.merge$discussion,
                                        levels = c("Never", "Rarely", "Sometimes", "Often", "Always"),
                                        ordered = TRUE)

queer.merge <- queer.merge %>% 
  select(-importance) # I suddently realized that the importance is overlapped with later measurements

# Change data type for authenticity, stereotype, and intersectionality ratings and psychological measurements
queer.merge <- queer.merge %>% 
  mutate(across(21:52, as.numeric))


## Reorder the columns by country and age
queer.merge <- queer.merge %>%
  arrange(country, age)


## Verify column values and change if necessary

# Replace ID with numbers from 1 to 174
queer.merge$ID <- seq(1, nrow(queer.merge))

# Replace country names for UK and US
queer.merge$country <- str_replace_all(queer.merge$country, "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")

state_abbreviations <- c("Alabama" = "AL", "Arizona" = "AZ", "Arkansas" = "AR", "California" = "CA", "Colorado" = "CO","Florida" = "FL", "Hawaii" = "HI", "Idaho" = "ID",
                         "Illinois" = "IL", "Indiana" = "IN","Louisiana" = "LA", "Maine" = "ME","Massachusetts" = "MA",
                         "Michigan" = "MI", "Mississippi" = "MS",
                         "Missouri" = "MO",  "Nevada" = "NV",  "New Jersey" = "NJ",
                         "New Mexico" = "NM",
                         "New York" = "NY",
                         "North Carolina" = "NC","Ohio" = "OH",  "Oregon" = "OR",
                         "Pennsylvania" = "PA","Texas" = "TX", "Virginia" = "VA",
                         "Washington" = "WA",  "Wisconsin" = "WI")
queer.merge <- queer.merge %>%
  mutate(country = if_else(str_detect(country, "United States of America"),
                           str_replace(country, "United States of America, (.+)", 
                                       paste("United States,", state_abbreviations[str_trim(str_extract(country, "(?<=, ).+"))])),
                           country))

# Verify gender identity levels
levels(queer.merge$gender_identity) 
queer.merge %>%
  count(gender_identity) # Though there is only one agender participant, I will keep this level

# Verify sexual orientation levels
levels(queer.merge$sexual_orientation)
queer.merge %>%
  count(sexual_orientation)

queer.merge <- queer.merge %>%
  mutate(sexual_orientation = str_replace_all(sexual_orientation, "asexual", "Asexual")) %>% #unify the case
  filter(!sexual_orientation %in% c("Hetero", "heterosexual", "Heterosexual", "Straight", "Straight woman"))  #Delete straight samples

# Verify race levels
levels(queer.merge$race)
queer.merge %>% 
  count (race)

queer.merge <- queer.merge %>%
  mutate(race = str_replace_all(race, "Unknown or not reported", "Not specified")) # I will keep the racial identity level since it is irresponsible to replace multiple identities with inferences

# I suddenly felt that education does not matter in this study, delete
queer.merge <- queer.merge %>%
  select(-education)
