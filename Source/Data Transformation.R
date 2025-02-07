### Data Transformation

## Load libarary
install.packages("tidyverse")
library (tidyverse)

## Read in cleaned dataset
queer.cleaned <- read_csv("data/queer.cleaned.csv")

## Change data type
queer.cleaned <- queer.cleaned %>% 
  mutate (
    ID = as.character(ID),
    age = as.integer(age),
    gender_identity = as.factor(gender_identity), 
    transgender = as.factor(transgender),  
    sexual_orientation = as.factor(sexual_orientation),  
    race = as.factor(race),  
  ) %>% 
  mutate(across(20:51, as.numeric))

unique(queer.cleaned$frequency)
queer.cleaned$frequency <- factor(queer.cleaned$frequency,
                                levels = c("Never", "A few times per year", "A few times per month", "A few times per week", "Daily"),
                                ordered = TRUE) 

unique(queer.cleaned$actively_seek_out)
queer.cleaned$actively_seek_out <- factor(queer.cleaned$actively_seek_out,
                                        levels = c("Never", "Rarely", "Sometimes", "Often", "Always"),
                                        ordered = TRUE)

unique(queer.cleaned$discussion)
queer.cleaned$discussion <- factor(queer.cleaned$discussion,
                                 levels = c("Never", "Rarely", "Sometimes", "Often", "Always"),
                                 ordered = TRUE)


## Create a new variable to sum the authenticity rating for each participant

# Reverse code stereotype
queer.cleaned <- queer.cleaned %>%
  mutate(across(starts_with("stereotype"), ~ 8 - .))

# Create a new variable to sum the authenticity rating
queer.cleaned <- queer.cleaned %>%
  rowwise() %>%
  mutate(authenticity_rating = (sum(c_across(starts_with("authenticity"))) + sum(c_across(starts_with("stereotype")))) / 7) %>%
  ungroup()

queer.cleaned <- queer.cleaned %>%
  select(-c(20:23), -starts_with("stereotype")) %>%
  relocate(authenticity_rating, .after = other_name)


## Create a new variable to sum the intersectionality rating for each participant

queer.cleaned <- queer.cleaned %>%
  mutate(intersectionality_rating = rowMeans(across(starts_with("intersectionality")), na.rm = TRUE))

queer.cleaned <- queer.cleaned %>%
  select(-c(21:24)) %>%
  relocate(intersectionality_rating, .after = authenticity_rating)


## Create a new variable to sum the individual development score for each participant
queer.cleaned <- queer.cleaned %>%
  mutate(individual_development = rowMeans(across(starts_with("individual")), na.rm = TRUE))

queer.cleaned <- queer.cleaned %>%
  select(-c(22:28)) %>%
  relocate(individual_development, .after = intersectionality_rating)


## Create a new variable to sum the community belonging score for each participant
queer.cleaned <- queer.cleaned %>%
  mutate(community_belonging = rowMeans(across(starts_with("group")), na.rm = TRUE))

queer.cleaned <- queer.cleaned %>%
  select(-c(23:28)) %>%
  relocate(community_belonging, .after = individual_development)


## Create a new variable to sum the emotional well-being score for each participant
queer.cleaned <- queer.cleaned %>%
  mutate(emotional_well_being = rowMeans(across(starts_with("psychological")), na.rm = TRUE))

queer.cleaned <- queer.cleaned %>%
  select(-c(24:31)) %>%
  relocate(emotional_well_being, .after = community_belonging)


## Relocate mediators and outcome variables after media consumption habits

queer.cleaned <- queer.cleaned %>%
  relocate(c(20:24), .after = discussion) %>% 
  mutate(across(where(is.numeric), ~round(., 2))) 



## Create a new variable to sum the overall psychological well-being score for each participant
queer.cleaned <- queer.cleaned %>%
  mutate(
    psychological_wellbeing_score = case_when(
      is.na(individual_development) | is.na(community_belonging) | is.na(emotional_well_being) ~ NA_real_,  # If any column is NA, set result to NA
      TRUE ~ (7 * individual_development + 6 * community_belonging + 8 * emotional_well_being) / 21        # Otherwise, calculate the score
    )
  )

queer.cleaned <- queer.cleaned %>%
  relocate(psychological_wellbeing_score, .after = intersectionality_rating) %>% 
  mutate(across(where(is.numeric), ~round(., 2)))


## Split the dataset into two dataset, one for hypothesis testing, the other for data visualization of word cloud

queer.hypo <- queer.cleaned %>%
  select(1:16)

queer.media.type <- queer.cleaned %>%
  select(1, 17:25)


## Transform queer.media.type dataset to long format

# Rename media types in the first column
queer.media.type$type <- str_replace_all(queer.media.type$type, "Dramas", "Drama")
queer.media.type$type <- str_replace_all(queer.media.type$type, "Comedies", "Comedy")
queer.media.type$type <- str_replace_all(queer.media.type$type, "Documentaries", "Documentary")
queer.media.type$type <- str_replace_all(queer.media.type$type, "Action/Adventure", "Action")
queer.media.type$type <- str_replace_all(queer.media.type$type, "Science Fiction/Fantasy", "SciFi")
queer.media.type$type <- str_replace_all(queer.media.type$type, "Reality TV", "Reality")
queer.media.type$type <- str_replace_all(queer.media.type$type, "Others, please specify:", "Others")

# Transform the dataset to long format

media.long <- queer.media.type %>%
     pivot_longer(
    cols = ends_with("_name"),  
    names_to = "media_genre",   
    values_to = "media_name") %>%
    filter(!is.na(media_name)) %>%
    mutate(
    media_genre = gsub("_name", "", media_genre)  # Remove the "_name" suffix to match with the `type` column
     )

media.long <-media.long %>% 
   select(-type) %>% 
   mutate(media_genre = paste0(toupper(substring(media_genre, 1, 1)), substring(media_genre, 2)))


## Rearrange the long data
media.long <- media.long %>% 
  arrange(media_genre, media_name)


 # Separate the media name values with more than one name in a data cell
  media.long <- media.long %>%
  separate_rows(media_name, sep = ",") %>%
  mutate(media_name = trimws(media_name)) %>% 
  arrange(media_genre, media_name)
 
   
## Rename media names or delete invalid values
  
 media.long <- media.long %>%
   filter(!row_number() %in% c(7, 11, 59)) %>%
   filter(complete.cases(.)) %>% 
   mutate(media_name = str_replace_all(media_name, "Night Action", "Night Agent")) %>%
   mutate(media_name = str_replace_all(media_name, "Dery Girls", "Derry Girls")) %>%
  mutate(media_name = ifelse(startsWith(media_name, "Brooklyn"), "Brooklyn Nine-Nine",media_name)) %>% 
  mutate(media_name = str_replace_all(media_name, "Inside Out Universe Stuff", "Inside Out")) %>%
  mutate(media_name = str_replace_all(media_name, "Rocky Horror Picture Show", "Rocky Horror")) %>%
  mutate(media_name = str_replace_all(media_name, "Steve Harvey", "The Steve Harvey Show")) %>%
  separate_rows(media_name, sep = ";") %>%
  mutate(media_name = trimws(media_name)) %>% 
  arrange(media_genre, media_name)

 
 
 media.long <- media.long %>%  # This cleaning is based on the exact show names, invalid data including very general answers or unfound shows
  filter(!row_number() %in% c(94,95,99,102,103,149,154,155,168,175,177,179,180,181,182,183,185,186,192,193,204,208,218,239,271,272,273,275)) %>%
  mutate (media_name = str_replace_all(media_name, "Will & Grace", "Will And Grace")) %>%
  mutate (media_name = str_replace_all(media_name, "Portait Of Jackson Films By Barbara Hammer", "Portrait of Jackson")) %>%
  mutate (media_name = str_replace_all(media_name, "Su Friedrich..", "Su Friedrich")) %>%
  mutate (media_name = str_replace_all(media_name, "Boys Dont Cry", "Boys Don't Cry")) %>%
  mutate (media_name = str_replace_all(media_name, "Brookback Mountain", "Brokeback Mountain")) %>%
  mutate(media_name = case_when( media_name == "L Word" ~ "The L Word", TRUE ~ media_name)) %>%
  mutate (media_name = str_replace_all(media_name, "Portrait De La Jeune Fille En Feu", "Portrait Of A Lady On Fire")) %>%
  mutate (media_name = str_replace_all(media_name, "Schitts Creek", "Schitt's Creek")) %>%
  mutate (media_name = str_replace_all(media_name, "Veneno And Love", "Veneno")) %>%
  mutate (media_name = str_replace_all(media_name, "A Horror Film Called Saint Maude Featured Two Sapphic Women In The Cast", "Saint Maude")) %>%
  mutate (media_name = str_replace_all(media_name, "Anime Demon Slayer", "Demon Slayer")) %>%
  mutate (media_name = str_replace_all(media_name, "Horror Haunting Of Bly Manor", "The Haunting of Bly Manor")) %>%
  mutate (media_name = str_replace_all(media_name, "Interview With The Vampire Tv Show", "Interview With The Vampire")) %>%
  mutate (media_name = str_replace_all(media_name, "Sitcoms Like Will And Grace", "Will And Grace")) %>%
  mutate(media_name = case_when( media_name == "Drag Race" ~ "RuPaul's Drag Race", TRUE ~ media_name)) %>%
  mutate(media_name = case_when( media_name == "Ru Paul's Drag Race" ~ "RuPaul's Drag Race", TRUE ~ media_name)) %>%
  mutate(media_name = case_when( media_name == "Ru Pauls Drag Race" ~ "RuPaul's Drag Race", TRUE ~ media_name)) %>%
  mutate(media_name = case_when( media_name == "The Ultimatum" ~ "The Ultimatum: Queer Love", TRUE ~ media_name)) %>%
  mutate(media_name = case_when( media_name == "The Ultimatum (Queer Love)" ~ "The Ultimatum: Queer Love", TRUE ~ media_name)) %>%
  mutate(media_name = case_when( media_name == "The Ultimatum: Queen Love" ~ "The Ultimatum: Queer Love", TRUE ~ media_name)) %>%
  mutate(media_name = case_when( media_name == "Traitors" ~ "The Traitors", TRUE ~ media_name)) %>%
  mutate (media_name = str_replace_all(media_name, "Youtube - Tryguys", "The Tryguys")) %>%
  mutate (media_name = str_replace_all(media_name, "Heartstoppers", "Heartstopper")) %>%
  mutate(media_name = case_when( media_name == "Royal Blue" ~ "Red, White & Royal Blue", TRUE ~ media_name)) %>%
  mutate(media_name = case_when( media_name == "Red White Royal Blue" ~ "Red, White & Royal Blue", TRUE ~ media_name)) %>%
  mutate (media_name = str_replace_all(media_name, "Το Καλοκαιρι Της Κάρμεν", "The Summer With Carmen")) %>%
  mutate (media_name = str_replace_all(media_name, "Dr Who", "Doctor Who")) %>%
  mutate (media_name = str_replace_all(media_name, "Dr. Who", "Doctor Who")) %>%
  mutate (media_name = str_replace_all(media_name, "The First Kill", "First Kill")) %>% 
  mutate (media_name = str_replace_all(media_name, "Shera", "She-Ra")) %>% 
  mutate (media_name = case_when( media_name == "Walking Dead" ~ "The Walking Dead", TRUE ~ media_name))

 
 ## Reorder "others" to the end of the media genre
 media.long <- media.long %>%
   arrange(media_genre == "Other", media_genre, media_name)
 
 
 ## Write out the two transformed dataset
 write_csv(queer.hypo, "data/queer.hypo.csv") # This dataset is for hypothesis testing
 write_csv(media.long, "data/media.long.csv") # This dataset is for data visualization of word cloud
