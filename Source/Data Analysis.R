### Data Analysis


## Install and library packages
if(!require(tidyverse)){install.packages("tidyverse")}
library(tidyverse)
if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)
if(!require(ggiraph)){install.packages("ggiraph")}
library(ggiraph)
if(!require(RColorBrewer)){install.packages("RColorBrewer")}
library(RColorBrewer)
if(!require(viridis)){install.packages("viridis")}
library(viridis)
if(!require(maps)){install.packages("maps")}
library(maps)
if(!require(mapdata)){install.packages("mapdata")}
library(mapdata)
if(!require(gridExtra)){install.packages("gridExtra")}
library(gridExtra)
if(!require(plotly)){install.packages("plotly")}
library(plotly)
if(!require(psych)){install.packages("psych")}
library("psych")
if(!require(shiny)){install.packages("shiny")}
library("shiny")



## Load data
queer.construct <- read_csv("data/queer.cleaned.csv")
queer.hypo <- read_csv("data/queer.hypo.csv")
media.long <- read_csv("data/media.long.csv")

## PART I: Construct reliability tests

# Reliability of authenticity rating measurement (4 authenticity & 3 stereotype items)
psych::alpha(queer.construct[,20:26],check.keys=TRUE) # Reverse code measurements of stereotypes
# Alpha = 0.76 (satisfactory)

# Reliability of intersectionality rating measurement (4 intersectionality items)
psych::alpha(queer.construct[,27:30]) 
# Alpha = 0.91 (excellent)

# Reliability of media individual development measurement (7 items)
psych::alpha(queer.construct[,31:37])
# Alpha = 0.96 (excellent)

# Reliability of media community belonging measurement (6 items)
psych::alpha(queer.construct[,38:43])
# Alpha = 0.96 (excellent)

# Reliability of emotional well-being measurement (8 items)
psych::alpha(queer.construct[,44:51])
# Alpha = 0.96 (excellent)

# Reliability of overall psychological well-being measurement (21 items)
psych::alpha(queer.construct[,31:51])
# Alpha = 0.98 (excellent)

## Change data type
queer.hypo <- queer.hypo %>% 
  mutate (
    ID = as.character(ID),
    age = as.integer(age),
    gender_identity = as.factor(gender_identity), 
    transgender = as.factor(transgender),  
    sexual_orientation = as.factor(sexual_orientation),  
    race = as.factor(race))
    
    unique(queer.hypo$frequency)
    queer.hypo$frequency <- factor(queer.hypo$frequency,
                                      levels = c("Never", "A few times per year", "A few times per month", "A few times per week", "Daily"),
                                      ordered = TRUE) 
    
    unique(queer.hypo$actively_seek_out)
    queer.hypo$actively_seek_out <- factor(queer.hypo$actively_seek_out,
                                              levels = c("Never", "Rarely", "Sometimes", "Often", "Always"),
                                              ordered = TRUE)
    
    unique(queer.hypo$discussion)
    queer.hypo$discussion <- factor(queer.hypo$discussion,
                                       levels = c("Never", "Rarely", "Sometimes", "Often", "Always"),
                                       ordered = TRUE)

    
    
## PART II: Descriptive statistics

## Demographic

    
# 1. Age
#Age descriptive: Cross-generation

mean_age <- mean(queer.hypo$age, na.rm = TRUE)
sd_age <- sd(queer.hypo$age, na.rm = TRUE)
min_age <- min(queer.hypo$age, na.rm = TRUE)
max_age <- max(queer.hypo$age, na.rm = TRUE)
median_age <- median(queer.hypo$age, na.rm = TRUE)

summary_stats_age <- data.frame(
  Statistic = c("Mean", "Standard Deviation", "Minimum", "Maximum", "Median"),
  Value = c(mean_age, sd_age, min_age, max_age, median_age))

print(summary_stats_age)

# Age visualization
boxplot(queer.hypo$age, main = "Age Distribution", ylab = "Age")



# 2. Country
# Country descriptive: Cross-national
queer.hypo.country <-queer.hypo %>% 
  mutate(country = str_replace_all(country, "^United States.*", "United States")) %>%
  group_by(country) %>%
  summarise(count = n(), .groups = 'drop')


# Country visualization: Bar plot
# Define UI
 ui_country_bar <- fluidPage(
     titlePanel("Participants' Country Distribution", windowTitle = "Country Distribution"),
     girafeOutput("countryPlot"))  # Use girafeOutput
     
# Define server logic
server <- function(input, output) {
  
  # Plotting
  output$countryPlot <- renderGirafe({
    # Create a tooltip that includes both the country and the count
    queer.hypo.country$tooltip <- paste(queer.hypo.country$country, ":", queer.hypo.country$count)
    
    p <- ggplot(queer.hypo.country, aes(x = reorder(country, -count), y = count, tooltip = tooltip, data_id = country)) +
      geom_bar_interactive(stat = "identity", aes(fill = country), show.legend = FALSE) +
      labs(title = "Number of Participants by Country",
           x = "Country",
           y = "Number of Participants") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8)  
      ) +
      scale_fill_viridis_d()  
    
    girafe(ggobj = p)
  })
}

# Run the app
shinyApp(ui = ui_country_bar, server = server)


# Country visualization: World map

# Prepare world map data
  queer.hypo.country <- queer.hypo.country %>%
  mutate(country = recode(country,
                          `United Kingdom` = "UK",
                          `United States` = "USA"))
  
world_map <- map_data("world")

# Merge with participant data
map_data <- world_map %>%
  left_join(queer.hypo.country, by = c("region" = "country")) %>%
  mutate(tooltip = paste(region, ":", count))  # Create tooltip for each region

# Define UI
ui_country_map <- fluidPage(
  titlePanel("Participants' Country Distribution on Interactive World Map"),
  girafeOutput("countryMap")  # Output for the interactive map
)

# Define server logic
server <- function(input, output) {
  
  output$countryMap <- renderGirafe({
    p <- ggplot(map_data, aes(x = long, y = lat, group = group, fill = count, tooltip = tooltip, data_id = region)) +
      geom_polygon_interactive(color = "#0073CF") +  # Use interactive polygons
      scale_fill_viridis_c(option = "plasma", na.value = "lightgrey") +  # Color scale
      labs(title = "Number of Participants by Country",
           fill = "Number of Participants") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),  # Center title and increase size
        legend.position = "bottom"
      )
    
    girafe(ggobj = p)
  })
}

# Run the app
shinyApp(ui = ui_country_map, server = server)



# 3. Sexuality

# Frequency count
freq_gender_identity <- table(queer.hypo$gender_identity)
print(freq_gender_identity)

freq_transgender <- table(queer.hypo$transgender)
print(freq_transgender)

freq_sexual_orientation <- table(queer.hypo$sexual_orientation)
print(freq_sexual_orientation)

# Recode transgender status
queer.hypo$transgender <- recode(queer.hypo$transgender, `0` = "No", `1` = "Yes")


# Visualization: Parallel pie charts
# Define UI
ui_sexuality <- fluidPage(
  titlePanel("Gender Identity, Transgender Status, and Sexual Orientation"),
  fluidRow(
    column(4, girafeOutput("genderPieChart",height = "400px")),  # Gender identity pie chart
    column(4, girafeOutput("transgenderPieChart",height = "400px")),  # Transgender status pie chart
    column(4, girafeOutput("orientationPieChart",height = "400px"))  # Sexual orientation pie chart
  )
)


# Define server logic
server <- function(input, output) {
  
  output$genderPieChart <- renderGirafe({
    gender_data <- queer.hypo %>%
      group_by(gender_identity) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(percentage = count / sum(count) * 100)
    
    p <- ggplot(gender_data, aes(x = "", y = count, fill = gender_identity)) +
      geom_bar_interactive(aes(tooltip = paste(gender_identity, ": ", count, " (", round(percentage, 1), "%)", sep = "")), stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(title = "Gender Identity") +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "bottom",
        legend.text = element_text(size = 7)
      ) +
      scale_fill_viridis_d()
    
    girafe(ggobj = p, width_svg = 6, height_svg = 6)
  })
  
  output$transgenderPieChart <- renderGirafe({
    transgender_data <- queer.hypo %>%
      group_by(transgender) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(percentage = count / sum(count) * 100)
    
    p <- ggplot(transgender_data, aes(x = "", y = count, fill = transgender)) +
      geom_bar_interactive(aes(tooltip = paste(transgender, ": ", count, " (", round(percentage, 1), "%)", sep = "")), stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(title = "Transgender Status") +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "bottom",
        legend.text = element_text(size = 8)
      ) +
      scale_fill_viridis_d()
    
    girafe(ggobj = p, width_svg = 6, height_svg = 6)
  })
  
  output$orientationPieChart <- renderGirafe({
    orientation_data <- queer.hypo %>%
      group_by(sexual_orientation) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(percentage = count / sum(count) * 100)
    
    p <- ggplot(orientation_data, aes(x = "", y = count, fill = sexual_orientation)) +
      geom_bar_interactive(aes(tooltip = paste(sexual_orientation, ": ", count, " (", round(percentage, 1), "%)", sep = "")), stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(title = "Sexual Orientation") +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "bottom",
        legend.text = element_text(size = 8)
      ) +
      scale_fill_viridis_d()
    
    girafe(ggobj = p, width_svg = 6, height_svg = 6)
  })
}

# Run the app
shinyApp(ui = ui_sexuality, server = server)


# 4. Race

# Frequency Count
freq_race <- table(queer.hypo$race)
print(freq_race)

# Visualization: Pie chart

# Define UI
ui <- fluidPage(
  titlePanel("Race Distribution"),
  girafeOutput("racePieChart", height = "1150px")
)

# Define server logic
server <- function(input, output) {
  output$racePieChart <- renderGirafe({
    # Summarize data for race
    race_data <- queer.hypo %>%
      group_by(race) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(percentage = count / sum(count) * 100)  # Calculate percentage
    
    # Create interactive pie chart for race
    p <- ggplot(race_data, aes(x = "", y = count, fill = race)) +
      geom_bar_interactive(aes(tooltip = paste(race, ": ", count, " (", round(percentage, 1), "%)", sep = "")), 
                           stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(title = "Race") +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 24),
        legend.position = "bottom",
        legend.text = element_text(size = 10)
      ) +
      scale_fill_viridis_d()  # Use a beautiful color palette
    
    girafe(ggobj = p, width_svg = 15, height_svg = 15)
  })
}

# Run the app
shinyApp(ui = ui, server = server)



## PART III: Hypothesis Testing

# Hypothesis 1: The impacts of six categorical variables' impacts on queer characters' psychological impacts (numeric variables)
# Specifically, the six categorical variables are age, country, gender identity, transgender status, sexual orientation, and race

# 1. Age (NO)

# Generation comparison on queer characters' psychological effects

queer.hypo <- queer.hypo %>%
  mutate(generation = case_when(
    age >= 18 & age <= 29 ~ "Gen Z",
    age >= 30 & age <= 45 ~ "Millennials",
    TRUE ~ "Gen X or Baby Boomer"
  )) %>%
  relocate(generation, .after = age)

anova_age_effect <- aov(psychological_wellbeing_score ~ generation, data = queer.hypo)
summary(anova_age_effect)

# p > .70, no significant difference has been found among different generations

# Visualization: Boxplot
# Define a theme
theme.clean <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 12, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 16, vjust = 1, hjust = 0.5),
          plot.caption = element_text (size = 8,vjust = 1, hjust = 0.9),
          legend.text = element_text(size = 12, face = "italic"),          
          legend.position = "right")}



age_effect_boxplot <- ggplot(queer.hypo, aes(generation, psychological_wellbeing_score)) + 
    geom_boxplot(aes(fill = generation)) +
    theme_bw() +
    scale_fill_manual(values = c("#5d2fb2", "#dab099", "#545b4b")) +               
    scale_colour_manual(values = c("#5d2fb2", "#dab099", "#545b4b")) + 
    labs(title = "Generation Comparison of Queer Characters' Psychological Impacts", 
         x = "Generation", y = "Queer Characters' Psychological Impact Score") +
    theme.clean()+
    theme(axis.text.x = element_text(hjust = 0.5))+
  scale_y_continuous(breaks = seq(floor(min(queer.hypo$psychological_wellbeing_score)), 
                                  ceiling(max(queer.hypo$psychological_wellbeing_score)), by = 1)) +
  stat_summary(fun = mean, geom = "point", color = "white", size = 3, shape = 18, 
               position = position_dodge(0.75), aes(shape = "Mean"), 
               show.legend = FALSE) +  
  stat_summary(fun = mean, geom = "text", aes(label = round(after_stat(y), 2)), 
               color = "white", vjust = -1, position = position_dodge(0.75), 
               size = 4, show.legend = FALSE) +
  labs(fill = "Generation") 


print (age_effect_boxplot)

# The visualization indicated that there is no significant differences among three generations. 



# 2. Country(Developing countries have higher scores: Significant)

# Identify country as developed or developing

classify_country <- function(country) {
  # Check if the country starts with "United States"
  if (grepl("^United States", country)) {
    return("Developed")
  }
  
  developed_countries <- c("Canada", "Czech Republic", "France", "Germany", 
                           "Greece", "Ireland", "Japan", "Latvia", 
                           "Norway", "Poland", "Portugal", "Spain", 
                           "Sweden", "United Kingdom", "United States")
  
  if (country %in% developed_countries) {
    return("Developed")
  } else {
    return("Developing")
  }
}

queer.hypo$development_status <- sapply(queer.hypo$country, classify_country)

queer.hypo <- queer.hypo %>% 
  relocate(development_status, .after = country)


anova_country_effect <- aov(psychological_wellbeing_score ~ development_status, data = queer.hypo)
summary(anova_country_effect)

# p = .014, F = 6.16, significant difference has been found between developed and developing countries

# Visualization: Boxplot
country_effect_boxplot <- ggplot(queer.hypo, aes(development_status, psychological_wellbeing_score)) + 
  geom_boxplot(aes(fill = development_status)) +
  theme_bw() +
  scale_fill_manual(values = c("#dbf1e5", "#4fba7f")) +               
  scale_colour_manual(values = c("#dbf1e5", "#4fba7f")) + 
  labs(title = "Country Comparison of Queer Characters' Psychological Impacts", 
       x = "Development Status", y = "Queer Characters' Psychological Impact Score") +
  theme.clean()+
  theme(axis.text.x = element_text(hjust = 0.5))+
  scale_y_continuous(breaks = seq(floor(min(queer.hypo$psychological_wellbeing_score)), 
                                  ceiling(max(queer.hypo$psychological_wellbeing_score)), by = 1)) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3, shape = 18, 
               position = position_dodge(0.75), aes(shape = "Mean"), 
               show.legend = FALSE) +  
  stat_summary(fun = mean, geom = "text", aes(label = round(after_stat(y), 2)), 
               color = "black", vjust = -1, position = position_dodge(0.75), 
               size = 4, show.legend = FALSE) +
  labs(fill = "Development Status") 

print (country_effect_boxplot)


# 3. Gender identity (Women/nonbinary/agender have higher scores, men have more low scores)
queer.hypo$gender_identity <- as.character(queer.hypo$gender_identity)

# Create the dual gender identity column using ifelse
queer.hypo <- queer.hypo %>%
  mutate(gender_identity_dual = ifelse(gender_identity == "Man", "Man", 
                                       "Women, or nonbinary, or agender")) %>%
  relocate(gender_identity_dual, .after = gender_identity)


anova_gender_identity_effect <- aov(psychological_wellbeing_score ~ gender_identity_dual, data = queer.hypo)
summary (anova_gender_identity_effect)

# p = .11, no significant difference has been found among different gender identity

# Visualization: Violinplot

gender_identity_effect_violinplot <- ggplot(queer.hypo, aes(gender_identity_dual, psychological_wellbeing_score)) + 
  geom_violin(aes(fill = gender_identity_dual)) +
  theme_bw() +
  scale_fill_manual(values = c("#6fa8dc", "#d5a6bd")) +               
  scale_colour_manual(values = c("#6fa8dc", "#d5a6bd")) + 
  labs(title = "Gender Identity Difference of Queer Characters' Psychological Impacts", 
       x = "Gender Identity", y = "Queer Characters' Psychological Impact Score") +
  theme.clean()+
  theme(axis.text.x = element_text(hjust = 0.5))+
  scale_y_continuous(breaks = seq(floor(min(queer.hypo$psychological_wellbeing_score)), 
                                  ceiling(max(queer.hypo$psychological_wellbeing_score)), by = 1)) +
  stat_summary(fun = mean, geom = "point", color = "white", size = 3, shape = 18, 
               position = position_dodge(0.75), aes(shape = "Mean"), 
               show.legend = FALSE) +  
  stat_summary(fun = mean, geom = "text", aes(label = round(after_stat(y), 2)), 
               color = "white", vjust = -1, position = position_dodge(0.75), 
               size = 4, show.legend = FALSE) +
  labs(fill = "Gender Identity") 

print (gender_identity_effect_violinplot)


# 4. Transgender status (transgender has higher wellbeing)

anova_transgender_effect <- aov(psychological_wellbeing_score ~ transgender, data = queer.hypo)
summary (anova_transgender_effect)

# p = .25, no significant difference has been found among different gender identity

# Visualization: Violinplot
transgender_effect_violinplot <- ggplot(queer.hypo, aes(transgender, psychological_wellbeing_score)) + 
  geom_violin(aes(fill = transgender)) +
  theme_bw() +
  theme(axis.text.x = element_text(hjust = 0.5, angle = 0, vjust = 0.5))+
  scale_fill_manual(values = c("#5BCEFA", "#F5A9B8")) +               
  scale_colour_manual(values = c("#5BCEFA", "#F5A9B8")) + 
  labs(title = "Transgender Status Difference of Queer Characters' Psychological Impacts", 
       x = "Transgender Status", y = "Queer Characters' Psychological Impact Score") +
  theme.clean()+
  scale_y_continuous(breaks = seq(floor(min(queer.hypo$psychological_wellbeing_score)), 
                                  ceiling(max(queer.hypo$psychological_wellbeing_score)), by = 1)) +
  stat_summary(fun = mean, geom = "point", color = "white", size = 3, shape = 18, 
               position = position_dodge(0.75), aes(shape = "Mean"), 
               show.legend = FALSE) +  
  stat_summary(fun = mean, geom = "text", aes(label = round(after_stat(y), 2)), 
               color = "white", vjust = -1, position = position_dodge(0.75), 
               size = 4, show.legend = FALSE) +
  labs(fill = "Transgender Status") 

print (transgender_effect_violinplot)



# 5. Sexual orientation (Bisexual has the lowerst wellbeing, asexual has the most outliers)
anova_sexual_orientation_effect <- aov(psychological_wellbeing_score ~ sexual_orientation, data = queer.hypo)
summary (anova_sexual_orientation_effect)

# p = 0.1, no significant differences have been found among different sexual orientation

# Filter three identified as queer and questioning between asexual and bisexual
queer.hypo_filtered <- queer.hypo %>%
  filter(!sexual_orientation %in% c("Queer", "Questioning between Asexual or bisexual"))

queer.hypo_filtered$sexual_orientation <- as.factor(queer.hypo_filtered$sexual_orientation)

# Make color palettes for data point visualization for each sexual orientation
color_palette <- list(
  "Asexual" = c("#000000", "#A3A3A3", "#FFFFFF", "#800080"),
  "Bisexual" = c("#D60270", "#9B4F96", "#0038A8"),
  "Gay or Lesbian" = c("#078D70", "#26CEAA", "#98E8C1", "#FFFFFF", "#7BADE2", "#5049CC",
                       "#3D1A78", "#D52D00", "#EF7627", "#FF9A56", "#FFFFFF", "#D162A4",
                       "#B55690", "#A30262"),
  "Pansexual" = c("#FF218C", "#FFD800", "#21B1FF")
)


# Function to get the middle color of each palette
get_middle_color <- function(colors) {
  mid <- ceiling(length(colors) / 2)
  return(colors[mid])
}

# Create a named vector of middle colors for each orientation
middle_colors <- sapply(color_palette, get_middle_color)

# Calculate mean for each group
mean_data <- queer.hypo_filtered %>%
  group_by(sexual_orientation) %>%
  summarise(mean_score = mean(psychological_wellbeing_score, na.rm = TRUE))

# Create the plotly boxplot
orientation_effect_boxplot <- plot_ly(queer.hypo_filtered, 
                                      x = ~sexual_orientation, 
                                      y = ~psychological_wellbeing_score, 
                                      type = "box",
                                      color = ~sexual_orientation,
                                      colors = middle_colors) %>%
  layout(title = "Sexual Orientation Comparison of Queer Characters' Psychological Impacts",
         xaxis = list(title = "Sexual Orientation"),
         yaxis = list(title = "Queer Characters' Psychological Impact Score"),
         showlegend = TRUE,
         legend = list(
           x = 1,  
           xanchor = "center",  
           y = 0.5,  
           yanchor = "top"  
         )) %>%
  add_markers(data = mean_data, 
              x = ~sexual_orientation, 
              y = ~mean_score, 
              marker = list(color = "white", size = 10, symbol = "diamond"),
              showlegend = FALSE) %>%
  add_text(data = mean_data,
           x = ~sexual_orientation,
           y = ~mean_score,
           text = ~round(mean_score, 2),
           textposition = "top center",
           showlegend = FALSE)

# Number of points to display
n_points <- 10

# Add sampled data points with individual colors from the flag palette
set.seed(123)  # Ensure reproducibility
for (orientation in names(color_palette)) {
  orientation_data <- queer.hypo_filtered %>%
    filter(sexual_orientation == orientation) %>%
    sample_n(min(n_points, n()))  # Sample points
  
  orientation_colors <- color_palette[[orientation]]
  
  orientation_effect_boxplot <- orientation_effect_boxplot %>%
    add_markers(data = orientation_data, 
                x = ~sexual_orientation, 
                y = ~psychological_wellbeing_score, 
                marker = list(color = orientation_colors, size = 5, opacity = 0.6),
                showlegend = FALSE)
}

# Display the plot
orientation_effect_boxplot

# Gay/lesbian has the highest score, bisexual has the lowest



# 6. Race (white has lower wellbeing than other races)
queer.hypo <- queer.hypo %>%
  mutate(race_category = ifelse(race == "White", "White", "Black, Asian, Indian, and Others")) %>% 
  relocate(race_category, .after = race)

anova_race_effect <- aov(psychological_wellbeing_score ~ race_category, data = queer.hypo)
summary(anova_race_effect)


# p = .09, marginally significant difference has been found between white and other

# Visualization: Violinplot
race_effect_boxplot <- ggplot(queer.hypo, aes(race_category, psychological_wellbeing_score)) + 
  geom_boxplot(aes(fill = race_category)) +
  theme_bw() +
  theme(axis.text.x = element_text(hjust = 0.5))+
  scale_fill_manual(values = c("#cfb5a0","#f1e2c8")) +               
  scale_colour_manual(values = c("#cfb5a0","#f1e2c8")) + 
  labs(title = "Race Comparison of Queer Characters' Psychological Impacts", 
       x = "Racial Identity", y = "Queer Characters' Psychological Impact Score") +
  theme.clean()+
  scale_y_continuous(breaks = seq(floor(min(queer.hypo$psychological_wellbeing_score)), 
                                  ceiling(max(queer.hypo$psychological_wellbeing_score)), by = 1)) +
  stat_summary(fun = mean, geom = "point", color = "white", size = 3, shape = 18, 
               position = position_dodge(0.75), aes(shape = "Mean"), 
               show.legend = FALSE) +  
  stat_summary(fun = mean, geom = "text", aes(label = round(after_stat(y), 2)), 
               color = "white", vjust = -1, position = position_dodge(0.75), 
               size = 4, show.legend = FALSE) +
  labs(fill = "Racial Identity") 

print (race_effect_boxplot)


# From the above, it is valuable to focus on the country, gender identity, sexual orientation, and racial identity's impact

# I want to see if these have differences on queer media consumption/engagement

# 1. Country

# Create the parallel bubble plot

create_country_bubble_plot <- function(queer.hypo, y_var, title) {
  ggplot(queer.hypo, aes(x = development_status, y = !!sym(y_var))) +
    geom_count(aes(size = ..n..), alpha = 0.6, color = "blue") +  
    labs(title = title,
         x = "Development Status",
         y = y_var) +
    scale_size_area(max_size = 15, name = "Count") +  
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_text(hjust = 0.5),  
      axis.text.x = element_text(hjust = 0.5),  
      legend.position = "bottom",  
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),  
      legend.key.size = unit(0.5, "cm"),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")  # Reduce margin
    ) +
    guides(size = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5))}  # Adjust legend layout

country_freq_bubbleplot <- create_country_bubble_plot(queer.hypo, "frequency", "Frequency")
country_seek_bubbleplot <- create_country_bubble_plot(queer.hypo, "actively_seek_out", "Actively Seek Out Queer Representations")
country_discussion_bubbleplot <- create_country_bubble_plot(queer.hypo, "discussion", "Discussion Engagement of Queer Media Representations")

combined_country_bubble_plot <- grid.arrange(country_freq_bubbleplot, country_seek_bubbleplot, country_discussion_bubbleplot, ncol = 3)

# Frequency plot showed that developed countries have more participants who engaged less with queer media
# There are only 22 participants in the developing countries, the percentage for seek out and engagement are quite similar to developed countries


# 2. Gender Identity

# Create the parallel bubble plot

create_gender_identity_bubble_plot <- function(queer.hypo, y_var, title) {
  ggplot(queer.hypo, aes(x = gender_identity_dual, y = !!sym(y_var))) +
    geom_count(aes(size = ..n..), alpha = 0.6, color = "blue") +  
    labs(title = title,
         x = "Gender Identity",
         y = y_var) +
    scale_size_area(max_size = 15, name = "Count") +  
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_text(hjust = 0.5),  
      axis.text.x = element_text(hjust = 0.5),  
      legend.position = "bottom",  
      legend.title = element_text(size = 8),  
      legend.text = element_text(size = 7),  
      legend.key.size = unit(0.5, "cm"),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")  
    ) +
    guides(size = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5))}  

gender_identity_freq_bubbleplot <- create_gender_identity_bubble_plot(queer.hypo, "frequency", "Frequency")
gender_identity_seek_bubbleplot <- create_gender_identity_bubble_plot(queer.hypo, "actively_seek_out", "Actively Seek Out Queer Representations")
gender_identity_discussion_bubbleplot <- create_gender_identity_bubble_plot(queer.hypo, "discussion", "Discussion Engagement of Queer Media Representations")

combined_gender_identity_bubble_plot <- grid.arrange(gender_identity_freq_bubbleplot, gender_identity_seek_bubbleplot, gender_identity_discussion_bubbleplot, ncol = 3)

# Women, nonbinary, agender have higher discussion frequency of queer media representations


# 3. Sexual Orientation

# Create the parallel bubble plot

create_sexual_orientation_bubble_plot <- function(queer.hypo_filtered, y_var, title) {
  ggplot(queer.hypo_filtered, aes(x = sexual_orientation, y = !!sym(y_var))) +
    geom_count(aes(size = ..n..), alpha = 0.6, color = "blue") +  
    labs(title = title,
         x = "Sexual Orientation",
         y = y_var) +
    scale_size_area(max_size = 15, name = "Count") +  
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_text(hjust = 0.5),  
      axis.text.x = element_text(hjust = 0.5),  
      legend.position = "bottom",  
      legend.title = element_text(size = 8),  
      legend.text = element_text(size = 7),  
      legend.key.size = unit(0.5, "cm"),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")  
    ) +
    guides(size = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5))}  

sexual_orientation_freq_bubbleplot <- create_sexual_orientation_bubble_plot(queer.hypo, "frequency", "Frequency")
sexual_orientation_seek_bubbleplot <- create_sexual_orientation_bubble_plot(queer.hypo, "actively_seek_out", "Actively Seek Out Queer Representations")
sexual_orientation_discussion_bubbleplot <- create_sexual_orientation_bubble_plot(queer.hypo, "discussion", "Discussion Engagement of Queer Media Representations")

combined_sexual_orientation_bubble_plot <- grid.arrange(sexual_orientation_freq_bubbleplot, sexual_orientation_seek_bubbleplot, sexual_orientation_discussion_bubbleplot, ncol = 3)

# 4. Racial Identity
# Create the parallel bubble plot

create_racial_identity_bubble_plot <- function(queer.hypo, y_var, title) {
  ggplot(queer.hypo, aes(x = race_category, y = !!sym(y_var))) +
    geom_count(aes(size = ..n..), alpha = 0.6, color = "blue") +  
    labs(title = title,
         x = "Racial Identity",
         y = y_var) +
    scale_size_area(max_size = 15, name = "Count") +  
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_text(hjust = 0.5),  
      axis.text.x = element_text(hjust = 0.5),  
      legend.position = "bottom",  
      legend.title = element_text(size = 8),  
      legend.text = element_text(size = 7),  
      legend.key.size = unit(0.5, "cm"),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")  
    ) +
    guides(size = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5))}  

racial_identity_freq_bubbleplot <- create_racial_identity_bubble_plot(queer.hypo, "frequency", "Frequency")
racial_identity_seek_bubbleplot <- create_racial_identity_bubble_plot(queer.hypo, "actively_seek_out", "Actively Seek Out Queer Representations")
racial_identity_discussion_bubbleplot <- create_racial_identity_bubble_plot(queer.hypo, "discussion", "Discussion Engagement of Queer Media Representations")

combined_racial_identity_bubble_plot <- grid.arrange(racial_identity_freq_bubbleplot, racial_identity_seek_bubbleplot, racial_identity_discussion_bubbleplot, ncol = 3)


# Hypothesis 2: Perceived authenticity rating and intersectionality rating are positively correlated with queer characters' psychological impacts

# 1. Perceived authenticity rating (strongly positive correlation)

authenticity.effect <- lm(psychological_wellbeing_score ~ authenticity_rating, data = queer.hypo)
summary(authenticity.effect) 

cor_auth_psych <- cor(queer.hypo$authenticity_rating, queer.hypo$psychological_wellbeing_score)
print (cor_auth_psych)

# The results show that the relationship between psychological wellbeing and authenticity rating is statistically significant (p < .01), r = 0.58, R-squared = 0.337, explaining 33.7% of the result. 


# Visualization: Scatterplot

# Visualization 1: Authenticity rating's impacts on queer characters' psychological impacts by development status

auth.psych.dev <- ggplot(queer.hypo, aes(x = authenticity_rating, y = psychological_wellbeing_score)) +
  geom_point(aes(colour = development_status, shape = development_status)) +                                
  labs(x = "Authenticity Rating", y = "Queer Characters' Psychological Impact Score") +
  labs(title = "Authenticity Rating's Impacts on Queer Characters' Psychological Effects by Development Status") + 
  scale_y_continuous(breaks = seq(floor(min(queer.hypo$psychological_wellbeing_score)), 
                                  ceiling(max(queer.hypo$psychological_wellbeing_score)), by = 1)) +
  scale_x_continuous(
    breaks = seq(floor(min(queer.hypo$authenticity_rating)), 
                 ceiling(max(queer.hypo$authenticity_rating)), by = 1),
    limits = c(floor(min(queer.hypo$authenticity_rating)), ceiling(max(queer.hypo$authenticity_rating)))
  ) +
  stat_smooth(method = "lm", aes(fill = development_status, colour = development_status)) +    
  scale_colour_manual(name = "Development Status", values = c("#FFC125", "#36648B")) +
  scale_fill_manual(name = "Development Status",values = c("#FFC125", "#36648B")) +
  scale_shape_discrete(name = "Development Status") +
  theme.clean() 

print(auth.psych.dev)

# The visualization indicated that developed countries feature more data points with low authenticity rating and low psychological impact scores.
# Thus, it is plausible that they are more critical/sensitive about authenticity and stereotypes, resulting in lower wellbeing scores. 

# Visualization 2: Authenticity rating's impacts on queer characters' psychological impacts by gender identity

auth.psych.gender <- ggplot(queer.hypo, aes(x = authenticity_rating, y = psychological_wellbeing_score)) +
  geom_point(aes(colour = gender_identity_dual, shape = gender_identity_dual)) +                                
  labs(x = "Authenticity Rating", y = "Queer Characters' Psychological Impact Score") +
  labs(title = "Authenticity Rating's Impacts on Queer Characters' Psychological Effects by Gender Identity") + 
  scale_y_continuous(breaks = seq(floor(min(queer.hypo$psychological_wellbeing_score)), 
                                  ceiling(max(queer.hypo$psychological_wellbeing_score)), by = 1)) +
  scale_x_continuous(
    breaks = seq(floor(min(queer.hypo$authenticity_rating)), 
                 ceiling(max(queer.hypo$authenticity_rating)), by = 1),
    limits = c(floor(min(queer.hypo$authenticity_rating)), ceiling(max(queer.hypo$authenticity_rating)))
  ) +
  stat_smooth(method = "lm", aes(fill = gender_identity_dual, colour = gender_identity_dual)) +    
  scale_colour_manual(name = "Gender Identity", values = c("#6fa8dc", "#d5a6bd")) +
  scale_fill_manual(name = "Gender Identity", values = c("#6fa8dc", "#d5a6bd")) +
  scale_shape_discrete(name = "Gender Identity") +
  theme.clean() 

print(auth.psych.gender)



# Visualization 3: Authenticity rating's impacts on queer characters' psychological impacts by sexual orientation
auth.psych.orientation <- ggplot(queer.hypo_filtered, aes(x = authenticity_rating, y = psychological_wellbeing_score)) +
  geom_point(aes(colour = sexual_orientation, shape = sexual_orientation)) +                                
  labs(x = "Authenticity Rating", y = "Queer Characters' Psychological Impact Score") +
  labs(title = "Authenticity Rating's Impacts on Queer Characters' Psychological Effects by Sexual Orientation") + 
  scale_y_continuous(breaks = seq(floor(min(queer.hypo$psychological_wellbeing_score)), 
                                  ceiling(max(queer.hypo$psychological_wellbeing_score)), by = 1)) +
  scale_x_continuous(
    breaks = seq(floor(min(queer.hypo$authenticity_rating)), 
                 ceiling(max(queer.hypo$authenticity_rating)), by = 1),
    limits = c(floor(min(queer.hypo$authenticity_rating)), ceiling(max(queer.hypo$authenticity_rating)))
  ) +
  stat_smooth(method = "lm", aes(fill = sexual_orientation, colour = sexual_orientation)) +    
  scale_colour_manual(name = "Sexual Orientation", values = c("#A3A3A3","#9B4F96","#3D1A78","#FFD800")) +
  scale_fill_manual(name = "Sexual Orientation", values = c("#A3A3A3","#9B4F96","#3D1A78","#FFD800")) +
  scale_shape_discrete(name = "Sexual Orientation") +
  facet_wrap(~sexual_orientation) +
  theme.clean() 

print(auth.psych.orientation)

# Bisexual has the lowest authenticity rating and psychological impact

# Visualization 4: Authenticity rating's impacts on queer characters' psychological impacts by racial identity

auth.psych.race <- ggplot(queer.hypo, aes(x = authenticity_rating, y = psychological_wellbeing_score)) +
  geom_point(aes(colour = race_category, shape = race_category)) +                                
  labs(x = "Authenticity Rating", y = "Queer Characters' Psychological Impact Score") +
  labs(title = "Authenticity Rating's Impacts on Queer Characters' Psychological Effects by Racial Identity") + 
  scale_y_continuous(breaks = seq(floor(min(queer.hypo$psychological_wellbeing_score)), 
                                  ceiling(max(queer.hypo$psychological_wellbeing_score)), by = 1)) +
  scale_x_continuous(
    breaks = seq(floor(min(queer.hypo$authenticity_rating)), 
                 ceiling(max(queer.hypo$authenticity_rating)), by = 1),
    limits = c(floor(min(queer.hypo$authenticity_rating)), ceiling(max(queer.hypo$authenticity_rating)))
  ) +
  stat_smooth(method = "lm", aes(fill = race_category, colour = race_category)) +    
  scale_colour_manual(name = "Racial Identity", values = c("#cfb5a0","#f1e2c8")) +
  scale_fill_manual(name = "Racial Identity", values = c("#cfb5a0","#f1e2c8")) +
  scale_shape_discrete(name = "Racial Identity") +
  theme.clean() 

print(auth.psych.race)


# 2. Perceived intersectionality rating (strongly positive correlation)
intersectionality.effect <- lm(psychological_wellbeing_score ~ intersectionality_rating, data = queer.hypo)
summary(intersectionality.effect) 

cor_inter_psych <- cor(queer.hypo$intersectionality_rating, queer.hypo$psychological_wellbeing_score)
print (cor_inter_psych)

# The results show that the relationship between psychological wellbeing and authenticity rating is statistically significant (p < .001), r = 0.48, R-squared = 0.233, explaining 23.3% of the result. 

# Visualization: Scatterplot

# Visualization 1: Intersectionality rating's impacts on queer characters' psychological impacts by development status

inter.psych.dev <- ggplot(queer.hypo, aes(x = intersectionality_rating, y = psychological_wellbeing_score)) +
  geom_point(aes(colour = development_status, shape = development_status)) +                                
  labs(x = "Intersectionality Rating", y = "Queer Characters' Psychological Impact Score") +
  labs(title = "Intersectionality Rating's Impacts on Queer Characters' Psychological Effects by Development Status") + 
  scale_y_continuous(breaks = seq(floor(min(queer.hypo$psychological_wellbeing_score)), 
                                  ceiling(max(queer.hypo$psychological_wellbeing_score)), by = 1)) +
  scale_x_continuous(
    breaks = seq(floor(min(queer.hypo$intersectionality_rating)), 
                 ceiling(max(queer.hypo$intersectionality_rating)), by = 1),
    limits = c(floor(min(queer.hypo$intersectionality_rating)), ceiling(max(queer.hypo$intersectionality_rating)))
  ) +
  stat_smooth(method = "lm", aes(fill = development_status, colour = development_status)) +    
  scale_colour_manual(name = "Development Status", values = c("#FFC125", "#36648B")) +
  scale_fill_manual(name = "Development Status",values = c("#FFC125", "#36648B")) +
  scale_shape_discrete(name = "Development Status") +
  theme.clean() 

print(inter.psych.dev)

# The visualization indicated that developed countries feature more data points with low intersectionality rating and low psychological impact scores.

# Visualization 2: Intersectionality rating's impacts on queer characters' psychological impacts by gender identity
inter.psych.gender <- ggplot(queer.hypo, aes(x = intersectionality_rating, y = psychological_wellbeing_score)) +
  geom_point(aes(colour = gender_identity_dual, shape = gender_identity_dual)) +                                
  labs(x = "Intersectionality Rating", y = "Queer Characters' Psychological Impact Score") +
  labs(title = "Intersectionality Rating's Impacts on Queer Characters' Psychological Effects by Gender Identity") + 
  scale_y_continuous(breaks = seq(floor(min(queer.hypo$psychological_wellbeing_score)), 
                                  ceiling(max(queer.hypo$psychological_wellbeing_score)), by = 1)) +
  scale_x_continuous(
    breaks = seq(floor(min(queer.hypo$intersectionality_rating)), 
                 ceiling(max(queer.hypo$intersectionality_rating)), by = 1),
    limits = c(floor(min(queer.hypo$intersectionality_rating)), ceiling(max(queer.hypo$intersectionality_rating)))
  ) +
  stat_smooth(method = "lm", aes(fill = gender_identity_dual, colour = gender_identity_dual)) +    
  scale_colour_manual(name = "Gender Identity", values = c("#6fa8dc", "#d5a6bd")) +
  scale_fill_manual(name = "Gender Identity", values = c("#6fa8dc", "#d5a6bd")) +
  scale_shape_discrete(name = "Gender Identity") +
  theme.clean() 

print(inter.psych.gender)


# Visualization 3: Intersectionality rating's impacts on queer characters' psychological impacts by sexual orientation
inter.psych.orientation <- ggplot(queer.hypo_filtered, aes(x = intersectionality_rating, y = psychological_wellbeing_score)) +
  geom_point(aes(colour = sexual_orientation, shape = sexual_orientation)) +                                
  labs(x = "Intersectionality Rating", y = "Queer Characters' Psychological Impact Score") +
  labs(title = "Intersectionality Rating's Impacts on Queer Characters' Psychological Effects by Sexual Orientation") + 
  scale_y_continuous(breaks = seq(floor(min(queer.hypo$psychological_wellbeing_score)), 
                                  ceiling(max(queer.hypo$psychological_wellbeing_score)), by = 1)) +
  scale_x_continuous(
    breaks = seq(floor(min(queer.hypo$intersectionality_rating)), 
                 ceiling(max(queer.hypo$intersectionality_rating)), by = 1),
    limits = c(floor(min(queer.hypo$intersectionality_rating)), ceiling(max(queer.hypo$intersectionality_rating)))
  ) +
  stat_smooth(method = "lm", aes(fill = sexual_orientation, colour = sexual_orientation)) +    
  scale_colour_manual(name = "Sexual Orientation", values = c("#A3A3A3","#9B4F96","#3D1A78","#FFD800")) +
  scale_fill_manual(name = "Sexual Orientation", values = c("#A3A3A3","#9B4F96","#3D1A78","#FFD800")) +
  scale_shape_discrete(name = "Sexual Orientation") +
  facet_wrap(~sexual_orientation) +
  theme.clean() 

print(inter.psych.orientation)

# I want to see intersectional identity's impacts: Combine gender identity and sexual orientation

inter.psych.orientation.gender <- ggplot(queer.hypo_filtered, aes(x = intersectionality_rating, y = psychological_wellbeing_score)) +
  geom_point(aes(colour = sexual_orientation, shape = sexual_orientation)) +                                
  labs(x = "Intersectionality Rating", y = "Queer Characters' Psychological Impact Score") +
  labs(title = "Intersectionality Rating's Impacts on Psychological Effects by Gender Identity and Sexual Orientation") + 
  scale_y_continuous(breaks = seq(floor(min(queer.hypo$psychological_wellbeing_score)), 
                                  ceiling(max(queer.hypo$psychological_wellbeing_score)), by = 1)) +
  scale_x_continuous(
    breaks = seq(floor(min(queer.hypo$intersectionality_rating)), 
                 ceiling(max(queer.hypo$intersectionality_rating)), by = 1),
    limits = c(floor(min(queer.hypo$intersectionality_rating)), ceiling(max(queer.hypo$intersectionality_rating)))
  ) +
  stat_smooth(method = "lm", aes(fill = sexual_orientation, colour = sexual_orientation)) +    
  scale_colour_manual(name = "Sexual Orientation", values = c("#A3A3A3","#9B4F96","#3D1A78","#FFD800")) +
  scale_fill_manual(name = "Sexual Orientation", values = c("#A3A3A3","#9B4F96","#3D1A78","#FFD800")) +
  scale_shape_discrete(name = "Sexual Orientation") +
  facet_grid(gender_identity_dual ~ sexual_orientation) +
  theme.clean() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 8),
        legend.position = "bottom")

print(inter.psych.orientation.gender)

# Bisexual men are more likely to have extreme values for intersectionality rating and psychological impact scores on the lower side, bisexual women feature higher intersectionality and psychological impact scores
# Gay have more variances
# Asexual and pansexual women are more likely to draw the positive relationship between intersectionality and psychological impacts, while asexual and pansexual men are more likely to have the negative relationship


# Visualization 4: Intersectionality rating's impacts on queer characters' psychological impacts by racial identity
inter.psych.race <- ggplot(queer.hypo, aes(x = intersectionality_rating, y = psychological_wellbeing_score)) +
  geom_point(aes(colour = race_category, shape = race_category)) +                                
  labs(x = "Intersectionality Rating", y = "Queer Characters' Psychological Impact Score") +
  labs(title = "Intersectionality Rating's Impacts on Queer Characters' Psychological Effects by Racial Identity") + 
  scale_y_continuous(breaks = seq(floor(min(queer.hypo$psychological_wellbeing_score)), 
                                  ceiling(max(queer.hypo$psychological_wellbeing_score)), by = 1)) +
  scale_x_continuous(
    breaks = seq(floor(min(queer.hypo$intersectionality_rating)), 
                 ceiling(max(queer.hypo$intersectionality_rating)), by = 1),
    limits = c(floor(min(queer.hypo$intersectionality_rating)), ceiling(max(queer.hypo$intersectionality_rating)))
  ) +
  stat_smooth(method = "lm", aes(fill = race_category, colour = race_category)) +    
  scale_colour_manual(name = "Racial Identity", values = c("#cfb5a0","#f1e2c8")) +
  scale_fill_manual(name = "Racial Identity", values = c("#cfb5a0","#f1e2c8")) +
  scale_shape_discrete(name = "Racial Identity") +
  theme.clean() 

print(inter.psych.race)

