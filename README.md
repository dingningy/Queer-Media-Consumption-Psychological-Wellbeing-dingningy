# Exploring the Impact of Queer Media Characters on LGBTQ+ Psychological Well-Being: The Role of Queer Media Consumption, Perceived Authenticity, and Intersectionality


## Repository Purpose
This repository contains the manuscript, data, images, and code for the final research project titled "Exploring the Impact of Queer Media Characters on LGBTQ+ Psychological Well-Being: The Role of Queer Media Consumption, Perceived Authenticity, and Intersectionality" in the course From Data to Manuscript in R. The study examined queer media characters’ impacts on LGBTQ+ individuals’ psychological well-being through two overarching research questions. The first research question investigated whether there were differences in queer characters’ psychological impacts among LGBTQ+ individuals with different identities. The second research question asked whether queer media consumption patterns, perceived character authenticity, and perceived character intersectionality contribute to different LGBTQ+ identities’ impacts on queer media characters’ psychological impacts. 


### Repository Contents
1. **Manuscript**: 
   - `Manuscript.qmd`: Quarto document containing the full research paper.

2. **Source Code**:
   - `Data Cleaning.R`: Script for cleaning and preprocessing raw data.
   - `Data Transformation.R`: Script for transforming cleaned data for analysis.
   - `Data Analysis.R`: Script containing statistical analyses, exploratory analysis, hypothesis testing, and data visualization. 

3. **Datasets**:
   - `/Data`: Directory containing two original, unprocessed survey datasets, one cleaned dataset, and two transformed datasets for data analysis. 
  
4. **Visualizations**:
   - `/images`: Directory containing all 25 data visualizations and figures used in the manuscript.

5. **Documentation**:
   - `README.md`: Overview of the project, repository structure, and instructions for replication.
   - `References.bib`: Bibliography file containing all cited works.

6. **Version Control**:
   - `.gitignore`: Specifies intentionally untracked files to ignore.

This repository aims to ensure transparency, reproducibility, and accessibility of our research. It provides a comprehensive collection of all materials used in the study, from raw data to the final manuscript, allowing for thorough peer review and potential replication of our findings.

For questions or further information, please contact [Dingning Yang] at (mailto:dingningy@uchicago.edu).


## Methodological Information
### Participants
This study recruited 180 LGBTQ+ participants worldwide through the data collection platform Prolific. Specifically, the respondents were aged from 18 to 72 in 17 countries (including countries in North America, Europe, Asia, and Africa) with different sexual orientations, gender identities, and racial identities. Each participant was paid $2 to participate in a survey with a seven-minute average finish time. 

### Measurements
This study developed a survey instrument to explore queer media characters’ psychological impacts among different LGBTQ+ individuals with five sections: 

**Demographic form**
The demographic form includes nine questions that ask participants’ age, residential country (and state if living in the US), gender identity, sexual orientation, race, and education. 

**Queer media consumption**
The queer media consumption scale asks five questions regarding participants’ exposure and engagement with queer media characters. Specifically, the scale measures the frequency of watching queer TV shows or movies, the specific queer media genre and names (as a mix of close-ended and open-ended questions), the discussion engagement of queer representations on social media, and the importance of queer media consumption. 

**Queer character authenticity**
The queer character authenticity scale consists of seven questions, with four of them asking the extent to which queer characters can reflect individual experiences and daily encounters. Another three questions measure the extent of queer characters displaying stereotypes, which are reverse-coded. The sum of seven questions consists of the authenticity rating of the whole scale. 

**Queer character intersectionality**
The queer character intersectionality scale consists of four questions that ask the extent to which queer characters can reflect diverse experiences of multiple factors such as racial and socioeconomic backgrounds. 

**Queer character’s psychological impacts**
This scale measures queer characters’ impacts on participants’ psychological well-being, which is constituted of three sub-scales: Self-development (seven questions), community belonging (six questions), and emotional well-being (eight questions). 

### Data Analysis
Data cleaning, transformation, analysis, and visualization were performed by RStudio.


## Data Information

### Dataset Names

**Raw dataset**
raw.queer.label.csv 
raw.queer.value.csv

**Cleaned dataset**
queer.cleaned.csv

**Transformed dataset**
queer.hypo.csv
media.long.csv


### Source
The datasets are collected from the Qualtrics online survey on the Prolific platform in February 2025.


### Size
Approximately 313 KB.


### Access Instructions
The dataset can be found in the `Data/` folder of the repository.

### Licensing
This dataset is for internal use only and should not be shared without permission.
