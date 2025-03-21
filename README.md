# Exploring the Impact of Queer Media Characters on LGBTQ+ Psychological Well-Being: The Role of Queer Media Consumption, Perceived Authenticity, and Intersectionality


## :rainbow: Repository Purpose
This repository contains the manuscript, data, images, and code for the final research project titled "Exploring the Impact of Queer Media Characters on LGBTQ+ Psychological Well-Being: The Role of Queer Media Consumption, Perceived Authenticity, and Intersectionality" in the course From Data to Manuscript in R. The study examined queer media characters’ impacts on LGBTQ+ individuals’ psychological well-being through two overarching research questions. The first research question investigated whether there were differences in queer characters’ psychological impacts among LGBTQ+ individuals with different identities. The second research question asked whether queer media consumption patterns, perceived character authenticity, and perceived character intersectionality contribute to different LGBTQ+ identities’ impacts on queer media characters’ psychological impacts. 


### :card_file_box:Repository Contents
1. **Manuscript**:
   - [Manuscript.qmd](./Queer%20Characters%20and%20LGBTQ%2B%20Well-being.qmd): Quarto document containing the full research paper.
   - [Manuscript.pdf](./Queer-Characters-and-LGBTQ+-Well-being.pdf): PDF document containing the full research paper.

3. **Source Code**:
   - [Data Cleaning.R](./Source/Data%20Cleaning.R): Script for cleaning and preprocessing raw data.
   - [Data Transformation.R](./Source/Data%20Transformation.R): Script for transforming cleaned data for analysis.
   - [Data Analysis.R](./Source/Data%20Analysis.R): Script containing statistical analyses, exploratory analysis, hypothesis testing, and data visualization. 

4. **Datasets**:
   - [Data](./Data/): Directory containing two original, unprocessed survey datasets, one cleaned dataset, and two transformed datasets for data analysis. 
  
5. **Visualizations**:
   - [Image](./Image): Directory containing all 25 data visualizations and figures used in either the manuscript or data visualization R script.

6. **Documentation**:
   - [README.md](./README.md): Overview of the project, repository structure, and instructions for replication.
   - [References.bib](./References.bib): Bibliography file containing all cited works.
   - [APA Quarto extensions](./_extensions/wjschne/apaquarto): APA Quarto extension files needed for producing the manuscript

7. **Version Control**:
   - [.gitignore](./.gitignore): Specifies intentionally untracked files to ignore.
   - [R Project](./queer%20characters.Rproj): R Project synchronized with GitHub version control. 
     

This repository aims to ensure transparency, reproducibility, and accessibility of our research. It provides a comprehensive collection of all materials used in the study, from raw data to the final manuscript, allowing for thorough peer review and potential replication of our findings.

For questions or further information, please contact [Dingning Yang] at :e-mail:(mailto:dingningy@uchicago.edu).


## :test_tube:Methodological Information
### :bust_in_silhouette:Participants
This study recruited 180 LGBTQ+ participants worldwide through the data collection platform Prolific. Specifically, the respondents were aged from 18 to 72 in 17 countries (including countries in North America, Europe, Asia, and Africa) with different sexual orientations, gender identities, and racial identities. Each participant was paid $2 to participate in a survey with a seven-minute average finish time. 

### :memo:Measurements
This study developed a survey instrument to explore queer media characters’ psychological impacts among different LGBTQ+ individuals with five sections: 

**Demographic form:**
The demographic form includes nine questions that ask participants’ age, residential country (and state if living in the US), gender identity, sexual orientation, race, and education. 

**Queer media consumption:**
The queer media consumption scale asks five questions regarding participants’ exposure and engagement with queer media characters. Specifically, the scale measures the frequency of watching queer TV shows or movies, the specific queer media genre and names (as a mix of close-ended and open-ended questions), the discussion engagement of queer representations on social media, and the importance of queer media consumption. 

**Queer character authenticity:**
The queer character authenticity scale consists of seven questions, with four of them asking the extent to which queer characters can reflect individual experiences and daily encounters. Another three questions measure the extent of queer characters displaying stereotypes, which are reverse-coded. The sum of seven questions consists of the authenticity rating of the whole scale. 

**Queer character intersectionality:**
The queer character intersectionality scale consists of four questions that ask the extent to which queer characters can reflect diverse experiences of multiple factors such as racial and socioeconomic backgrounds. 

**Queer character’s psychological impacts:**
This scale measures queer characters’ impacts on participants’ psychological well-being, which is constituted of three sub-scales: Self-development (seven questions), community belonging (six questions), and emotional well-being (eight questions). 

### :chart_with_upwards_trend: Data Analysis
Data cleaning, transformation, analysis, and visualization were performed by RStudio.


## :bar_chart:Data Information

### Dataset Names

**Raw dataset:**
raw.queer.label.csv:[View the Dataset](./Data/raw.queer.label.csv);
raw.queer.value.csv:[View the Dataset](./Data/raw.queer.value.csv)

**Cleaned dataset:**
queer.cleaned.csv:[View the Dataset](./Data/queer.cleaned.csv)

**Transformed dataset:**
queer.hypo.csv:[View the Dataset](./Data/queer.hypo.csv);
media.long.csv:[View the Dataset](./Data/media.long.csv)


### Data Source
The data were collected from the Qualtrics online survey on the Prolific platform in February 2025.


### Size
Approximately 313 KB.


### Licensing
This dataset is for internal use only and should not be shared without permission.
