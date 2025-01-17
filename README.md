# March Madness Machine Learning

Final project for _INDENG 242: Applications in Data Analysis_ that predicts the final NCAA March Madness Tournament bracket. We use a logistic regression model to predict the probability that each team wins a given match-up, for all possible match-ups. Then we feed these probabilities into a simulation to predict the most likely bracket.

## Project Overview

```
.
├── data
│   ├── FinalStats.csv
│   ├── NabeelStats.csv
│   ├── RegularSeasonDetailedResults.csv
│   ├── SampleSubmission.csv
│   ├── TeamRatings.csv
│   ├── Teams.csv
│   ├── TourneyCompactResults.csv
│   └── TourneySeeds.csv
├── descriptiveAnalysis.Rmd # Descriptive analysis R Markdown file
├── descriptiveAnalysis.pdf # PDF generated by descriptiveAnalysis.Rmd
├── project.R               # Data processing
├── sampleSimulation.xlsx   # Simulation in Excel
├── targetVariables.R       # Predictive Models
└── teamRatings.R           # Pull team ratings from external source
```

The CSVs from the data folder are from [Kaggle](https://www.kaggle.com/c/march-machine-learning-mania-2017) with our own modifications.

**A note on using R Markdown for descriptive analysis**

For recent versions of RStudio, the Knit PDF button does not seem to work. Instead, enter the following command in the Console to generate a new PDF:

```
render('descriptiveAnalysis.Rmd')
```

## Authors

- Farshad Miraftab (@farshadl123)
- Matthew Rosendin (@rosendin)
- Nabeel Saleem (@darealnabeel)
