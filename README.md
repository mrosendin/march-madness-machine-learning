# March Madness Machine Learning

Final project for _INDENG 242: Applications in Data Analysis_ that predicts the final NCAA March Madness Tournament bracket. We use a logistic regression model to predict the probability that each team wins a given match-up, for all possible match-ups. Then we feed these probabilities into a simulation to predict the most likely bracket.

## Project Overview

```
.
├── data
│   ├── RegularSeasonDetailedResults.csv
│   ├── RegularSeasonDetailedResults2.csv
│   ├── Teams.csv
│   ├── TourneyCompactResults.csv
│   ├── TourneyCompactResults2.csv
│   └── TourneyCompactResults2017.csv
├── projectv1.R             # Data processing
├── sampleSimulation.xlsx   # Simulation in Excel
├── targetVariables.R       # Predictive Models
└── teamRatings.R           # Pull team ratings from external source
```

The CSVs from the data folder are from [Kaggle](https://www.kaggle.com/c/march-machine-learning-mania-2017) with our own modifications.

## Authors

- Farshad Miraftab (@farshadl123)
- Matthew Rosendin (@rosendin)
- Nabeel Saleem (@darealnabeel)
