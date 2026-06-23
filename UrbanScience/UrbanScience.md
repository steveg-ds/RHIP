---
aliases:
tags:
  - linker-exclude
  - subject/housing
  - subject/spatial
  - subject/rural
  - subject/sociology
---
# Urban Science Article



```table-of-contents
style: nestedList # TOC style (nestedList|nestedOrderedList|inlineFirstLevel)
minLevel: 2 # Include headings from the specified level
maxLevel: 0 # Include headings up to the specified level
exclude:/Table of Contents/i
includeLinks: true # Make headings clickable
hideWhenEmpty: false # Hide TOC if no headings are found
```


## Basics

- More urban focus than thesis 
- Use socioeconomic data
- "Put your eggs in the 4 Cs basket and talk about the idea that the literature is fundamentally biased toward understanding the tenor of the urban issue (because that's where all the work is); it's important to see whether or not there is a spectral nature to the problem and how the different ends of the RUCA spectrum react to similar classification approaches.  How that problem presents itself across that spectrum becomes important for development on both ends." - Matt
- RUCA Codes

## Brainstorm

https://search.r-project.org/CRAN/refmans/usdm/html/lisa.html
https://link.springer.com/article/10.1007/s11749-018-0599-x
https://github.com/mszell/geospatialdatascience

[[RUCA Codes]]


## Data

- RUCA Codes 
- 2023 Census data
	- since there will be ~ 80,000 census tracts I think it would be better to have fewer variables with a deeper investigation 
	- I should boil it down to the core 4 Cs: 
		- pct of households spending more than 30% of income on housing
		- pct of households with incomplete plumbing/kitchen (stand in for housing quality)
		- some kind of statistic that reflects demographic diversity
		- pct of population under poverty line
		- pct of population w/o high school diploma
	- Maybe correlation and [[Principal Component Analysis]] can be used to  make sure chosen variables are independent.

## Methods

- **Variance Inflation Factor (VIF)**: Multicollinearity diagnostic to ensure variable independence.
- **Multinomial Logistic Regression (MLR)**: Modeling the likelihood of a tract falling into RUCA categories 2-10 compared to reference Metropolitan Core (RUCA 1).
- **Decision Trees**: Non-linear classification to predict rurality and capture variable importance.
- **K-Means Clustering**: Unsupervised grouping of tracts on socioeconomic dimensions to identify socio-spatial archetypes.
- **Spatial Autocorrelation**: Future step using LISA (Local Indicators of Spatial Association) to detect geographical clustering of archetypes.

## Data & Variables

- **Scope**: 82,673 continental US census tracts (2020 ACS 5-Year estimates + 2020 USDA RUCA).
- **Socioeconomic Indicators**:
  - `PovertyRate`: Ratio of households below poverty line (`B17001`).
  - `CostBurden`: Renters and owners spending $\ge 30\%$ of income on housing (`B25070` + `B25091`).
  - `HousingQuality`: Substandard housing lacking plumbing or kitchen facilities (`B25047` + `B25051`).
  - `LessThanHS`: Population 25+ without a high school diploma (`B15003`).
  - `PercentMinority`: Non-white share of the population (`B03002`).
  - `Gini`: Income inequality index (`B19083`).
  - `NormalizedEntropy`: Shannon Entropy index of employment shares across 13 NAICS industries, scaled 0 to 1 (`C24030`). Used to quantify economic/industrial diversity.

## Results & Findings

### 1. Multinomial Logistic Regression (MLR)
- **Model Fit & Performance**:
  - The model converged successfully over 81,802 tracts ($p < 0.001$).
  - McFadden’s Pseudo $R^2$ of **0.2071** confirms that neighborhood socioeconomic characteristics explain a substantial and statistically significant share of the variation in rural-urban spatial classifications.
  - All outcomes ($RUCA=2$ to $RUCA=10$) are compared against the reference category, **RUCA 1 (Metropolitan Core)**.
- **Housing Cost Burden (Strong negative relationship with rurality)**:
  - Cost burden significantly drops as rurality increases.
  - Outlying commuting zones are less likely to experience high housing cost burdens compared to metropolitan cores, peaking in small town commuting zones ($\beta_{RUCA9} = -15.03$).
- **Housing Quality (Strong positive relationship with rurality)**:
  - Substandard housing conditions (lacking complete plumbing/kitchen facilities) are positively associated with rural classification.
  - Deficiencies are most pronounced in isolated rural areas ($\beta_{RUCA10} = 10.87$).
- **Education & Human Capital (Strong positive relationship with rurality)**:
  - Lower educational levels (percentage of residents with less than a high school education) strongly predict rural status.
  - The deficit is highest in small town commuting tracts ($\beta_{RUCA9} = 14.15$).
- **Demographics (Strong negative relationship with rurality)**:
  - Racial and ethnic minority representation decreases consistently as rurality increases.
  - Isolated rural tracts have the lowest proportion of minority residents compared to metropolitan cores ($\beta_{RUCA10} = -7.34$).
- **Income Inequality / Gini Coefficient (Positive relationship with rurality)**:
  - Internal income inequality is significantly higher in rural tracts.
  - Inequality effects are most extreme in small town fringe areas ($\beta_{RUCA8} = 6.53$) and isolated rural areas ($\beta_{RUCA10} = 6.05$).

### 2. Socio-Spatial Clustering (K-Means, k=3)
Clustering reveals a clear bifurcation of geographic disadvantage:
- **Cluster 0: Stable/Affluent (65% of tracts)**: Low poverty (8.8%), low cost burden (25.0%). Dominates suburban commuting zones (RUCA 2: 87.5%, RUCA 3: 82.9%).
- **Cluster 1: Extreme Deprivation (11% of tracts)**: High poverty (33.9%), high Gini inequality (0.493), low economic diversity (0.76). Peaks in micropolitan cores (RUCA 4: 20.1%) and small town cores (RUCA 7: 20.5%).
- **Cluster 2: Urban Affordability Stress (24% of tracts)**: Moderate poverty (18.0%), extreme housing cost burden (41.4%). Concentrated heavily in metropolitan cores (RUCA 1: 30.2%).

*Key Insight*: Inequality presents differently across the urban-rural spectrum. Metro cores face affordability stress, while rural/small-town cores suffer from structural deprivation and low industrial diversity.

## Next Steps
- Execute Local Spatial Autocorrelation (LISA) analysis on cluster assignments.
- Commit final plots and draft manuscript for *Urban Science*.


