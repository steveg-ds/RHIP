---
aliases: []
tags: []
---

# RUCA Codes

| Code | Classification description |
|----|----|
| 1 | Metropolitan core: primary flow is within an urban area of 50,000 or more people (metro UA) |
| 2 | Metropolitan high commuting: primary flow is 30 percent or more to a metro UA |
| 3 | Metropolitan low commuting: primary flow is 10 percent to 30 percent to a metro UA |
| 4 | Micropolitan core: primary flow is within an urban area of 10,000 to 49,999 people (micro UA) |
| 5 | Micropolitan high commuting: primary flow is 30 percent or more to a micro UA |
| 6 | Micropolitan low commuting: primary flow is 10 percent to 30 percent to a micro UA |
| 7 | Small town core: primary flow is within an urban area of 9,999 or fewer people (small town UA) |
| 8 | Small town high commuting: primary flow is 30 percent or more to a small town UA |
| 9 | Small town low commuting: primary flow is 10 percent to 30 percent to a small town UA |
| 10 | Rural area: primary flow is to a tract outside an UA |
| 99 | Not coded: census tract is entirely in coastal or inland water bodies, with zero population and zero land area |

See the [RUCA Codes](https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes/documentation) documentation for detailed info on data and methods used for creating RUCA Codes.

    Total # of census tracts: 85528
    # of continental census tracts: 83776

|  | TractFIPS | TractName | CountyFIPS | CountyName | StateFIPS | StateName | RUCA |
|---:|---:|:---|---:|:---|---:|:---|---:|
| 0 | 01001020100 | Census Tract 201 | 01001 | Autauga County | 01 | Alabama | 1 |
| 1 | 01001020200 | Census Tract 202 | 01001 | Autauga County | 01 | Alabama | 1 |
| 2 | 01001020300 | Census Tract 203 | 01001 | Autauga County | 01 | Alabama | 1 |
| 3 | 01001020400 | Census Tract 204 | 01001 | Autauga County | 01 | Alabama | 1 |
| 4 | 01001020501 | Census Tract 205.01 | 01001 | Autauga County | 01 | Alabama | 1 |

``` python
STATES = list(ruca["StateFIPS"].unique())  # List of states to pass to pytidycensus
YEAR: int = 2024
```

``` python
ruca_counts = {
    "Count": ruca["RUCA"].value_counts().sort_index(),
    "Proportion": round(
        ruca["RUCA"].value_counts(normalize=True).sort_index() * 100, 2
    ),
}
pd.DataFrame(ruca_counts)
```

| RUCA | Count | Proportion |
|-----:|------:|-----------:|
|    1 | 57544 |      68.69 |
|    2 |  8481 |      10.12 |
|    3 |  1149 |       1.37 |
|    4 |  4630 |       5.53 |
|    5 |  2224 |       2.65 |
|    6 |   693 |       0.83 |
|    7 |  1475 |       1.76 |
|    8 |   607 |       0.72 |
|    9 |   336 |        0.4 |
|   10 |  6343 |       7.57 |
|   99 |   294 |       0.35 |

# ACS Data

## Cost Burden

Collects variables for housing cost burden (renters and owners spending 30% or more of household income on housing) to measure overall cost burden.

## Poverty Rate

Calculates poverty rate based on the number of people with income below the poverty level in 12 months and divides it by the population.

## Housing Quality

Collects variables for substandard plumbing and kitchen facilities to measure overall housing quality.

## Education

Calculates percentage of the population that does not hold a high school diploma

## Demographics

Calculates the percentage of the population that is white alone. lower number → more diversity.

## Household Vars

Variables regarding household characterstics rather than individual characteristics. Currently just the Gini Index.

# Data Analysis

Merge all individual ACS socioeconomic indicators and combines them with the cleaned RUCA dataset by Census Tract GEOID.

Tract count from merging data: 82879
Tract count after removing infinite values and null values: 81,802 (-1,077)

|  | TractFIPS | TractName | CountyFIPS | CountyName | StateFIPS | StateName | RUCA | GEOID | PovertyRate | CostBurden | HousingQuality | LessThanHS | PercentMinority | Gini |
|---:|---:|:---|---:|:---|---:|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| 0 | 01001020100 | Census Tract 201 | 01001 | Autauga County | 01 | Alabama | 1 | 01001020100 | 0.17406 | 0.168478 | 0.00791557 | 0.0941974 | 0.334643 | 0.4238 |
| 1 | 01001020200 | Census Tract 202 | 01001 | Autauga County | 01 | Alabama | 1 | 01001020200 | 0.0950506 | 0.163851 | 0.100287 | 0.0974729 | 0.676971 | 0.41 |
| 2 | 01001020300 | Census Tract 203 | 01001 | Autauga County | 01 | Alabama | 1 | 01001020300 | 0.0713866 | 0.235628 | 0.0921248 | 0.130983 | 0.35854 | 0.3519 |
| 3 | 01001020400 | Census Tract 204 | 01001 | Autauga County | 01 | Alabama | 1 | 01001020400 | 0.153495 | 0.142857 | 0.00841852 | 0.0535777 | 0.119048 | 0.588 |
| 4 | 01001020501 | Census Tract 205.01 | 01001 | Autauga County | 01 | Alabama | 1 | 01001020501 | 0.0351035 | 0.189303 | 0 | 0.0317658 | 0.215122 | 0.3954 |

## Descriptive Statistics

|  | RUCA | PovertyRate | CostBurden | HousingQuality | LessThanHS | PercentMinority | Gini |
|:---|---:|---:|---:|---:|---:|---:|---:|
| count | 81802 | 81802 | 81802 | 81802 | 81802 | 81802 | 81802 |
| mean | 2.31409 | 0.135813 | 0.312121 | 0.0442922 | 0.109586 | 0.416713 | 0.423781 |
| std | 2.62832 | 0.10856 | 0.125279 | 0.0696827 | 0.0969945 | 0.291475 | 0.0674244 |
| min | 1 | 0 | 0 | 0 | 0 | 0 | 0.0001 |
| 25% | 1 | 0.0586836 | 0.217634 | 0 | 0.0414791 | 0.165269 | 0.3783 |
| 50% | 1 | 0.106323 | 0.294195 | 0.0203091 | 0.0811245 | 0.346462 | 0.4179 |
| 75% | 2 | 0.180921 | 0.391065 | 0.0584669 | 0.147231 | 0.640236 | 0.463 |
| max | 10 | 1 | 1 | 1.30769 | 0.800313 | 1 | 0.826 |


![](Data%20Exploration_files/figure-commonmark/cell-20-output-1.png)

Inner lines show 25%, 50% (median), and 75% interquartile ranges. Wider parts -> more tracts within value range, skinny parts → fewer tracts within value range x-axis = variable as a percentage of the population

## Pearson Correlation Matrix

![](Data%20Exploration_files/figure-commonmark/cell-21-output-1.png)

- The minority percentage and cost burden percentage of a census tract decreases as census tracts become more urban whereas poor housing quality go's up.
- LessThanHS has decent postive correlations (0.3 - 0.5) with PovertyRate and PercentMinority

## Variable Inflation Factor

``` python
# Keep only numeric columns (excluding the target RUCA variable)
vif_features = [
    "PovertyRate",
    "CostBurden",
    "HousingQuality",
    "LessThanHS",
    "PercentMinority",
    "Gini",
]

df_vif = df[vif_features].astype(float)

vif_data = pd.DataFrame()
vif_data["feature"] = df_vif.columns

vif_data["VIF"] = [
    variance_inflation_factor(df_vif.values, i) for i in range(len(df_vif.columns))
]
vif_data
```

|     | feature         |     VIF |
|----:|:----------------|--------:|
|   0 | PovertyRate     | 4.34349 |
|   1 | CostBurden      | 12.3108 |
|   2 | HousingQuality  | 1.68077 |
|   3 | LessThanHS      | 3.83457 |
|   4 | PercentMinority | 5.66061 |
|   5 | Gini            | 8.52679 |

- [VIF Interpretation](https://statisticsbyjim.com/regression/multicollinearity-in-regression-analysis/)
  - VIF ≈ 1: No correlation with other predictors
  - 1 \< VIF ≤ 5: Mild to moderate correlation (usually fine)
  - VIF \> 10: Strong multicollinearity -\> take corrective steps

CostBurden and Percent Minority are over 5 but less than 6, so I don't think they'll cause significant multicolinearity issues.

At 8.5 the Gini coefficient is close to being problematic. Since it's the main way the ACS measures income inequality, so unless it causes problems down the road I guess it's fine ¯\\_(ツ)_/¯

## Multinomial Logistic Regression

 Current function value: 0.931436
 Iterations: 375
 Function evaluations: 377
 Gradient evaluations: 377

Pretty much all the coefficients are statistically significant but considering the large number of observations and the very low amount of variation between values, I don't think it really means much beyond being able to say they're statistically significant.

![](Data%20Exploration_files/figure-commonmark/cell-24-output-1.png)

Positive multinomial log-odds coefficient: variable increase -\> likelihood of class (RUCA code) increases.

``` python
# Predict probabilities of classification on the training data
probs = result.predict(X)

# Extract correct classification probability for each tract
df["RucaProb"] = probs.values[np.arange(len(y)), y - 1]

# Display summary statistics of the correct classification probabilities
print(
    "Mean probability of correct classification:", round(df["RucaProb"].mean() * 100, 2)
)
print("Median probability of correct classification:", np.median(df["RucaProb"]))
```

    Mean probability of correct classification: 59.81
    Median probability of correct classification: 0.7298630414056722



![](Data%20Exploration_files/figure-commonmark/cell-26-output-1.png)

It's certainly one of the distributions ever made.

