# vcvn
R package for Variable  Selection, Curve Fitting, Variable Conversion, Normalisation and Accuracy Measures

This will include following:

## Variable Selection:
1) Information Value
2) Gini Index
3) Gini Impurity
4) Entropy Gain
5) Misclassification Error

## To be Implemented under Variable Selection Methods:
1) Variable Ranking Methods - voting / scoring / weighted scoring / weighted voting
2) Generic Scoring Function (for Regressiona and Classification)
3) Variable Inflation Factor
4) Other Variable Impacts for regression

## Curve Fitting(To be implemented):
1) Template for Curve Fitting for Contineous and Categorical Variable
2) Curve Comparision Methods
3) Curve Indentification
4) Curve Tuning
5) Curve to Normal Conversion
6) Non - Curve / Random / Many Matching curve Decision Criterion
7) Goodness of Fit Test:
  a) Kolomogorov- Simronov Test
  b) Carmer-Von Mises Test
  c) Anderson-Darling Test
  d) Shapiro -Wilk Test
  e) Chi-Squared Test
  f) Akaike Information Criterion (AIC)
  g) Hosmer - Lemeshow Test

## Variable Conversion(To be implemented):
1) Continuous to Categorical
  a) Range Binning
  b) WOE Criterion Binning
  c) Dependent Binning

2) Categrical to Contineous
  a) One - Hot Encoding with and without reference
  b) Label Encoding
  c) Weightage Encoding
  d) Boosted Encoding ( Based on CatBoost Methodology by Yandex)

## Normalisation (To be Implemented):
1) Unit Mean
2) Unit SD
3) Unit Mean And SD
4) Min - Max
5) Box-Cox
6) Log
7) Exponential
8) Mean Difference
9) Median Difference
10) Mean Difference wiht SD
11) Median Difference with SD

**Will also try to include predict function for applying variable conversion and normalisation on raw data.

## Measures (These are extensions for other calculations):
1) RMSE
2) MAE
3) MAPE
4) R-squared
5) AIC
6) BIC
7) AUC

## To be implemented Measures:
1) Kendall's Tau
2) Gini Index
3) Weights
4) Extension to caret's ConfusionMatrix

**Will also try to include methods for finding best and/or biased limit for probablity cut-off of calssification problem

## Updated as on 28th August, 2017