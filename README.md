# vcvn
R package for Variable  Selection, Curve Fitting, Variable Conversion, Normalisation and Accuracy Measures

This will include following:

## Variable Selection:
1) Information Value
2) Gini Index
3) Gini Impurity
4) Entropy Gain
5) Misclassification Error
6) Variable Ranking Methods - voting / scoring / weighted scoring / weighted voting

## Curve Fitting:
1) Template for Curve Fitting for Contineous and Categorical Variable
2) Curve Comparision Methods
3) Curve Indentification
4) Curve Tuning
5) Curve to Normal Conversion
6) Non - Curve / Random / Many Matching curve Decision Criterion

## Variable Conversion:
1) Continuous to Categorical
  a) Range Binning
  b) WOE Criterion Binning
  c) Dependent Binning

2) Categrical to Contineous
  a) One - Hot Encoding with and without reference
  b) Label Encoding
  c) Weightage Encoding
  d) Boosted Encoding ( Based on CatBoost Methodology by Yandex)

## Normalisation:
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

## Measures:
1) RMSE
2) MAE
3) MAPE
4) R-squared
5) AIC
6) BIC
7) ROC
8) AUC
9) GINI Index
10) Kendall's Tau
11) Extension to caret's ConfusionMatrix

**Will also try to include methods for finding best and/or biased limit for probablity cut-off of calssification problem

