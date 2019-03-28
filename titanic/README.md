# titanic-exercise
A data wrangling exercise in dealing with missing values using the [titanic dataset](https://www.kaggle.com/c/titanic/data) from Kaggle.

## Port of embarkation
Missing values in the *embarked* column are known to correspond to passengers who actually embarked at Southampton, and are replaced with "S."

## Age
There were many missing values in the *Age* column missing. They have been replaced by the mean of the *Age* column, but perhaps should be replaced by the median, as the distribution of ages is skewed.

## Lifeboat
Many passengers did not make it onto a lifeboat, thus many values in the *boat* column are missing. They have been replaced with NA.

## Cabin
Many passengers did not have a cabin number associated with them, which can indicate they are ship crew or stowaways. This can be a useful indicator of survival, so a new feature, *has_cabin_number*, was created.
