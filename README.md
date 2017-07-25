# income_predict

Analysis using R on the [Adult Dataset](http://archive.ics.uci.edu/ml/datasets/Adult) in UCI machine learning repository, extracted from the census bureau database of the US GOV. The dataset deals with a binary classification problem, that is to determine whether a person makes over 50K a year.

As a course project of Machine Learning of [Universitat Politècnica de Catalunya](http://www.upc.edu/), which is explained in detailed in [project report](https://github.com/yanghanxy/Income_predict/blob/master/Report/ML_Project_Report.pdf). 

## Run the code

1. adult.data is the raw data from UCI machine learning repository, 
   if you use this dataset, you have to do every step in the R code
   to finish the data pre-procecssing. 
2. adult.data.csv is the cooked data, 
   if you use this dataset, you can skip all the data-preprocessing part, 
   read it as the X.adult.complete (which is line 6 and 7 with comment),
   and then direct skip to line 189 for the visiualization

The reason for having the cooked dataset is that 
in imputing the missing values, I use MICE(), it takes quite a long time to finish! 

