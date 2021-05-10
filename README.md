# MATH4100 Indutrial Problems in Applied Math

## Problem Statement:

IntelyCare uses data and technology to help long-term care facilities find the extra help they need on a per-shift basis from our pool of credentialed nursing professionals. We give nursing professionals full flexibility to work as much or as little as they choose, and we observe substantial variance in usage across our pool of nurses. Our goal is to identify cohorts of nurses with similar in-app behaviors. Who are nurses that accept whatever shift we show them? Who are the nurses who only work when they find the exact shift they like? Which nurses can work more shifts in a week and which nurses cannot? Who are the weekend warriors? Who are the holiday heroes? . 

These behavioral cohorts will allow us to personalize the nurse experience in important ways. We can send more timely and relevant communications, improve our in-app shift recommendations, and offer more effective promotions if we understand what behavior patterns characterize a given nursing professional. 

For this project, we used several unsupervised learning techniques and supervised learning techniques to achieve the problems we metioned. 

## Methods
We are using RStudio. 

## Results
Our model was capable of giving accurate predictions for nurses shifts based on our machine learning process.

## Discussion
Based on our results, it turns out most of nurses tend to work at weekdays, and most of them would like to plan their schedule ahead around 3 to 4 days. 

## Summary
After some data cleaning we've kept the most important feature that correlates with the nurses, 
which were the Years of Prior Work History, Prior Distinct Jobs, Total Accepted Shifts, Total Viewed Shifts, Total Clicked Shifts, Day difference between date of first shift and date of starting app, date.diff, Day difference between fifth and first shift, Average day difference between shift date and accepted date, Difference between first and last app open, Clicks per view.
Then we moved on the unspervised leanring techniques to reduce the dimensions by using PCA and clustered the data set by K-mean clustering method. We generate two clusters for the data set. After that, we developed several supervised learning techniques based on the clustered data. The techniques is shown as below. And for the results, you can find all figures and screenshots in the attached reports.

### Logistic Regression
Logistic Regression is a useful model to run early in the workflow. Logistic regression measures the relationship between the categorical dependent variable (feature) and one or more independent variables (features) by estimating probabilities using a logistic function, which is the cumulative logistic distribution. Reference Wikipedia.

### Random Forest Regression
The next model Random Forests is one of the most popular. Random forests or random decision forests are an ensemble learning method for classification, regression and other tasks, that operate by constructing a multitude of decision trees (n_estimators=100) at training time and outputting the class that is the mode of the classes (classification) or mean prediction (regression) of the individual trees. Reference Wikipedia.

### Support Vector Machine (SVM)
Next we model using Support Vector Machines which are supervised learning models with associated learning algorithms that analyze data used for classification and regression analysis. Given a set of training samples, each marked as belonging to one or the other of two categories, an SVM training algorithm builds a model that assigns new test samples to one category or the other, making it a non-probabilistic binary linear classifier. Reference Wikipedia.
