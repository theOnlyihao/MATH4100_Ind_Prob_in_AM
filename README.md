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

### K-mean Clustering and PCA

In the research and application of many fields, it is necessary to observe the data containing multiple variables, collect a large amount of data and analyze it to find the law. Multivariate large data sets will undoubtedly provide rich information for research and application, but it also increases the workload of data collection to a certain extent. More importantly, in many situations, there may be correlations between many variables, which increases the complexity of problem analysis. If you analyze each indicator separately, the analysis is often isolated and cannot fully utilize the information in the data. Therefore, blindly reducing the indicators will lose a lot of useful information, resulting in wrong conclusions. 

Therefore, it is necessary to find a reasonable method to minimize the loss of information contained in the original indicators while reducing the indicators that need to be analyzed, to achieve the purpose of comprehensive analysis of the collected data. Since there is a certain correlation between the variables, it can be considered to change the closely related variables into as few new variables as possible, so that these new variables are pairwise uncorrelated, then fewer comprehensive indicators can be used to represent them, respectively. Various types of information that exist in various variables. Principal component analysis and factor analysis belong to this type of dimensionality reduction algorithm. PCA is an exploratory tool to simplify a large and complex data set into a smaller, more easily understandable data set. It summarizes the complex data set by creating variables that are linear combinations of the original data set. 

It does not necessarily describe or explain the relationship among variables. After constructing principal component analysis, each of new variables, called principal components, is uncorrelated with all others. It is effectively used as a method of pattern analysis bringing strong patterns to the forefront. In summary, PCA looks to find a low-dimensional representation of the observation that explain a good fraction of the variance.

 K-means is  one of  the simplest unsupervised  learning  algorithms  that  solve the well  known clustering problem. The procedure follows a simple and  easy  way to classify a given data set  through a certain number of  clusters (assume k clusters) fixed apriori. The  main  idea  is to define k centers, one for each cluster. These centers  should  be placed in a cunning  way  because of  different  location  causes different  result. So, the better  choice  is  to place them  as  much as possible  far away from each other. The  next  step is to take each point belonging  to a  given data set and associate it to the nearest center. When no point  is  pending,  the first step is completed and an early group age  is done. At this point we need to re-calculate k new centroids as barycenter of  theclusters resulting from the previous step. After we have these k new centroids, a new binding has to be done  between  the same data set points  and  the nearest new center. A loop has been generated. As a result of  this loop we  may  notice that the k centers change their location step by step until no more changes  are done or  in  other words centers do not move any more. Finally, this  algorithm  aims at  minimizing  an objective function know as squared error function given by: 
    $$J(V)=\sum_{i=1}^{c}\sum_{j=1}^{c_{i}}\left(\left\|x_{i}-v_{j}\right\|\right)^{2}$$
    The algorithmic steps for k-mean clustering is shown with following: Let                        $X = (x1,x2,x3,……..,x_n)$ be the set of data points and V = ($v_1$,$v_2$,…….,$v_c$) be the set of centers.1) Randomly select ‘c’ cluster centers. 2) Calculate the distance between each data point and cluster centers. 3) Assign the data point to the cluster center whose distance from the cluster center is minimum of all the cluster centers. 4) Recalculate the new cluster center using: where, ‘ci’ represents the number of data points in $i_th$ cluster. $$\boldsymbol{v}_{i}=\left(1 / c_{i}\right) \sum_{j=1}^{\mathcal{C}_{i}} \boldsymbol{x}_{i}$$ 5) Recalculate the distance between each data point and new obtained cluster centers. 6) If no data point was reassigned then stop, otherwise repeat from step 3).

### Logistic Regression
Logistic Regression is a useful model to run early in the workflow. Logistic regression measures the relationship between the categorical dependent variable (feature) and one or more independent variables (features) by estimating probabilities using a logistic function, which is the cumulative logistic distribution. Reference Wikipedia.

### Random Forest Regression
The next model Random Forests is one of the most popular. Random forests or random decision forests are an ensemble learning method for classification, regression and other tasks, that operate by constructing a multitude of decision trees (n_estimators=100) at training time and outputting the class that is the mode of the classes (classification) or mean prediction (regression) of the individual trees. Reference Wikipedia.

### Support Vector Machine (SVM)
Next we model using Support Vector Machines which are supervised learning models with associated learning algorithms that analyze data used for classification and regression analysis. Given a set of training samples, each marked as belonging to one or the other of two categories, an SVM training algorithm builds a model that assigns new test samples to one category or the other, making it a non-probabilistic binary linear classifier. Reference Wikipedia.
