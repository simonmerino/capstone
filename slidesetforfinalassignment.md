Data Science Specialization Final Assignment
========================================================
author: Simon Merino
date: June 2018
autosize: true

Problem Description
========================================================

The Coursera Data Science Specialization has the following final assignment: 

<center>
*Creating a text prediction application using R language*
</center>


The *input* information provided to build such prediction model is a collection of tweets, news and blogs texts.

The expected *deliverable* is an application hosted in shinyapps that got as an input a text string and generated the most probable next word.


Model Description
========================================================
The text prediction model is based on the N-grams approach to text treament, being the codede flow:

- A 1-gram corpus is created for each sampled (20%) dataset, removing stopwords to reduce the universe of words to those with proper meaning.
- From the 1-gram corpus, the 2-grams, 3-grams and 4-grams are built
- The N-grams corpus are tabulated
- The twitter, news and blogs frequency N-gram tables are joined together to form a single dataset.
- Finally, removing of the least frequent N-grams is done to reduce the size of the model and get lower response times.


Application Description
========================================================

The shiny application is made of 3 building blocks:

* The Global Part:
This code is run at application launch time and is in charge of loading all needed libraries and model information as well as supporting functions to use the loaded model

* The Server Part:
It parses the input text string and feeds it into the model. Returns 2 pieces of information:
    + The most probable next word for the input string
    + A wordcloud made of the most probable next words

* The UI Part
It is the frotend of the application. Has an input form in the left part as well as "Run" button and the output part in the right side

Application Look & Feel
========================================================
Find it here!!: https://simonmerino.shinyapps.io/Text_predictor/

<center>

![](app_snapshot.PNG)
</center>

