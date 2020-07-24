The "Simulated Data" file is a comma separated value (CSV) file, which R can read in easily, either using the `read.csv()` function or by clicking "Import dataset" as we did before in class.

This dataset is simulated experimental data for a reading time, rating, and comprehension experiment, which means there are several types of responses. Each column is described below.

subj = unique number for each subject or participant
age = the age of the participant
item = the item number that elicited the given response
freq = the categorical frequency of the manipulated region (high or low)
gram = whether or not the item is grammatical (yes or no)
rating = the subject's response to rating the acceptability of the item (1-7)
accuracy = the subject's response to a comprehension question about the item (1=correct, 0=incorrect)
region = which word number the reading time was measured for
word = an example word that might have appeared in the region (simplified across items)
rt = how long the subject spent reading the word in the given region

There are two crossed factors, each with two levels:
gram = yes / no
freq = high / low

This means there are four conditions:
1. High frequency grammatical
2. Low frequency grammatical
3. High frequency ungrammatical
4. Low frequency ungrammatical

This experiment doesn't really ask a question, because it's not a real experiment. Instead, it sets up a paradigm that will allow you to compare across conditions using three different types of data:

* continuous data (rt)
* binomial data (accuracy)
* ordinal rating data (rating)

Since each item contains five regions, but rating and accuracy data are per item rather than per region, you will have to take a subset of the total dataset in order to properly graph and analyse rating or accuracy data. If you don't you'll be graphing or analysing five copies of this data at once.