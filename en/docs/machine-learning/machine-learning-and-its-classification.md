---
title: "Machine learning and it's classification"
slug: "machine-learning-and-its-classification"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## What is machine learning ?
Two definitions of Machine Learning are offered. *Arthur Samuel* described it as:

> the field of study that gives computers the ability to learn without
> being explicitly programmed.

This is an older, informal definition.

*Tom Mitchell* provides a more modern definition: 

> A computer program is said to learn from experience E with respect to
> some class of tasks T and performance measure P, if its performance at
> tasks in T, as measured by P, improves with experience E.

Example: playing checkers.

E = the experience of playing many games of checkers

T = the task of playing checkers.

P = the probability that the program will win the next game.

In general, any machine learning problem can be assigned to one of two broad classifications:

 1. Supervised learning
 2. Unsupervised learning.


## What is supervised learning ?
Supervised learning is a type of machine learning algorithm that uses a known data-set (called the training data-set) to make predictions.

Category of supervised learning:

 1. **Regression:** In a regression problem, we are trying to predict results within a continuous output, meaning that we are trying to map input variables to some continuous function.
 2. **Classification:** In a classification problem, we are instead trying to predict results in a discrete output. In other words, we are trying to map input variables into discrete categories.

**Example 1:**

Given data about the size of houses on the real estate market, try to predict their price. Price as a function of size is a continuous output, so this is a regression problem.


**Example 2:**

(a) *Regression* -  For continuous-response values. For example given a picture of a person, we have to predict their age on the basis of the given picture

(b) *Classification* - for categorical response values, where the data can be separated into specific “classes”. For example given a patient with a tumor, we have to predict whether the tumor is malignant or benign.

## What is unsupervised learning ?
Unsupervised learning allows us to approach problems with little or no idea what our results should look like. We can derive structure from data where we don't necessarily know the effect of the variables.


**Example:**

*Clustering:* Is used for exploratory data analysis to find hidden patterns or grouping in data. Take a collection of 1,000,000 different genes, and find a way to automatically group these genes into groups that are somehow similar or related by different variables, such as lifespan, location, roles, and so on.


