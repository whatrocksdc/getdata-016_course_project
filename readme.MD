---
title: "readme"
author: "whatrocksdc"
date: "Sunday, December 21, 2014"
output: html_document
---

This repository stores a script 'run_analysis.R' that performs some data tidying processes on a larger [UCI Machine Learning Dataset](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones) that keeps a codebook describing output data and variables in 'codebook.md'

This script performs the following tasks:

* Loads necessary libraries
* Imports data into a series of data tables
* Assigns labels and formatting to large sets of observations
* Sorts and groups observations first by each of the 30 subjects in the test, then by the 6 activities they perform
* Finds the average of every group of observations that record a mean (mean()) or standard deviation (std()) for each subject-activity pair
* Writes data out to tidy_output.txt, a data table with 180 observations, 66 averaged observable variables, and two identifier variables (180 rows, 68 columns) along with descriptive column headers

To tidy your data:

* Set your working directory
* Input the "UCI HAR Dataset" folder in your working directory in its entirety
* Run the 'run_analysis.R' script
* Visualize the tab-delimited output 'tidy_output.txt' in the viewer of your choice