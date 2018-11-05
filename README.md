# Measuring Prominence in Parliament

This repository contains code and instructions for reproducing the analysis and results contained in the following paper:

**Fraussen, B., Graham, T., & Halpin, D. (2018). Assessing the prominence of interest groups in parliament: a supervised machine learning approach. The Journal of Legislative Studies (FJLS). DOI: 10.1080/13572334.2018.1540117**

## Purpose of this repository

This repository contains all the R code for this paper, providing future researchers the ability to replicate the study and reproduce the data and findings.

## Paper abstract

Ascertaining which interest groups are considered relevant by policymakers presents an important challenge for political scientists. Existing approaches often focus on the submission of written evidence or the inclusion in expert committees. While these approaches capture the effort of groups, they do not directly indicate whether policy makers consider these groups as highly relevant political actors. In this paper we introduce a novel theoretical approach to address this important question, namely prominence. We argue that, in the legislative arena, prominence can be operationalised as groups being mentioned strategically – used as a resource – by elected officials as they debate policy matters. Furthermore, we apply a machine learning solution to reliably assess which groups are prominent among legislators. We illustrate this novel method relying on a dataset of mentions of over 1300 national interest groups in parliamentary debates in Australia over a six-year period (2010–2016).

## Instructions for use

The code for this paper is contained in a single file in this repository, namely: `FINAL_COMPLETE_METHODOLOGY_FOR_PROMINENCE_PAPER.R`.

The R script has four main parts:

1) Scraping data from Hansard (Australian parliamentary records);
2) Cleaning the data and curating it;
3) Supervised machine learning to classify the prominence labels;
4) Generating analysis and results

There is also some associated data files that are necessary to run the code. These are included in the repository and are primarily the manually labelled data (which occurred in two phases of data collection so there are two separate files).







