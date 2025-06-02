# README

For part of the exploratory analysis, you need the onesampb function by
Rand R. Wilcox. You can download it from here (the file "Rallfun-v44.txt"):

https://osf.io/xhe8u/

Place the file in the same directory as this README file.


The recommended way to rerun the analysis is to use RStudio. This allows you to
open the Data Analysis.Rproj file, which sets some environment variables so the
R scripts find the correct data files.

The code for the analysis is in the "analysis" folder:
- 01_checks_and_cleaning.R contains code to get from the original raw data file
  to the cleaned data file used in the analysis. It also contains the code that
  produces the additional data files that a) reassign participants using their
  responses to the comprehension check and b) exclude participants with
  incorrect answers to the comprehension check.
- 02_analysis.R contains the code for the actual analysis, including code to
  create the plots. You can use the same file to rerun the analysis with the
  alternative data (reassigned or excluded), just by uncommenting one line.
  Detailed instructions are in the file.

The "data" folder contains the following files:
- "raw_data_anonymized.csv", the anonymized export from limesurvey
- "clean_data.csv", the data used for the analysis as reported in the paper
- "only_who_passed_the_comprehension_check.csv", the same as "clean_data.csv",
  but excluding the cases with failed comprehension check
- "participants_reassigned_to_comprehension_check_instead_of_groups.csv", the
  same as "clean_data.csv", but reassigning participants to the groups that
  match their responses to the comprehension check

The "output" folder contains the following files:
- "ALA.png", plot of the Audience-and-Location Axes analysis
- "social_acceptability.png", the plot of the hypothesis testing
