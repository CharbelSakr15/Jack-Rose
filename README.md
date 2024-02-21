# Jack-Rose

This repository hosts an university assignment meant to familiarize students with the version control tool Git.

The assignment consists of processing and analyzing the famous Titanic dataset using R. There are 3 major blocks to the assignment manifesting in 2 scripts:

• "preprocess_data.R" - cleans the data, conducts feature encoding and engineering
• "analyze_data.R" - analyzes the data, visualizes results
and 2 modules consisting of functions used by the scripts:
- "Funktionen-R-Skript 1.R" - analysis and visualization functions
- "Funktionen-R-Skript 2.R" - various miscellaneous functions 

Although not required, an additional "main.R" was created to pipeline the 
entire procedure from retrieving the data, processing and analyzing it, and
visualizing the results for convenience.

### Dependencies
The scripts require the following R packages to be installed:
- ggplot2
- ggalluvial
- stringr