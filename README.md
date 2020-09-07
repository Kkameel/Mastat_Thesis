# Mastat_Thesis
Private Repo, covering all of the code for reproducing this master thesis.

Please note that not all data-engineering happened in either R or Python (Jupyter Notebooks).
Certain data engineering steps happened manually and were executed in Excel, in order to proceed efficiently whilst manually checking for anomalies.

An overview of all documents is found below in a <b>chronological order</b>, with a concise explanation on their content and use.

•	Scraping_final.ipynb<br>
This code entails most of the data collection and feature engineering process, from scraping up until the creation of most final features.<br>
•	Regex_dictionary.txt<br>
The regular expressions used to create the linguistic cues, created as explained in the method section of this paper.<br>
•	Additional Manipulation.ipynb<br>
In this annex, additional manipulations were done to the dataset after the whole collection process was done.<br>
•	JCPGoogleAndCrossRef.csv<br>
	An overview of all citations for both Google Scholar and CrossRef.<br>
•	analysis.xlsx<br>
This dataset is the dataset, as created by combining the two notebooks. Further manual manipulation of certain variables still happened afterwards. (e.g. creation of the p-value distribution).<br>
•	meta_data_analysis.csv<br>
After completing all data manipulation, this dataset was used as the input for the modelling process.<br>
•	indirect_effects_final.R<br>
The final (and cleaned) script which takes "meta_data_analysis.csv" as an input. This script account partially for the EDA, as well as for all of the modelling.<br>
•	final_data_scaled.csv<br>
The final dataset which was used for the modelling itself, a scaled (and filtered) variant of the raw dataset where outliers were removed.<br>
