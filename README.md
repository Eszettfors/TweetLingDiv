# TweetLingDiv

### Description and code
This is the repository holding the code used to create the dataset TweetLingDiv, published at [Zenodo](https://zenodo.org/records/19332457).
The datset is a derivative of the UNT tweet archive, which we accessed using their API. We requested the number of tweets for each combination of country, year and language to yielding a distribution of languages across both geography and time. The data with tweets per language and year are indexed per country and stored in `data/country_data/` the scripts for generating them are in `dataset_creation/extract_data`. First run `setup_countries_and_langs.py`, then `extraction.py`.

The country individual datasets are then combined from the script `dataset_creation/compile_country_data.R` and country and language data is added from the linguistic database Glottolog and rnaturalearth.  

Diversity measures according to the Leinster-cobbold framework are calculated with the scripts in `dataset_creation/diversity_measures/. First load the linguistic databases Glottolog and ASJP and process them using the scripts in `dataset_creation/databaseprocessing`.

### Access the data
The dataset can be found in `data/final_dataset` or at Zenodo: https://zenodo.org/records/19332457


### Explore the data
We have developed an Shiny Dashboard to allow easy exploration of data. Run app.R locally.

### Citation
If you use the data, please cite:
Essfors, H. (2026). TweetLingDiv [Dataset]. Zenodo. https://doi.org/10.5281/zenodo.19332457
