# TweetLingDiv

### Description and code
This is the repository holding the code used to create the dataset TweetLingDiv, published at [Zenodo](https://zenodo.org/records/19332457).
The datset is a derivative of the UNT tweet archive (Feng et al. 2023), which we accessed using their API. We requested the number of tweets for each combination of country, year and language to yielding a distribution of languages across both geography and time. The data with tweets per language and year are indexed per country and stored in `data/country_data/` the scripts for generating them are in `dataset_creation/extract_data`. First run `setup_countries_and_langs.py`, then `extraction.py`.

The country individual datasets are then combined from the script `dataset_creation/compile_country_data.R` and country and language data is added from the linguistic database Glottolog and rnaturalearth.  

Diversity measures according to the Leinster-Cobbold framework are calculated with the scripts in `dataset_creation/diversity_measures/`. First load the linguistic databases Glottolog and ASJP and process them using the scripts in `dataset_creation/databaseprocessing`.

### Access the data
The dataset can be found in `data/final_dataset` or at Zenodo: https://zenodo.org/records/19332457


### Explore the data
We have developed an Shiny Dashboard to allow easy exploration of data. Run app.R locally.

### Citation
If you use the data, please cite:
Essfors, H. (2026). TweetLingDiv [Dataset]. Zenodo. https://doi.org/10.5281/zenodo.19332457

### Dataset creation

To recreate the dataset, files are run in the following order:

- dataset_creation/extract_data/setup_countries_and_langs.py: creates two .csv of the available countries and languages
- dataset_creation/extract_data/extraction.py: Loops through all country, languages and years and requests the number of tweet-IDs from the UNT. 

- dataset_creation/database_processing/fetch_asjp.R: fetches the ASJP from Zenodo
- dataset_creation/database_processing/asjp_processing.R: processes the ASJP-data to harmonize it with the twitter data
- dataset_creation/database_processing/fetch_glottolog.R: fetches the Glottolog from Zenodo
- dataset_creation/database_processing/process_glottolog.R: processes the Glottolog-data to harmonize it with the twitter data


- dataset_creation/compile_tweet_data/compile_country_data.R: Gathers the number of tweets per country, year and language, strucutres them as a dataframe and stores as a .csv.
- dataset_creation/compile_tweet_data/add_country_meta_data.R: Accesses country data from Natural Earth and process it to match the data set.
- dataset_creation/compile_tweet_data/add_language_meta_data.R: Accesses language data from Glottolog and matches it with the twitter data.

- dataset_creation/diversity_measures/generate_similarities.R: Generates a lexical similarity matrix for all languages in the Twitter data based on the 40-item ASJP word lists.
- dataset_creation/diversity_measures/generate_diversity_measures.R: Calculates naive and non-naive (lexical) diversity according to the Leinster-cobbold framework.