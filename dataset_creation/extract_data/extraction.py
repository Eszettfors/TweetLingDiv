import utils as utils
import pandas as pd
import numpy as np
import time
import json
from concurrent.futures import ThreadPoolExecutor, as_completed
import concurrent.futures
import os
# this script loops through all countries, languages and years and extracts the number of tweets, storing it in a 
# pandas dataframe. The pandas dataframe is then exported as as csv

data_path = "/Users/hannes/Projekt/Dig_ling_div_UNT/data/processed"
countries = pd.read_csv("/Users/hannes/Projekt/Dig_ling_div_UNT/data/processed/country_codes.csv")
languages = pd.read_csv("/Users/hannes/Projekt/Dig_ling_div_UNT/data/processed/language_codes.csv")
country_data_path = "/Users/hannes/Projekt/Dig_ling_div_UNT/data/country_data"


# fix namibia
countries = countries.fillna("NA")


# define countries and languages to loop over
country_list = list(countries.country_code)
language_list = list(languages.language_code)

# define years to loop over 2012 - 2021
years = [str(year) for year in range(2012, 2022)]

""" nesting structure: 
{country: 
    {year : 
        {language : n}
    
}}
"""

# general test case
#test_country = ["SE", "AT"]
#test_langs = ["sv", "de", "en"]
#test_years = ["2018", "2019"]

def get_n_tweets_sequental(countries = country_list, languages = language_list, years = years):
    """
    Takes a list of countries, languages and years and makes api calls for all combinations in sequenece.
    """

    t1 = time.time()
    
    full_dict = dict()
    i = 0
    for country in countries:
        full_dict[country] = {}
        for year in years:
            full_dict[country][year] = {}
            for lang in languages:
                n = utils.get_n_tweets_tweet_loc(country, lang, year)
                full_dict[country][year][lang] = n
                i += 1
                print(str((i / (len(countries) * len(languages) * len(years))) * 100) + "% complete")

    
    t2 = time.time()
    ttot = t2-t1
    print("done, it took " + str(ttot) + " seconds")
    return(full_dict)

#get_n_tweets_sequental(test_country, test_langs, test_years)
#get_n_tweets_sequental(["SE"], ["sv"], ["2018", "2019"])

#utils.get_n_tweets_tweet_loc("SE", "sv", "2018")

def get_n_tweets_parallel(countries, languages, years):
    """
    Attempt at making parallel requests to the UNT api - throttles at more than 4 requests
    """
    # define start time
    t1 = time.time()

    # create combinations of country, language and year -> will be run in parallel
    combinations = [(country, lang, year) for country in countries for lang in languages for year in years]
    
    # create a full dictionary to store results
    full_dict = {country: {year: {lang: 0 for lang in languages} for year in years} for country in countries}

    def fetch_and_store(country, lang, year):
        # fetch the number of tweets for a specific country, language and year and store it in the full_dict
        try:
            n = utils.get_n_tweets_tweet_loc(country, lang, year)
        except Exception as e:
            print(f"Error fetching {country}-{lang}-{year}: {e}")
            n = None
        full_dict[country][year][lang] = n
        return (country, lang, year, n)

    with ThreadPoolExecutor(max_workers=5) as executor:
        # submit all combinations to the executor
        futures = [executor.submit(fetch_and_store, *combo) for combo in combinations]
        for f in as_completed(futures):
            try:
                country, lang, year, n = f.result()
                print(f"Completed {country}-{lang}-{year}: {n}")
            except Exception as e:
                print(f"Error in task: {e}")

    t2 = time.time()
    print(f"it took {t2 - t1:.2f} seconds")
    return full_dict, t2 - t1

def export_one_country(country, languages = language_list, years = years):
    """
    Exports the number of tweets for one country, all languages and years to a csv file.
    """
    
    country_dict = get_n_tweets_sequental([country], languages, years)
    
    rows = []
    for year, langs in country_dict[country].items():
        for lang, n in langs.items():
            rows.append({
                "country": country,
                "year": year,
                "language": lang,
                "n_tweets": n
            })

    # Create DataFrame
    df = pd.DataFrame(rows)

    # export
    df.to_csv(f"{country_data_path}/{country}_n_tweets.csv", index=False)


def get_countries_missing():
    """
    retrieves a list of countries for which data has not been extracted from the API
    """
    
    # lists the country files that have been created
    file_names = os.listdir(country_data_path)
    
    # retrieves the country code from created files
    extracted_countries = [file_name[0:2] for file_name in file_names]
    
    # removes the already processed country codes from the list of countries
    non_extracted_countries = set(country_list) - set(extracted_countries)
    
    return list(non_extracted_countries)
    

def get_country_progress():
    """
    Calculates how many percent of the countries have been completed
    """
    
    # get list of countries not extracted yet
    missing_countries_n = len(get_countries_missing())
    
    # get list of total countries to extract
    total_to_extract_n = len(country_list)
    
    extracted_countries_n = total_to_extract_n - missing_countries_n
    
    # calculate percent left to extract
    perc_left = (missing_countries_n / total_to_extract_n) * 100
    
    # return one mins perc left = perc completed
    progress = 100 - perc_left
    print(f'{extracted_countries_n} countries have been extracted of {total_to_extract_n}. \nThere are {missing_countries_n} countries left. \n{progress}% of all countries have been extracted')


#get_n_tweets_sequental(test_country, test_langs, test_years)
#test_country = ["SE"]
#test_langs = ["sv", "de", "en"]
#test_years = ["2018", "2019", "2020"]

#get_n_tweets_parallel(test_country, test_langs, test_years)
#full_dict = get_n_tweets_sequental(test_country, test_langs, test_years)

# flatten and export
"""
#rows = []
for country, years in full_dict.items():
    for year, langs in years.items():
        for lang, n in langs.items():
            rows.append({
                "country": country,
                "year": year,
                "language": lang,
                "n_tweets": n
            })

# Create DataFrame
df = pd.DataFrame(rows)

# export
df.to_csv("/Users/hannes/Projekt/Dig_ling_div_UNT/data/processed/test_data.csv", index = False)
"""


# run export one country for every country that has not been processed



for country in get_countries_missing():
    get_country_progress()
    if len(get_countries_missing()) == 0:
        print("all countries extracted\n Ending script")
        exit()
    export_one_country(country)

