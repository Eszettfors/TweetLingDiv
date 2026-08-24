import re
import pandas as pd

# this file sets up two lists with countries and languages available in UNT which where extracted from the webpages HTML

# define path and file names
path = "/data/"
country_file = "raw/countries.txt"
language_file = "raw/languages.txt"


# open the country and langauge files
with open(path + country_file, "r", encoding = "utf-8") as f:
    countries = f.read()
with open(path + language_file, "r", encoding = "utf-8") as f:
    languages = f.read()
    
    
# extract the two or three letter country or language code, the ? makes the wildcard optional
pattern = r"'\w\w?\w"

# find pattern and strip "'"
country_list = re.findall(pattern, countries) 
country_list = [country.strip("'") for country in country_list]

language_list = re.findall(pattern, languages)
language_list = [language.strip("'") for language in language_list]

# extract unique elements
countries = set(country_list)
languages = set(language_list)


# turn the sets into dataframes and export as csv
country_df = pd.DataFrame(countries, columns=["country_code"])
language_df = pd.DataFrame(languages, columns = ["language_code"])

country_df.to_csv(path + "/processed/country_codes.csv", index = False)
language_df.to_csv(path + "/processed/language_codes.csv", index = False)
