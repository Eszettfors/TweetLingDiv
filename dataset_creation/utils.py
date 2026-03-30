import requests
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry

# configure retries for requests session
session = requests.Session()
retries = Retry(
    total=5,                  # retry up to 5 times
    backoff_factor=2,          # wait 2s, 4s, 8s, ...
    status_forcelist=[500, 502, 503, 504],
    allowed_methods=["GET", "POST"]
)
adapter = HTTPAdapter(max_retries=retries)
session.mount("http://", adapter)
session.mount("https://", adapter)

# base urls:
url_tweet_loc = "https://sigspatial.yunhefeng.me/api/query_tweet_location.php"
url_user_loc = "https://sigspatial.yunhefeng.me/api/query_user_location.php"
url_coord_loc = "https://sigspatial.yunhefeng.me/api/query_coordinates.php"


def get_n_tweets_tweet_loc(country : str, language : str, year : str):
    """
    This function takes a country, language and year, access the UNT API and returns the number of tweets
    for that period based on tweet location
    """
    # create a parameter dictionary according to specification
    tweet_loc_param = {
        "country" : country,
        "language" : language,
        "from_date" : year + "-01-01",
        "to_date" : year + "-12-31"
    }
    
    # sends a requests
    #response = requests.get(url_tweet_loc, tweet_loc_param)
    response = session.get(url_tweet_loc, params=tweet_loc_param)
    print("request sent for " + country + ", " + language + ", " + year)
    
    # checks if a request was accepted. If yes -> extract data
    if response.status_code == 200:
        ids = response.json()
        print("sucessfull api call")
    else:
        # returns and error if status code is not 200 -> no response
        print("error:" + response.status_code)
        
    # return number of unique ids
    return(len(set(ids)))



def get_n_tweets_user_loc(country : str, language : str, year : str):
    """
    This function takes a country, language and end and year, access the UNT API and returns the number of tweets
    for that period based on user location
    """
    # create a parameter dictionary according to specification
    tweet_loc_param = {
        "country" : country,
        "language" : language,
        "from_date" : year + "-01-01",
        "to_date" : year + "-12-31"
    }
    
    # sends a requests
    response = requests.get(url_user_loc, tweet_loc_param)
    
    # checks if a request was accepted. If yes -> extract data
    if response.status_code == 200:
        ids = response.json()
    else:
        # returns and error if status code is not 200 -> no response
        print("error:" + str(response.status_code))
        
    # return number of unique ids
    return(len(set(ids)))