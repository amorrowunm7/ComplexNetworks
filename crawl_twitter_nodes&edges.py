#import graphlab as gl
import pickle
import twitter
import logging
import time
from collections import defaultdict


### Setup a console and file logger

logger = logging.getLogger('crawler')
logger.setLevel(logging.DEBUG)
fh = logging.FileHandler('crawler.log')
fh.setLevel(logging.INFO)
ch = logging.StreamHandler()
ch.setLevel(logging.INFO)
formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
ch.setFormatter(formatter)
fh.setFormatter(formatter)
logger.addHandler(ch)
logger.addHandler(fh)

### Setup signals to make sure API calls only take 60s at most

from functools import wraps
import errno
import os
import signal

class TimeoutError(Exception):
    pass

def timeout(seconds=60, error_message=os.strerror(errno.ETIME)):
    def decorator(func):
        def _handle_timeout(signum, frame):
            raise TimeoutError(error_message)

        def wrapper(*args, **kwargs):
            signal.signal(signal.SIGALRM, _handle_timeout)
            signal.alarm(seconds)
            try:
                result = func(*args, **kwargs)
            finally:
                signal.alarm(0)
            return result

        return wraps(func)(wrapper)

    return decorator

@timeout()
def getFollowers(api, follower):
    ''' Function that will get a user's list of followers from an api object. 
    NOTE: the decorator ensures that this only runs for 60s at most. '''
    # return api.GetFollowerIDs(follower)
    return api.GetFriendIDs(follower)


### Twitter API

# Lets create our list of api OAuth parameters
API_TOKENS = [
    {"consumer_key": '9K7SrSf7fbffaVmBNaO7Ace5r',
    "consumer_secret": 'JGIYddchEh6suhseGUwXxpFyp5UOgwQ8jaIA88rHSJfVJCoI7A',
    "access_token_key": '733478864530219008-2VQPGYxxNrUXhRrlO3IcHZJULWXYHIa',
    "access_token_secret": 'tGW7suoOOBfmX9DIDBwJmtvPK5xLLFQTtFEu2nPJsbswj',
    "requests_timeout": 60},

    {"consumer_key": '1aiFwamENcX14O2rjcn73A1Yo',
    "consumer_secret": 'CVpgEGU6xwLuhjuq3Kg09j0fJYSw6KKiEnryd1IKgLfRo8vFQK',
    "access_token_key": '733478864530219008-qMPa2akjwxBLdRjcUlii2MR9zFjlLfs',
    "access_token_secret": 'CEnWZY7Xut0jDQln1Hv4VCEvG0JomKA4Ooff6Cq2W6sI9',
    "requests_timeout": 60},

    {"consumer_key": 'E4azqaStWPQvfbtHNg8b1TkHO',
    "consumer_secret": 'zFxAMJFVRmYIeezhxCIFAnor9bxohFYouRdTbkp6V4gyBxiDvp',
    "access_token_key": '733478864530219008-B41BFz0qFz3YT2bTMeqf2NSrVEBgJFG',
    "access_token_secret": 'FZye3U4MfYrU1iLn5FyqgdQJlWmzmiWH1ncb17oz01Swi',
    "requests_timeout": 60},

    {"consumer_key": 'avZpjObqQN9vue2Y4gu9zIF9X',
    "consumer_secret": 'Ka6WCj3fyon5yGgf5YJIIl8nVcLcUh5YT99N58qy8qv4kfaMbc',
    "access_token_key": '16562593-VNuGD09Cr29ZlzNCWnV5MOujU7PsexSwfTgfKQNqC',
    "access_token_secret": '9P3hB3qDb9zPDFCUhWU16N4CMXPwHacl6HJbCc0EuGj7s',
    "requests_timeout": 60},

    {"consumer_key": 'sQ9H5NKteroNZSWvIrkSWvXR0',
    "consumer_secret": 'lC0ttZKdIZhhJAE1I5RxMxdjpSiADQCVUnHS7LbtfVmI2pz2F2',
    "access_token_key": '16562593-4LOk7QkXWD0boF01BmZ6NP2oPtHmDZ1OVJ883aANG',
    "access_token_secret": 'JJ85qMqzVowN1KdQ6w4YlhJB9YF9eWbw6SGbxQoU6gvne',
    "requests_timeout": 60},

    {"consumer_key": 'DHppZ2LG3iYj8vEx7ibRRLN35',
    "consumer_secret": 'wdTQeyp7ZNDN7ne40IriRw7Ah1J8cAi2OIlw4MVtgpq5MMKjYE',
    "access_token_key": '16562593-WN8zvEWAxVfJPrneMwUjDoVQw0geuLckOOJqFimsC',
    "access_token_secret": 'ZgVi2onPB3RPGtRmPBs6QXymIMgXwJHUOQycesp64S0Hp',
    "requests_timeout": 60},

    {"consumer_key": 'lIgtfdkC2WmN7XAcicrGygQBp',
    "consumer_secret": '2D9WIJN2MIPwFpMeIGcP6vWjQC8vvy7G5ZlHMSH1F1CsgWGKfz',
    "access_token_key": '16562593-7lhPpeZNNAGoQQJnqcnTtBiGq1O52XMZ4CMeVqXiY',
    "access_token_secret": 'WKRBQsr36MMB2EpCcZLr89ik0MSJfPoBORCKu9E1hw96I',
    "requests_timeout": 60},

    {"consumer_key": '1XFu2urZzoMoC5sadXAjA7IoQ',
    "consumer_secret": 'FrJOlHfNLp3M7ejJWiO5k74E9ai6L5EzQJ45HmlsUINbh8qUUi',
    "access_token_key": '16562593-Texko6g7VyCwhNUfxBDoJKJl4058hpvQkqAYWRKpi',
    "access_token_secret": 'ISZCTvN6bYJVaJ3Z2iidQObTzE2pxkINBLi0WWe9Ab2Zv',
    "requests_timeout": 60},

    {"consumer_key": 'r8Bvdm6I8QrRPuVzP4VtRYpqd',
    "consumer_secret": 'CzA8u8M8nDiDCCrSzCsXpR3SyTGCaLppDWbdTxSg78ZKgtKkhh',
    "access_token_key": '16562593-I3l0ZSmfZbMxIQ2NbiiM2eDMA4KNzFmFBeUkWxunR',
    "access_token_secret": '9HkILP4kSMF0hgvsB126jpoUzsRXETYMlSM0YSKb2yMJH',
    "requests_timeout": 60},

    {"consumer_key": 'NmMjfP1Zt3n2VDZ15X7SDGM6G',
    "consumer_secret": 'j9JBx7HUbMpcDnFteiIAAgHSoA8idlqQ20A1xbvnMrqMrOHQ1n',
    "access_token_key": '16562593-zUNyMUdO9JnSIstmTrqdyHHmX2lpv9NqkQxGC8faP',
    "access_token_secret": 'DEeHvLjTXlxNGmqDntXOK0cJCX08cnpg0btoRXWATW3X2',
    "requests_timeout": 60},

    

    {"consumer_key": 'E4azqaStWPQvfbtHNg8b1TkHO',
    "consumer_secret": 'zFxAMJFVRmYIeezhxCIFAnor9bxohFYouRdTbkp6V4gyBxiDvp',
    "access_token_key": '733478864530219008-sh2XNPjVwIt7XVyD0kU9LBJR77R25jw',
    "access_token_secret": 'z5ktoi3sxHghfyKZxAXQpjkmLYNLTYGoFZIK9q8XCVRTp',
    "requests_timeout": 60},


    {"consumer_key": 'oBpA2LLQwv90bvMaChYs9o91F',
    "consumer_secret": 'Jv14NpZ03qx1WQfTPVpzJZf7vP3uipwqtS7OSFXxKmuYQdGhFx',
    "access_token_key": '16562593-zUNyMUdO9JnSIstmTrqdyHHmX2lpv9NqkQxGC8faP',
    "access_token_secret": 'DEeHvLjTXlxNGmqDntXOK0cJCX08cnpg0btoRXWATW3X2',
    "requests_timeout": 60},

    {"consumer_key": '9K7SrSf7fbffaVmBNaO7Ace5r',
    "consumer_secret": 'JGIYddchEh6suhseGUwXxpFyp5UOgwQ8jaIA88rHSJfVJCoI7A',
    "access_token_key": '733478864530219008-2VQPGYxxNrUXhRrlO3IcHZJULWXYHIa',
    "access_token_secret": 'tGW7suoOOBfmX9DIDBwJmtvPK5xLLFQTtFEu2nPJsbswj',
    "requests_timeout": 60}


]

# Now create a list of twitter API objects
apis = []
for token in API_TOKENS:
    apis.append( twitter.Api(consumer_key=token['consumer_key'],
                    consumer_secret=token['consumer_secret'],
                    access_token_key=token['access_token_key'],
                    access_token_secret=token['access_token_secret']))
                    


# The account id / screen name we want followers from
account_screen_name = 'Heritage'
account_id = '10168082'

# Keep track of nodes connected to account, and all edges we need in the graph
nodes = set()
edges = defaultdict(set)


# Try to load first level followers from pickle;
# otherwise, generate them from a single API call and save via pickle
try:
    logger.info("Loading followers for %s" % account_screen_name)
    f = open("following1", "rb")
    following = pickle.load(f)
except Exception as e:
    logger.info("Failed. Generating followers for %s" % account_screen_name)
    following = api.GetFriendIDs(screen_name=account_screen_name)
    pickle.dump(following, open("following1", "wb"))

# Try to load the nodes and first level edges from pickle;
# otherwise generate them from the 'following' list and save
try:
    logger.info("Loading nodes and edges for depth = 1, for %s" % account_screen_name)
    n = open("nodes.follow1.set", "rb")
    e = open("edges.follow1.dict", "rb")
    nodes = pickle.load(n)
    edges = pickle.load(e)
except Exception as e:
    logger.info("Failed. Generating nodes and edges for depth = 1, for %s" % account_screen_name)
    for follower in following:
        nodes.add(follower)
        edges[account_id].add(follower)
    pickle.dump(nodes, open("nodes.follow1.set", "wb"))
    pickle.dump(edges, open("edges.follow1.dict", "wb"))



### Crawling for Depth2


# Index the api list, and start from the first api object
api_idx = 0
api = apis[api_idx]

# Some accounts give us issues (either too many followers or no permissions)
blacklist= [74323323, 43532023, 19608297, 25757924, 240369959, 173634807, 17008482, 142143804]
api_updated = False

# It is nice to start from a point in the list, instead of from the beginning
starting_point = 562989744
if starting_point:
    starting_point_idx = following.index(starting_point)
    following_iter = range(starting_point_idx, len(following))
else:
    following_iter = range(len(following))

# Try loading second layer of followers from pickle, otherwise start from scratch
try:
    f = open("edges.follow2.dict", "rb")
    edges = pickle.load(f)
    logger.info("Loaded edges.follow2 into memory!")
except Exception as e:
    logger.info("Starting from SCRATCH: did not load edges.follow2 into memory!")
    pass

# For each follower of the main account ...
for follower_idx in following_iter:
    follower = following[follower_idx]
    success = False
    
    # ... check if they are on the blacklist; if so, skip
    if follower in blacklist:
        logger.info("Skipping due to blacklist")
        continue

    # Otherwise, attempt to get list of their followers
    followers_depth2_list = []
    while not success:
        try:
            logger.info("Getting followers for follower %s" % follower)
            followers_depth2_list = getFollowers(api, follower)
            success = True
        except TimeoutError as e:
            # If api call takes too long, move on
            logger.info("Timeout after 60s for follower %d" % follower)
            success = True      # technically not a success but setting flag so next loop moves on
            continue
        except Exception as e:
            # IF we get here, then we hit API limits
            logger.info("API Exception %s; api-idx = %d" % (str(e), api_idx))
            
            # Are we at the begining of api list? 
            # IF so, dump edges so far via pickle and sleep
            if api_updated and api_idx % len(API_TOKENS) == 0 and api_idx >= len(API_TOKENS):
                logger.info("Save edges to pickle file for follower = %s" % follower)
                pickle.dump(edges, open("edges.follow2.dict", "wb"))
                logger.info("Sleeping ...")
                time.sleep(60)
                api_updated = False
            # Otherwise, move on to the next api object and try again
            else:
                api_idx += 1
                api = apis[api_idx % len(API_TOKENS)]
                api_updated = True
            
    
    # After getting the followers, find the intersection of those followers
    # with those of the first-level followers and add to edge dict
    if followers_depth2_list:
        logger.info("Adding followers to the graph")
        edges[follower].update(nodes.intersection(followers_depth2_list))


# Write out final list of edges via pickle
logger.info("Save edges to pickle file for follower = %s" % follower)
pickle.dump(edges, open("edges.follow2.dict", "wb"))