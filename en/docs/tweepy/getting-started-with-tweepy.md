---
title: "Getting started with tweepy"
slug: "getting-started-with-tweepy"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation
Tweepy can be installed from its [PyPI repository](https://pypi.python.org/pypi/tweepy) using `pip` or `easy_install`:

    pip install tweepy
or

    easy_install tweepy

You can also download the source [from GitHub](https://github.com/tweepy/tweepy) and install it using `setup.py`:

    python setup.py install

See the [tweepy documentation](http://docs.tweepy.org/en/v3.5.0/index.html) for more.

## Using Tweepy to access the Twitter Search API
The Search API provides access to recent tweets*. This is as opposed to the Stream API, which provides search results in real-time.



    <example>



*Note that "the Search API is focused on relevance and not completeness" - [Twitter Search API](https://dev.twitter.com/rest/public/search)

## Using Tweepy to access the Twitter Stream API
The Stream API provides access to tweets in real-time. Streams can be filtered based on keywords, language, location, and more. Here's a simple example to track mentions of the word "tweepy":
    
    #set up a new class using tweepy.StreamListener

    class SimpleListener(tweepy.StreamListener):
        def on_status(self, status): 
            #code to run each time the stream receives a status
            print(status.text)

        def on_direct_message(self, status): 
            #code to run each time the stream receives a direct message
            print(status.text)
        
        def on_data(self, status):
            #code to run each time you receive some data (direct message, delete, profile update, status,...)
            print(status.text)

        def on_error(self, staus_code):
            #code to run each time an error is received
            if status_code == 420:
                return False
            else:
                return True

    #initialize the stream
    
    tweepy_listener = SimpleListener()
    tweepy_stream = tweepy.Stream(auth = api.auth, listener=tweepy_listener())
    tweepy_stream.filter(track=['tweepy'])

You can track different keywords by changing the `track` parameter.

    <to add: examples of filtering based on locations, languages, etc.>

You can track data adressed to your account by using `userstream()` instead of filter.

    api.userstream(async=True)

