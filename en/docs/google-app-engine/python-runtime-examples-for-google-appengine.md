---
title: "Python Runtime Examples for Google Appengine"
slug: "python-runtime-examples-for-google-appengine"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## NDB with Python on AppEngine
NDB relates models as python objects, which can be stored and accessed in [the Appengine NoSQL datastore][1], available to all AppEngine applications.  


models.py

    from google.appengine.ext import ndb

    # https://cloud.google.com/appengine/docs/python/ndb/properties
    
    
    class Series(ndb.Model):
        """TV Series Object"""
        folder_name = ndb.StringProperty()
        title = ndb.StringProperty()
        rating = ndb.StringProperty()
        banner_blob_key = ndb.BlobKeyProperty()
        year = ndb.IntegerProperty()
        plot = ndb.TextProperty()
        genre = ndb.StringProperty(repeated=True)
        json_of_show = ndb.JsonProperty()
        date_added = ndb.DateTimeProperty(auto_now_add=True)
        date_updated = ndb.DateTimeProperty(auto_now=True)
    
    
    class Episode(ndb.Model):
        """Episode Object (Series have Episodes)"""
        series = ndb.KeyProperty(kind=Series)
        episode_title = ndb.StringProperty()
        season = ndb.IntegerProperty()
        episode_number = ndb.IntegerProperty()
        thumb_blob_key = ndb.BlobKeyProperty()
        episode_json = ndb.JsonProperty()
        date_added = ndb.DateTimeProperty(auto_now_add=True)
        date_updated = ndb.DateTimeProperty(auto_now=True)


With out models defined we can create new objects for entry to the datastore:

    nfo = xmltodict.parse(my_great_file.xml)
    s = Series()
    s.folder_name = gcs_file.filename[:-10]
    s.title = nfo['tvshow'].get('title', None)
    s.rating = nfo['tvshow'].get('rating', None)
    #  Below we use the google cloud storage library to generate a blobkey for a GCS file
    s.banner_blob_key = BlobKey((blobstore.create_gs_key('/gs' + gcs_file.filename[:-10] + 'banner.jpg')))
    s.year = int(nfo['tvshow'].get('year', None))
    s.plot = nfo['tvshow'].get('plot', None)
    #  genre is a repeated type, and can be stored as a list
    s.genre = nfo['tvshow'].get('genre', 'None').split('/')
    s.json = json.dumps(nfo)
    s.put_async()  #put_async writes to the DB without waiting for confirmation of write.
    

Adding an episode and relating it to a Series:
    
    nfo = xmltodict.parse(my_great_file.xml)
    
    epi = Episode()
    epi.show_title = nfo['episodedetails'].get('showtitle', None)
    epi.title = nfo['episodedetails'].get('title', None)

    #  We'll query the Series for use later
    show_future = Series.query(Series.title == epi.show_title).get_async()

    epi.json = json.dumps(nfo)                    
    ... #  We perform other assorted operations to store data in episode properties                        

    #  Ask for the show we async queried earlier                    
    show = show_future.get_result()
    #  Associate this episode object with a Series by Key
    epi.series = show.key
    epi.put_async()  #  Write the object without waiting 


Later, to retrieve all Series:

    shows = Series.query() 
[Filters][2] could be applied if all shows were not desired.  

More reading:

 - [Life of a Datastore Write][3]

                     


  [1]: https://cloud.google.com/appengine/docs/python/ndb/
  [2]: https://cloud.google.com/appengine/docs/python/ndb/#queries
  [3]: https://cloud.google.com/appengine/articles/life_of_write

