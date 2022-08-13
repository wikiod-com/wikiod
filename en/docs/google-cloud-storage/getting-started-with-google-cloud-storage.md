---
title: "Getting started with google-cloud-storage"
slug: "getting-started-with-google-cloud-storage"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Initial Setup
Google maintains documentation on getting started here: https://cloud.google.com/storage/docs/quickstart-console

Getting ready to use GCS:
1. [Create a Google Cloud project][1], if you don't have one already.
2. [Enable billing for your project][2] to allow buckets to be created.
3. (Optional) [Install the Google Cloud SDK][3], which includes gsutil, GCS's command line utility. As an alternative, you can use gsutil directly from the Google Cloud UI by using the [Google Cloud Shell][4].


  [1]: https://console.cloud.google.com/iam-admin/projects
  [2]: https://support.google.com/cloud/answer/6293499#enable-billing
  [3]: https://cloud.google.com/sdk/docs/
  [4]: https://cloud.google.com/shell/docs/

## HTTP Download (public objects)
If you want to download an object from GCS that is publicly viewable, the simplest way is to use a web browser or a command line tool to fetch a URL with this pattern: https://storage.googleapis.com/bucketName/objectName.

Example: https://storage.googleapis.com/pub/someOfTheTeam.jpg

## Upload Files using Python
Import needed libraries:

    from gcloud import storage

Define needed variables:

**Client**: Bundles the configuration needed for API requests

    client = storage.Client()

Optional params for `Client()`:

 - **project**: the project which the client acts on behalf of. Will be passed when creating a topic. If not passed, falls back to the default inferred from the environment.
 - **credentials**: OAuth2 Credentials used for the connection. If not passed, falls back to the default inferred from the environment.
 - **http**: HTTP object to make requests.  If not passed, an `http` object is created that is bound to the credentials for the current object.

**Bucket**: Selects the bucket created in the project through the Google Cloud Console

    bucket = client.get_bucket('<your-bucket-name>')

For more detailed information about the `Client` functions refer to [Storage Client][1]

**Blob**: File name that will be saved. 

    blob = bucket.blob('my-test-file.txt')

You can also define directories like this:

    filename = "%s/%s" % (folder, filename)
    blob = bucket.blob(filename)
    
There are several methods to upload a file. You can be expecting a file in the payload of a `POST` or `PUT` request, or have it locally on your file system. You can even send text directly to a text file.

    # Uploading string of text
    blob.upload_from_string('this is test content!')
    
    # Uploading from a local file using open()
    with open('photo.jpg', 'rb') as photo:
        blob.upload_from_file(photo)
    
    # Uploading from local file without open()
    blob.upload_from_filename('photo.jpg')

For more detailed information about the upload functions refer to [Blob/Objects][2]

If you need your blob to be public, you can set the privacy of the file public:

    blob.make_public()
    url = blob.public_url

 


  [1]: https://googlecloudplatform.github.io/google-cloud-python/stable/storage-client.html
  [2]: https://googlecloudplatform.github.io/google-cloud-python/stable/storage-blobs.html

