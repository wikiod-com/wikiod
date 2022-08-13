---
title: "File Uploading"
slug: "file-uploading"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

The CollectionFS package has been shelved and discontinued by it's author; however, since there's no alternative package in Atmosphere or the Meteor ecosystem for using Mongo's GridFS functionality, and the code still works perfectly fine; we recommend not removing the example from StackOverflow Documentation until some other GridFS solution can be documented as it's replacement.  


**Additional Research**  
[Filepicker.io Uploads and Image Conversion](https://meteor.hackpad.com/Meteor-Cookbook-Filepicker.io-Uploads-and-Image-Conversion-hIpSwJQV3HJ)  
[Dario's Save File Pattern](https://gist.github.com/dariocravero/3922137  )  
[Micha Roon's File Upload Pattern](https://coderwall.com/p/7tpa8w/file-upload-with-meteor-method)  
[EventedMind File Upload Package](https://www.eventedmind.com/items/meteor-build-a-file-upload-package)  

## Server/Client
Uploading files can be easy or really complicated, depending on what you're wanting to do. In general, transfering a file itself isn't all that difficult. But there are lots of edge cases around attachments, binary files, and the like. And the real sticking point is horizontal scaling, and creating a solution that works when the server is cloned a second, third, and nth time.

Let's start with a basic server/client upload model. We begin by adding a file input element to the document object model.

```html
<template name="example">
  <input type=file />
</template>
```

Then attach an event to the input element within your controller, and call a local Meteor method ``startFileTransfer'' to initiate the transfer.

```js
// client/example.js
Template.example.events({
  'change input': function(ev) {  
    _.each(ev.srcElement.files, function(file) {
      Meteor.startFileTransfer(file, file.name);
    });
  }
});

// client/save.js
/**
 * @blob (https://developer.mozilla.org/en-US/docs/DOM/Blob)
 * @name the file's name
 * @type the file's type: binary, text (https://developer.mozilla.org/en-US/docs/DOM/FileReader#Methods) 
 *
 * TODO Support other encodings: https://developer.mozilla.org/en-US/docs/DOM/FileReader#Methods
 * ArrayBuffer / DataURL (base64)
 */
Meteor.startFileTransfer = function(blob, name, path, type, callback) {
  var fileReader = new FileReader(),
    method, encoding = 'binary', type = type || 'binary';
  switch (type) {
    case 'text':
      // TODO Is this needed? If we're uploading content from file, yes, but if it's from an input/textarea I think not...
      method = 'readAsText';
      encoding = 'utf8';
      break;
    case 'binary': 
      method = 'readAsBinaryString';
      encoding = 'binary';
      break;
    default:
      method = 'readAsBinaryString';
      encoding = 'binary';
      break;
  }
  fileReader.onload = function(file) {
    Meteor.call('saveFileToDisk', file.srcElement.result, name, path, encoding, callback);
  }
  fileReader[method](blob);
}
```

The client will then call the saveFileToDisk server method, which does the actual transfer and puts everything to disk.

```js
// 
/**
 * TODO support other encodings:
 * http://stackoverflow.com/questions/7329128/how-to-write-binary-data-to-a-file-using-node-js
 */
Meteor.methods({
  saveFileToDisk: function(blob, name, path, encoding) {
    var path = cleanPath(path), fs = __meteor_bootstrap__.require('fs'),
      name = cleanName(name || 'file'), encoding = encoding || 'binary',
      chroot = Meteor.chroot || 'public';
    // Clean up the path. Remove any initial and final '/' -we prefix them-,
    // any sort of attempt to go to the parent directory '..' and any empty directories in
    // between '/////' - which may happen after removing '..'
    path = chroot + (path ? '/' + path + '/' : '/');

    // TODO Add file existance checks, etc...
    fs.writeFile(path + name, blob, encoding, function(err) {
      if (err) {
        throw (new Meteor.Error(500, 'Failed to save file.', err));
      } else {
        console.log('The file ' + name + ' (' + encoding + ') was saved to ' + path);
      }
    }); 

    function cleanPath(str) {
      if (str) {
        return str.replace(/\.\./g,'').replace(/\/+/g,'').
          replace(/^\/+/,'').replace(/\/+$/,'');
      }
    }
    function cleanName(str) {
      return str.replace(/\.\./g,'').replace(/\//g,'');
    }
  }
});
```

That's sort of the bare-bones approach, and it leaves a lot to be desired. It's maybe good for uploading a CSV file or something, but that's about it.

## Dropzone (with iron:router)
If we want something a bit more polished, with an integrated Dropzone UI and a REST endpoint, we're going to need to start adding custom REST routes and packages with UI helpers.

Lets begin by importing Iron Router and Dropzone.

```
 meteor add iron:router
 meteor add awatson1978:dropzone
```

And configure the uploads url route that's specified in the dropzone helper.

```
Router.map(function () {
    this.route('uploads', {
      where: 'server',
      action: function () {
        var fs = Npm.require('fs');
        var path = Npm.require('path');
        var self = this;

        ROOT_APP_PATH = fs.realpathSync('.');

        // dropzone.js stores the uploaded file in the /tmp directory, which we access
        fs.readFile(self.request.files.file.path, function (err, data) {

          // and then write the file to the uploads directory
          fs.writeFile(ROOT_APP_PATH + "/assets/app/uploads/" +self.request.files.file.name, data, 'binary', function (error, result) {
            if(error){
              console.error(error);
            }
            if(result){
              console.log('Success! ', result);
            }
          });
        });
      }
    });
  });
```

Cool! We have a file uploader with snazzy UI and a programmable REST endpoint. Unfortunately, this doesn't scale particularly well.

## Filepicker.io
To scale things, we have to stop using local storage on our server, and start using either a dedicated file storage service or implement a horizontal storage layer. The easiest way to get started with scalable file storage is to use a solution like Filepicker.io, which supports S3, Azure, Rackspace, and Dropbox. loadpicker has been a popular Filerpicker unipackage for awhile.

```
meteor add mrt:filepicker
```

The Filepicker pattern is rather different than the other solutions, because it's really about 3rd party integration. Begin by adding a filepicker input, which you'll see relies heavily on data-* attributes, which is a fairly uncommon pattern in Meteor apps.

```html
<input type="filepicker"
  id="filepickerAttachment"
  data-fp-button-class="btn filepickerAttachment"
  data-fp-button-text="Add image" 
  data-fp-mimetypes="image/*"
  data-fp-container="modal"
  data-fp-maxsize="5000000" 
  data-fp-services="COMPUTER,IMAGE_SEARCH,URL,DROPBOX,GITHUB,GOOGLE_DRIVE,GMAIL">
```

You'll walso want to set an API key, construct the filepicker widget, trigger it, and observe it's outputs.

```js
if(Meteor.isClient){
  Meteor.startup(function() {
    filepicker.setKey("YourFilepickerApiKey");
  });
  Template.yourTemplate.rendered = function(){
    filepicker.constructWidget($("#filepickerAttachment"));
  }
  Template.yourTemplate.events({
  'change #filepickerAttachment': function (evt) {
    console.log("Event: ", evt, evt.fpfile, "Generated image url:", evt.fpfile.url);
  });
});
```

## Server Uploads
The following scripts are for uploading a file from the server filesystem into the server. Mostly for config files and filewatchers.

```
//https://forums.meteor.com/t/read-file-from-the-public-folder/4910/5

// Asynchronous Method.
Meteor.startup(function () {
    console.log('starting up');

    var fs = Npm.require('fs');
    // file originally saved as public/data/taxa.csv
    fs.readFile(process.cwd() + '/../web.browser/app/data/taxa.csv', 'utf8', function (err, data) {
        if (err) {
            console.log('Error: ' + err);
            return;
        }

        data = JSON.parse(data);
        console.log(data);
    });
});


// Synchronous Method.
Meteor.startup(function () {
    var fs = Npm.require('fs');
    // file originally saved as public/data/taxa.csv
    var data = fs.readFileSync(process.cwd() + '/../web.browser/app/data/taxa.csv', 'utf8');

    if (Icd10.find().count() === 0) {
        Icd10.insert({
            date:  new Date(),
            data:  JSON.parse(data)
        });
    }
});


Meteor.methods({
  parseCsvFile:function (){
    console.log('parseCsvFile');

    var fs = Npm.require('fs');
    // file originally saved as public/data/taxa.csv
    var data = fs.readFileSync(process.cwd() + '/../web.browser/app/data/taxa.csv', 'utf8');
    console.log('data', data);
  }
});
```

## CollectionFS
However, if you're really serious about storage, and you want to store millions of images, you're going to need to leverage Mongo's GridFS infrastructure, and create yourself a storage layer. For that, you're going to need the excellent CollectionFS subsystem.

Start by adding the necessary packages.

```
meteor add cfs:standard-packages
meteor add cfs:filesystem
```

And adding a file upload element to your object model.

```html
<template name="yourTemplate">
    <input class="your-upload-class" type="file">
</template>
```

Then add an event controller on the client.

```
Template.yourTemplate.events({
    'change .your-upload-class': function(event, template) {
        FS.Utility.eachFile(event, function(file) {
            var yourFile = new FS.File(file);
            yourFile.creatorId = Meteor.userId(); // add custom data
            YourFileCollection.insert(yourFile, function (err, fileObj) {
                if (!err) {
                   // do callback stuff
                }
            });
        });
    }
});
```

And define your collections on your server:

```
YourFileCollection = new FS.Collection("yourFileCollection", {
    stores: [new FS.Store.FileSystem("yourFileCollection", {path: "~/meteor_uploads"})]
});
YourFileCollection.allow({
    insert: function (userId, doc) {
        return !!userId;
    },
    update: function (userId, doc) {
        return doc.creatorId == userId
    },
    download: function (userId, doc) {
        return doc.creatorId == userId
    }
});
```

Thanks to Raz for this excellent example. You'll want to check out the complete CollectionFS Documentation for more details on what all CollectionFS can do.

