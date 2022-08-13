---
title: "Visual Recognition"
slug: "visual-recognition"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Get a list of custom classifiers
This lists all of the custom classifiers you have trained.

```
'use strict';

let watson = require('watson-developer-cloud');

var visualRecognition = watson.visual_recognition({
  version: 'v3',
  api_key: process.env['API_KEY'],
  version_date:'2016-05-19'
});


let url = "https://upload.wikimedia.org/wikipedia/commons/1/1c/Chris_Evans_filming_Captain_America_in_DC_cropped.jpg"

visualRecognition.classify({url: url}, function(error, results) {
  console.log(JSON.stringify(results,null,2));
});
```

## Get information about a specific custom classifier
This returns information about a specific classifier ID you have trained. This includes information about its current status (i.e., if it is ready or not).

```
'use strict';

let watson = require('watson-developer-cloud');

var visualRecognition = watson.visual_recognition({
  version: 'v3',
  api_key: process.env.API_KEY,
  version_date:'2016-05-19'
});


visualRecognition.getClassifier({classifier_id: 'DogBreeds_1162972348'}, function(error, results) {
  console.log(JSON.stringify(results,null,2));
});
```



## Train a custom classifier
Training a custom classifier requires a corpus of images organized into groups.   In this example, I have a bunch of images of apples in one ZIP file, a bunch of images of bananas in another ZIP file, and a third group of images of things that are *not* fruits for a *negative set*. Once a custom classifier is created, it will be in state `training` and you'll have to use the classifier ID to check if it is ready (using the 'Get information about a specific custom classifier' example).

```
'use strict';

let watson = require('watson-developer-cloud');
let fs = require('fs');

var visualRecognition = watson.visual_recognition({
  version: 'v3',
  api_key: process.env.API_KEY,
  version_date:'2016-05-19'
});


let custom_classifier = {
  apple_positive_examples: fs.createReadStream('./apples.zip'),
  banana_positive_examples: fs.createReadStream('./bananas.zip'),
  negative_examples: fs.createReadStream('./non-fruits.zip'),
  name: 'The Name of My Classifier'
}

visualRecognition.createClassifier(custom_classifier, function(error, results) {
  console.log(JSON.stringify(results,null,2));
});
```


## Delete a custom classifier
```
'use strict';

let watson = require('watson-developer-cloud');
let fs = require('fs');

var visualRecognition = watson.visual_recognition({
  version: 'v3',
  api_key: process.env.API_KEY,
  version_date:'2016-05-19'
});

let classifier_id_to_delete = 'TheNameofMyClassifier_485506080';

visualRecognition.deleteClassifier({classifier_id: classifier_id_to_delete}, function(error, results) {
  console.log(JSON.stringify(results,null,2));
});
```

## Classify an Image
# Prerequisites

First, you have to install the `watson-developer-cloud` SDK.

```
$ npm install watson-developer-cloud
```

# Classify an image URL

 ![Chris Evans](https://upload.wikimedia.org/wikipedia/commons/1/1c/Chris_Evans_filming_Captain_America_in_DC_cropped.jpg)


We'll use an image of Captain America from Wikipedia.  

```
'use strict';

let watson = require('watson-developer-cloud');

var visualRecognition = watson.visual_recognition({
  version: 'v3',
  api_key: "<YOUR API KEY GOES HERE>",
  version_date:'2016-05-19'
});


let url = "https://upload.wikimedia.org/wikipedia/commons/1/1c/Chris_Evans_filming_Captain_America_in_DC_cropped.jpg"

visualRecognition.classify({url: url}, function(error, results) {
  console.log(JSON.stringify(results,null,2));
});
```

