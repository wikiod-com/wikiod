---
title: "Custom Python Layers"
slug: "custom-python-layers"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

This tutorial will guide through the steps to create a simple custom layer for Caffe using python. By the end of it, there are some examples of custom layers. Usually you would create a custom layer to implement a functionality that isn't available in Caffe, tuning it for your requirements.

Creating a python custom layer adds some overhead to your network and probably isn't as efficient as a C++ custom layer. However, this way, you won't have to compile the whole caffe with your new layer.

## Parameters
| **Parameter** | **Details** |
| ------ | ------ |
| top   | An array with the top blobs of your layer. Access data passed to it by using *top[i].data*, where **i** is the index of a specific blob   |
| bottom   | An array with the bottom blobs of your layer. Access data passed to it by using *bottom[i].data*, where **i** is the index of a specific blob|



# - Caffe build with Python layer
Caffe needs to be compiled with `WITH_PYTHON_LAYER` option:

    WITH_PYTHON_LAYER=1 make && make pycaffe


# - Where should I save the class file?
You have two options (at least that I know of). Either you can save the custom layer file in the same folder as you are going to run the caffe command (probably where your prototxt files would be). Another way, also my favorite one, is to save all your custom layers in a folder and adding this folder to your PYTHONPATH.

# References
1. [Christopher Bourez's blog](http://christopher5106.github.io/deep/learning/2015/09/04/Deep-learning-tutorial-on-Caffe-Technology.html)
2. [Caffe Github](https://github.com/BVLC/caffe/issues/684)
3. [StackOverflow](https://github.com/BVLC/caffe/issues/684)

## Layer Template
```python
import caffe

class My_Custom_Layer(caffe.Layer):
    def setup(self, bottom, top):
        pass
        
    def forward(self, bottom, top):
        pass
        
    def reshape(self, bottom, top):
        pass

    def backward(self, bottom, top):
        pass
```

So important things to remember:
- Your custom layer has to inherit from **caffe.Layer** (so don't forget to *import caffe*);
- You must define the four following methods: **setup**, **forward**, **reshape** and **backward**;
- All methods have a *top* and a *bottom* parameters, which are the blobs that store the input and the output passed to your layer. You can access it using *top[i].data* or *bottom[i].data*, where *i* is the index of the blob in case you have more than one upper or lower blob.

### - Setup method
The Setup method is called once during the lifetime of the execution, when Caffe is instantiating all layers. This is where you will read parameters, instantiate fixed-size buffers. 

### - Reshape method
Use the reshape method for initialization/setup that depends on the bottom blob (layer input) size. It is called once when the network is instantiated.

### - Forward method
The Forward method is called for each input batch and is where most of your logic will be.

### - Backward method
The Backward method is called during the backward pass of the network. For example, in a convolution-like layer, this would be where you would calculate the gradients. This is optional (a layer can be forward-only).

## Prototxt Template
 Ok, so now you have your layer designed! This is how you define it in your *.prototxt* file:

```
layer {
  name: "LayerName"
  type: "Python"
  top: "TopBlobName"
  bottom: "BottomBlobName"
  python_param {
    module: "My_Custom_Layer_File"
    layer: "My_Custom_Layer_Class"
    param_str: '{"param1": 1,"param2":True, "param3":"some string"}'
  }
  include{
        phase: TRAIN
  }
}
```

Important remarks:
- **type** must be **Python**;
- You must have a **python_param** dictionary with at least the **module** and **layer** parameters;
- **module** refers to the file where you implemented your layer (without the *.py*);
- **layer** refers to the name of your class;
- You can pass parameters to the layer using **param_str** (more on accessing them bellow);
- Just like any other layer, you can define in which phase you want it to be active (see the examples to see how you can check the current phase);

## Passing parameters to the layer
You can define the layer parameters in the prototxt by using *param_str*. Once you've done it, here is an example on how you access these paremeters inside the layer class:

```python
def setup(self, bottom, top):
    params = eval(self.param_str)
    param1 = params["param1"]
    param2 = params.get('param2', False) #I usually use this when fetching a bool
    param3 = params["param3"]
    
    #Continue with the setup
    # ...
```

## Measure Layer
In this example we will design a "measure" layer, that outputs the accuracy and a confusion matrix for a binary problem during training and the accuracy, false positive rate and false negative rate during test/validation. Although Caffe already has a Accuracy layer, sometimes you want something more, like a F-measure.

This is my *measureLayer.py* with my class definition:

```python
#Remark: This class is designed for a binary problem, where the first class would be the 'negative'
# and the second class would be 'positive'

import caffe
TRAIN = 0
TEST = 1

class Measure_Layer(caffe.Layer):
    #Setup method
    def setup(self, bottom, top):
        #We want two bottom blobs, the labels and the predictions
        if len(bottom) != 2:
            raise Exception("Wrong number of bottom blobs (prediction and label)") 

        #And some top blobs, depending on the phase
        if self.phase = TEST and len(top) != 3:
            raise Exception("Wrong number of top blobs (acc, FPR, FNR)")
        if self.phase = TRAIN and len(top) != 5:
            raise Exception("Wrong number of top blobs (acc, tp, tn, fp and fn)")
       
        #Initialize some attributes
        self.TPs = 0.0
        self.TNs = 0.0
        self.FPs = 0.0
        self.FNs = 0.0
        self.totalImgs = 0

    #Forward method
    def forward(self, bottom, top):
        #The order of these depends on the prototxt definition
        predictions = bottom[0].data
        labels = bottom[1].data

        self.totalImgs += len(labels)

        for i in range(len(labels)): #len(labels) is equal to the batch size
                pred = predictions[i]   #pred is a tuple with the normalized probability 
                                        #of a sample i.r.t. two classes
                lab = labels[i]
                
                if pred[0] > pred[1]:
                        if lab == 1.0:
                                self.FNs += 1.0
                        else:
                                self.TNs += 1.0
                else:
                        if lab == 1.0:
                                self.TPs += 1.0
                        else:
                                self.FPs += 1.0

        acc = (self.TPs + self.TNs) / self.totalImgs
        
        try: #just assuring we don't divide by 0
                fpr = self.FPs / (self.FPs + self.TNs)
        except:
                fpr = -1.0

        try: #just assuring we don't divide by 0
                fnr = self.FNs / (self.FNs + self.TPs)
        except:
                fnr = -1.0
           
       #output data to top blob
       top[0].data = acc
       if self.phase == TRAIN:
           top[1].data = self.TPs
           top[2].data = self.TNs
           top[3].data = self.FPs
           top[4].data = self.FNs
       elif self.phase == TEST:
           top[1].data = fpr
           top[2].data = fnr
           
    def reshape(self, bottom, top):
        """
        We don't need to reshape or instantiate anything that is input-size sensitive
        """
        pass

    def backward(self, bottom, top):
        """
        This layer does not back propagate
        """
        pass
```

And this is an example of a *prototxt* with it:
```
layer {
  name: "metrics"
  type: "Python"
  top: "Acc"
  top: "TPs"
  top: "TNs"
  top: "FPs"
  top: "FNs"
  
  bottom: "prediction"   #let's supose we have these two bottom blobs
  bottom: "label"

  python_param {
    module: "measureLayer"
    layer: "Measure_Layer"
  }
  include {
    phase: TRAIN
  }
}

layer {
  name: "metrics"
  type: "Python"
  top: "Acc"
  top: "FPR"
  top: "FNR"
  
  bottom: "prediction"   #let's supose we have these two bottom blobs
  bottom: "label"

  python_param {
    module: "measureLayer"
    layer: "Measure_Layer"
  }
  include {
    phase: TEST
  }
}
```

## Data Layer
This example is a custom data layer, that receives a text file with image paths, loads a batch of images and preprocesses them. Just a quick tip, Caffe already has a big range of data layers and probably a custom layer is not the most efficient way if you just want something simple.

My *dataLayer.py* could be something like:
```python
import caffe

class Custom_Data_Layer(caffe.Layer):
    def setup(self, bottom, top):
        # Check top shape
        if len(top) != 2:
                raise Exception("Need to define tops (data and label)")
        
        #Check bottom shape
        if len(bottom) != 0:
            raise Exception("Do not define a bottom.")
        
        #Read parameters
        params = eval(self.param_str)
        src_file = params["src_file"]
        self.batch_size = params["batch_size"]
        self.im_shape = params["im_shape"]
        self.crop_size = params.get("crop_size", False)
        
        #Reshape top
        if self.crop_size:
            top[0].reshape(self.batch_size, 3, self.crop_size, self.crop_size)
        else:
            top[0].reshape(self.batch_size, 3, self.im_shape, self.im_shape)
            
        top[1].reshape(self.batch_size)

        #Read source file
        #I'm just assuming we have this method that reads the source file
        #and returns a list of tuples in the form of (img, label)
        self.imgTuples = readSrcFile(src_file) 
        
        self._cur = 0 #use this to check if we need to restart the list of imgs
        
    def forward(self, bottom, top):
        for itt in range(self.batch_size):
            # Use the batch loader to load the next image.
            im, label = self.load_next_image()
            
            #Here we could preprocess the image
            # ...
            
            # Add directly to the top blob
            top[0].data[itt, ...] = im
            top[1].data[itt, ...] = label
    
    def load_next_img(self):
        #If we have finished forwarding all images, then an epoch has finished
        #and it is time to start a new one
        if self._cur == len(self.imgTuples):
            self._cur = 0
            shuffle(self.imgTuples)
        
        im, label = self.imgTuples[self._cur]
        self._cur += 1
        
        return im, label
    
    def reshape(self, bottom, top):
        """
        There is no need to reshape the data, since the input is of fixed size
        (img shape and batch size)
        """
        pass

    def backward(self, bottom, top):
        """
        This layer does not back propagate
        """
        pass
```

And the *prototxt* would be like:
```
layer {
  name: "Data"
  type: "Python"
  top: "data"
  top: "label"
 
  python_param {
    module: "dataLayer"
    layer: "Custom_Data_Layer"
    param_str: '{"batch_size": 126,"im_shape":256, "crop_size":224, "src_file": "path_to_TRAIN_file.txt"}'
  }
}
```

