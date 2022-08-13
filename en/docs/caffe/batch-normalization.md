---
title: "Batch normalization"
slug: "batch-normalization"
draft: false
images: []
weight: 9318
type: docs
toc: true
---

From [the docs](http://caffe.berkeleyvision.org/doxygen/classcaffe_1_1BatchNormLayer.html):

"Normalizes the input to have 0-mean and/or unit (1) variance across the batch.

This layer computes Batch Normalization as described in [1].

[...]

[1] S. Ioffe and C. Szegedy, "Batch Normalization: Accelerating Deep Network Training by Reducing Internal Covariate Shift." arXiv preprint arXiv:1502.03167 (2015)."

## Parameters
| Parameter| Details|
| ------ | ------ |
| use_global_stats   | [From rohrbach's post from 2nd March 2016](https://github.com/BVLC/caffe/issues/3347) - maybe he knows: |
| (use_global_stats) |"By default, during training time, the network is computing global mean/ variance statistics via a running average, which is then used at test time to allow deterministic outputs for each input. You can manually toggle whether the network is accumulating or using the statistics via the use_global_stats option. IMPORTANT: for this feature to work, you MUST set the learning rate to zero for all three parameter blobs, i.e., param {lr_mult: 0} three times in the layer definition. |
| (use_global_stats) |This means by default (as the following is set in batch_norm_layer.cpp), you don't have to set use_global_stats at all in the prototxt. use_global_stats_ = this->phase_ == TEST;" |

## Prototxt for training
The following is an example definition for training a BatchNorm layer with channel-wise scale and bias. Typically a BatchNorm layer is inserted between convolution and rectification layers. In this example, the convolution would output the blob `layerx` and the rectification would receive the `layerx-bn` blob.

    layer { bottom: 'layerx' top: 'layerx-bn' name: 'layerx-bn' type: 'BatchNorm'
      batch_norm_param {
        use_global_stats: false  # calculate the mean and variance for each mini-batch
        moving_average_fraction: .999  # doesn't effect training 
      }
      param { lr_mult: 0 } 
      param { lr_mult: 0 } 
      param { lr_mult: 0 }}
    # channel-wise scale and bias are separate
    layer { bottom: 'layerx-bn' top: 'layerx-bn' name: 'layerx-bn-scale' type: 'Scale',
      scale_param { 
        bias_term: true
        axis: 1      # scale separately for each channel
        num_axes: 1  # ... but not spatially (default)
        filler { type: 'constant' value: 1 }           # initialize scaling to 1
        bias_filler { type: 'constant' value: 0.001 }  # initialize bias
    }}

More information can be found in [this thread][1].

 [1]: http://stackoverflow.com/q/41269570/1714410

## Prototxt for deployment
The main change needed is to switch `use_global_stats` to `true`. This switches to using the moving average.

    layer { bottom: 'layerx' top: 'layerx-bn' name: 'layerx-bn' type: 'BatchNorm'
      batch_norm_param {
        use_global_stats: true  # use pre-calculated average and variance
      }
      param { lr_mult: 0 } 
      param { lr_mult: 0 } 
      param { lr_mult: 0 }}
    # channel-wise scale and bias are separate
    layer { bottom: 'layerx-bn' top: 'layerx-bn' name: 'layerx-bn-scale' type: 'Scale',
      scale_param { 
        bias_term: true
        axis: 1      # scale separately for each channel
        num_axes: 1  # ... but not spatially (default)
    }}

