---
title: "Usage of `accumarray()` Function"
slug: "usage-of-accumarray-function"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

`accumarray` allows to aggregate items of an array in various ways, potentially applying some function to the items in the process. `accumarray` can be thought of as a lightweight [reducer][1] (see also: https://www.wikiod.com/hadoop/introduction-to-mapreduce ).

This topic will contain common scenarios where `accumarray` is especially useful.

  [1]: https://en.wikipedia.org/wiki/MapReduce "Wikipedia: MapReduce"

## Syntax
 - accumarray(subscriptArray, valuesArray)
 - accumarray(subscriptArray, valuesArray, sizeOfOutput)
 - accumarray(subscriptArray, valuesArray, sizeOfOutput, funcHandle)
 - accumarray(subscriptArray, valuesArray, sizeOfOutput, funcHandle, fillVal)
 - accumarray(subscriptArray, valuesArray, sizeOfOutput, funcHandle, fillVal, isSparse)

## Parameters
| Parameter | Details |  
| --------- | ------- |  
| `subscriptArray` | Subscript matrix, specified as a vector of indices, matrix of indices, or cell array of index vectors.  |  
| `valuesArray`| Data, specified as a vector or a scalar. |
| `sizeOfOutput`| Size of output array, specified as a vector of positive integers. |
| `funcHandle`| Function to be applied to each set of items during aggregation, specified as a function handle or `[]`. |
| `fillVal`| Fill value, for when `subs` does not reference each element in the output. |
| `isSparse`| Should the output be a sparse array? |


- Introduced in MATLAB v7.0.
## **References**:
1. ["Under-appreciated `accumarray`", _by Loren Shure_, February 20, 2008](
http://blogs.mathworks.com/loren/2008/02/20/under-appreciated-accumarray/).
2. [`accumarray`][1] in the official MATLAB documentation.


  [1]: https://www.mathworks.com/help/matlab/ref/accumarray.html "Construct array with accumulation"

## Finding the maximum value among elements grouped by another vector
**<sup>This is an official MATLAB example</sup>**

Consider the following code:

    month = [1;1;2;3;8;1;3;4;9;11;9;12;3;4;11];
    temperature = [57;61;60;62;45;59;64;66;40;56;38;65;61;64;55];
    maxTemp = accumarray(month,temperature,[],@max);

The image below demonstrates the computation process done by `accumarray` in this case:

[![Explanation of computation process][1]][1]

In this example, all values that have the same `month` are first collected, and then the function specified by the 4<sup>th</sup> input to `accumarray` (in this case, `@max`) is applied to each such set.

  [1]: https://i.stack.imgur.com/MSlId.png

## Apply Filter to Image Patches and Set Each Pixel as the Mean of the Result of Each Patch
Many modern Image Processing algorithms use patches are their basic element to work on.  
For instance one could denoise patches (See BM3D Algorithm).  

Yet when building the image form the processed patches we have many results for the same pixel.  
One way to deal with it is taking the average (Empirical Mean) of all values of the same pixel.

The following code shows how to break an image into patches and them reconstruct the image from patches using the average by using `[accumarray()][1]`:


    numRows = 5;
    numCols = 5;
    
    numRowsPatch = 3;
    numColsPatch = 3;
    
    % The Image
    mI = rand([numRows, numCols]);
    
    % Decomposing into Patches - Each pixel is part of many patches (Neglecting
    % boundariwes, each pixel is part of (numRowsPatch * numColsPatch) patches).
    mY = ImageToColumnsSliding(mI, [numRowsPatch, numColsPatch]);
    
    % Here one would apply some operation which work on patches
    
    % Creating image of the index of each pixel
    mPxIdx = reshape(1:(numRows * numCols), [numRows, numCols]);
    
    % Creating patches of the same indices
    mSubsAccu = ImageToColumnsSliding(mPxIdx, [numRowsPatch, numColsPatch]);
    
    % Reconstruct the image - Option A
    mO = accumarray(mSubsAccu(:), mY(:)) ./ accumarray(mSubsAccu(:), 1);
    
    % Reconstruct the image - Option B
    mO = accumarray(mSubsAccu, mY(:), [(numRows * numCols), 1], @(x) mean(x));
    
    % Rehsape the Vector into the Image
    mO = reshape(mO, [numRows, numCols]);


  [1]: https://www.mathworks.com/help/matlab/ref/accumarray.html

