---
title : caffe Tutorial
slug : caffe-tutorial
weight : 9945
draft : false
images : []
type : docs
---

Caffe is a library written in C++, to facilitate the experimentation with and use of Convolutional Neural Networks (CNN). Caffe has been developed by Berkeley Vision and Learning Center (BVLC). 

Caffe is actually an abbreviation referring to "Convolutional Architectures for Fast Feature Extraction". This acronym encapsulates an important scope of the library. Caffe in the form of a library offers a general programming framework/architecture which can be used to perform efficient training and testing of CNNs. "Efficiency" is a major hallmark of caffe, and stands as a major design objective of Caffe.

Caffe is an open-source library released under [BSD 2 Clause license.][1]

Caffe is maintained on [GitHub][2]

Caffe can be used to  :

 - Efficiently train and test multiple CNN architectures, specifically any architecture that can be represented as a directed acyclic graph (DAG).
 - Utilize multiple GPUs (upto 4) for training and testing. It is recommended that all the GPUs should be of the same type. Otherwise, performance is limited by the limits of the slowest GPU in the system. For example, in case of TitanX and GTX 980, the performance will be limited by the latter. Mixing multiple architectures is not supported, e.g. Kepler and Fermi [3].

Caffe has been written following efficient Object Oriented Programming (OOP) principles. 

A good starting point to begin an introduction to caffe is to get a bird's eye view of how caffe works through its fundamental objects.


  [1]: https://github.com/BVLC/caffe/blob/master/LICENSE
  [2]: https://github.com/BVLC/caffe
  [3]: https://github.com/BVLC/caffe/blob/master/docs/multigpu.md

