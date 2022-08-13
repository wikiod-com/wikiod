---
title: "Simple Comparison of Weka Interfaces"
slug: "simple-comparison-of-weka-interfaces"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Weka has many interfaces, Explorer, KnowledgeFlow, Experimenter, SimpleCLI, Workbench. 

All of them share mostly can do the same tasks, with different focus and flexibility. 

Here, we are going to explore their different focuses and flexibilities.



**Explorer**   

pro:

1. do all things quickly
2. give a quick and comprehensive view of data structure

cos: can't save the process; 

**Experimenter**    

pro: 
1. compare several models at once, e.g., run 3 different classifiers against 5 datasets all together, and see the compared result at one place;     
2. experiment can be saved   

**KnowledgeFlow**   

pro: 
1. do almost all things that Explorer can do    
2. can save the process     

cos: 
1. KF can't do Experimenter's job, as it doesn't support loops, but [ADAMS](https://adams.cms.waikato.ac.nz/) can help;     
2. KF can't access low-level functionalities inside Weka API;

**simpleCLI**

pro: run similar tasks of what Explorer does using command line

cos: it can't access all functionalities of Weka API, Jython or Groovy scripting is recommended for this task.


**Workbench**

pro: it gathers all other interfaces together into one place

## simpleCLI and Jython examples
**simpleCLI**    

go to simpleCLI, enter the following code


    java weka.classifiers.rules.ZeroR -t path/to/a-file-of-dataset


**Jython Example**    

codes from [Advanced Weka MOOC course lesson 5.1](https://weka.waikato.ac.nz/advanceddataminingwithweka/unit?unit=5)

```
# imports
import weka.core.converters.ConverterUtils.DataSource as DS
import weka.filters.Filter as Filter
import weka.filters.unsupervised.attribute.Remove as Remove
import os

# load data
data = DS.read(os.environ.get("MOOC_DATA") + os.sep + "iris.arff")

# remove class attribute
rem = Remove()
rem.setOptions(["-R", "last"])
rem.setInputFormat(data)
dataNew = Filter.useFilter(data, rem)

# output filtered dataset
print(dataNew)
```


