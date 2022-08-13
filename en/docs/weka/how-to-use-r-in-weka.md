---
title: "How to use R in Weka"
slug: "how-to-use-r-in-weka"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

# **Why use R in Weka?** 
1. R is a powerful tool for preprocessing data 
2. R has a huge number of libraries and keeps growing    
3. R in Weka, can easily get data from, process it, and pass to Weka seamlessly     

----

# **How to setup R in Weka**
**For Mac User**    
1. replace the old info.Plist with [the new one](https://drive.google.com/file/d/0B5yvejrCQ2QSVnZHWUVGRXhZMVU/view?usp=drive_web) given by Mark Hall
2. [download](https://www.r-project.org/) and install R     
2. install `rJava` inside R with 

    install.packages('rJava')

3. install `Rplugin` with `Weka Package Manager`    
4. go to `weka 3-8-0` folder (if it is the version you are using), and open its terminal, and     
5. run the following 2 lines of codes (thanks to Michael Hall)     

    export R_HOME=/Library/Frameworks/R.framework/Resources    
    java -Xss10M -Xmx4096M -cp .:weka.jar weka.gui.GUIChooser

6. to make life easier, inside a directory where you want to work with weka, save the code above into a file named as `weka_r.sh`    
7. make it executable, inside this directory's terminal, run the code below:    

    chmod a+x weka_r.sh
8. paste `weka.jar` from weka 3-8-0 into the directory and run the code below:    

    ./weka_r.sh

Now, you are ready to go. Next time, you just need to go to the directory's terminal and run `./weka_r.sh` to start R with Weka. 

----

# **How to receive data from Weka?**     
 
**open Weka from terminal**:     
go to directory of `Weka 3-8-0`, open its terminal, run the following code: 

    java -jar weka.jar





**data through Weka Explorer**:     
1. `preprocess` panel, click `open file`, choose a data file from `weka data folder`;   
2. go to `R console` panel, type R scripts inside `R console box`.

**data through Weka KnowledgeFlow**: 
1. `Data mining processes` panel, click `DataSources` to choose `ArffLoader` for example, click it onto canvas; 
2. double-click `ArffLoader` to load a data file   
3. `Scripting` panel, click `RscriptExecutor` onto canvas
4. `option` + click `ArffLoader`, select `dataset`, then click `RScript Executor` to link them
5. double click `RScript Executor` to type R script, or 
6. click `Settings` and select `R Scripting` to use R console with weka's data 


----
# **Playing R Codes** 
1. load `iris.arff` with either Explorer or KnowledgeFlow; 
2. try `Plotting inside R Console` example above


   


  




## Plotting inside R Console
> The following Codes can be found from [Weka course](https://weka.waikato.ac.nz/advanceddataminingwithweka/unit?unit=3&lesson=3)    

Given `iris.arff` is loaded in weka, inside Weka Explorer's `R console` or Weka KnowledgeFlow's `R Scripting`, you can play with the following codes to make beautiful plots: 

    library(ggplot2)
    
    ggplot(rdata, aes(x = petallength)) + geom_density()
    
    ggplot(rdata, aes(x = petallength)) + geom_density() + xlim(0,8)
    
    ggplot(rdata, aes(x = petallength)) + geom_density(adjust = 0.5) + xlim(0,8)
    
    ggplot(rdata, aes(x = petallength, color = class)) + geom_density(adjust = 0.5) + xlim(0,8)
    
    ggplot(rdata, aes(x = petallength, color = class, fill = class)) + geom_density(adjust = 0.5) + xlim(0,8)
    
    ggplot(rdata, aes(x = petallength, color = class, fill = class)) + geom_density(adjust = 0.5, alpha = 0.5) + xlim(0,8)





    library(reshape2)
    ndata = melt(rdata)
    ndata
    
    ggplot(ndata, aes(x = value, color = class, fill = class)) + geom_density(adjust = 0.5, alpha = 0.5) + xlim(0,8) + facet_grid(variable ~ .)
    
    ggplot(ndata, aes(x = value, color = class, fill = class)) + geom_density(adjust = 0.5, alpha = 0.5) + xlim(0,8) + facet_grid(. ~ variable)

    ggplot(ndata, aes(y = value, x = class, colour = class)) + geom_boxplot() + facet_grid(. ~ variable)



