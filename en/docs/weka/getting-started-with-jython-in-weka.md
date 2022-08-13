---
title: "Getting Started With Jython in Weka"
slug: "getting-started-with-jython-in-weka"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

**Why would we use Jython inside Weka?**      


1. If you are unsatisfied with what Explorer, Experimenter, KnowledgeFlow, simpleCLI allow you to do, and looking for something to unleash the greater power of weka;       
  
2. With Jython, we can access all functionalities provided by Weka API, right inside Weka;     

3. Its syntax is Python-like, which is considered to be a beginner-friendly scripting language;

# How to setup Jython in weka

1. install `Jython` and `JFreeChart` library from Weka Package manager; 
2. go to home directory's terminal, enter `nano .bash_profile`
3. inside `.bash_profile`, add a line of code as below

    `export Weka_Data=User/Documents/Directory/Of/Your/Data`
4. save and exit
5. inside terminal run `source .bash_profile`

Then, restart Weka, go to `tools` and click `Jython console`, and you can try those examples above



## Load and Filter Data
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



## Build a classifier
    # imports
    import weka.core.converters.ConverterUtils.DataSource as DS
    import weka.classifiers.trees.J48 as J48
    import os
    
    # load data
    data = DS.read(os.environ.get("MOOC_DATA") + os.sep + "anneal.arff")
    data.setClassIndex(data.numAttributes() - 1)
    
    # configure classifier
    cls = J48()
    cls.setOptions(["-C", "0.3"])
    
    # build classifier
    cls.buildClassifier(data)
    
    # output model
    print(cls)



## Cross-validate Classifier
    # imports
    import weka.core.converters.ConverterUtils.DataSource as DS
    import weka.classifiers.Evaluation as Evaluation
    import weka.classifiers.trees.J48 as J48
    import java.util.Random as Random
    import os
    
    # load data
    data = DS.read(os.environ.get("MOOC_DATA") + os.sep + "anneal.arff")
    data.setClassIndex(data.numAttributes() - 1)
    
    # configure classifier
    cls = J48()
    cls.setOptions(["-C", "0.3"])
    
    # cross-validate classifier
    evl = Evaluation(data)
    evl.crossValidateModel(cls, data, 10, Random(1))
    
    # print statistics
    print(evl.toSummaryString("=== J48 on anneal (stats) ===", False))
    print(evl.toMatrixString("=== J48 on anneal (confusion matrix) ==="))



## Make A Prediction
    # imports
    import weka.classifiers.trees.J48 as J48
    import weka.core.converters.ConverterUtils.DataSource as DS
    import os
    
    # load training data
    data = DS.read(os.environ.get("MOOC_DATA") + os.sep + "anneal_train.arff")
    data.setClassIndex(data.numAttributes() - 1)
    
    # configure classifier
    cls = J48()
    cls.setOptions(["-C", "0.3"])
    
    # build classifier on training data
    cls.buildClassifier(data)
    
    # load unlabeled data
    dataUnl = DS.read(os.environ.get("MOOC_DATA") + os.sep + "anneal_unlbl.arff")
    dataUnl.setClassIndex(dataUnl.numAttributes() - 1)
    
    # test compatibility of train/unlabeled datasets
    msg = dataUnl.equalHeadersMsg(data)
    if msg is not None:
        print("train and prediction data are not compatible:\n" + msg)
    
    # make predictions
    for inst in dataUnl:
        dist = cls.distributionForInstance(inst)
        labelIndex = cls.classifyInstance(inst)
        label = dataUnl.classAttribute().value(int(labelIndex))
        print(str(dist) + " - " + str(labelIndex) + " - " + label)



## Cross-validate Classifier Error Bubble
    # Note: install jfreechartOffscreenRenderer package as well for JFreeChart library
    
    # imports
    import weka.classifiers.Evaluation as Evaluation
    import weka.classifiers.functions.LinearRegression as LinearRegression
    import weka.core.converters.ConverterUtils.DataSource as DS
    import java.util.Random as Random
    import org.jfree.data.xy.DefaultXYZDataset as DefaultXYZDataset
    import org.jfree.chart.ChartFactory as ChartFactory
    import org.jfree.chart.plot.PlotOrientation as PlotOrientation
    import org.jfree.chart.ChartPanel as ChartPanel
    import org.jfree.chart.renderer.xy.XYBubbleRenderer as XYBubbleRenderer
    import org.jfree.chart.ChartUtilities as ChartUtilities
    import javax.swing.JFrame as JFrame
    import java.io.File as File
    import os
    
    # load data
    data = DS.read(os.environ.get("MOOC_DATA") + os.sep + "bodyfat.arff")
    data.setClassIndex(data.numAttributes() - 1)
    
    # configure classifier
    cls = LinearRegression()
    cls.setOptions(["-C", "-S", "1"])
    
    # cross-validate classifier
    evl = Evaluation(data)
    evl.crossValidateModel(cls, data, 10, Random(1))
    
    # collect predictions
    act = []
    prd = []
    err = []
    for i in range(evl.predictions().size()):
        prediction = evl.predictions().get(i)
        act.append(prediction.actual())
        prd.append(prediction.predicted())
        err.append(abs(prediction.actual() - prediction.predicted()))
        
    # create plot
    plotdata = DefaultXYZDataset()
    plotdata.addSeries("LR on " + data.relationName(), [act, prd, err])
    plot = ChartFactory.createScatterPlot(\
        "Classifier errors", "Actual", "Predicted", \
        plotdata, PlotOrientation.VERTICAL, True, True, True)
    plot.getPlot().setRenderer(XYBubbleRenderer())
    
    # display plot
    frame = JFrame()
    frame.setTitle("Weka")
    frame.setSize(800, 800)
    frame.setLocationRelativeTo(None)
    frame.getContentPane().add(ChartPanel(plot))
    frame.setVisible(True)



## Display Graph
    # imports
    import weka.classifiers.bayes.BayesNet as BayesNet
    import weka.core.converters.ConverterUtils.DataSource as DS
    import weka.gui.graphvisualizer.GraphVisualizer as GraphVisualizer
    import javax.swing.JFrame as JFrame
    import os
    
    # load data
    data = DS.read(os.environ.get("MOOC_DATA") + os.sep + "iris.arff")
    data.setClassIndex(data.numAttributes() - 1)
    
    # configure classifier
    cls = BayesNet()
    cls.setOptions(["-Q", "weka.classifiers.bayes.net.search.local.K2", "--", "-P", "2"])
    
    # build classifier
    cls.buildClassifier(data)
    
    # display tree
    gv = GraphVisualizer()
    gv.readBIF(cls.graph())
    frame = JFrame("BayesNet - " + data.relationName())
    frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE)
    frame.setSize(800, 600)
    frame.getContentPane().add(gv)
    frame.setVisible(True)
        
    # adjust tree layout
    gv.layoutGraph()



