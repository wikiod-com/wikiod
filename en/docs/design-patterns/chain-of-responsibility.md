---
title: "Chain of Responsibility"
slug: "chain-of-responsibility"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Chain of Responsibility example (Php)
A method called in one object will move up the chain of objects until one is found that can properly handle the call. This particular example uses scientific experiments with functions that can just get the title of the experiment, the experiments id or the tissue used in the experiment.

    abstract class AbstractExperiment {
        abstract function getExperiment();
        abstract function getTitle();
    }
     
    class Experiment extends AbstractExperiment {
        private $experiment;
        private $tissue;
        function __construct($experiment_in) {
            $this->experiment = $experiment_in;
            $this->tissue = NULL;
        }
        function getExperiment() {
            return $this->experiment;
        }
        //this is the end of the chain - returns title or says there is none
        function getTissue() {
          if (NULL != $this->tissue) {
            return $this->tissue;
          } else {
            return 'there is no tissue applied';
          }
        }
    }
    
    class SubExperiment extends AbstractExperiment {
        private $experiment;
        private $parentExperiment;
        private $tissue;
        function __construct($experiment_in, Experiment $parentExperiment_in) {
          $this->experiment = $experiment_in;
          $this->parentExperiment = $parentExperiment_in;
          $this->tissue = NULL;
        }
        function getExperiment() {
          return $this->experiment;
        }
        function getParentExperiment() {
          return $this->parentExperiment;
        }   
        function getTissue() {
          if (NULL != $this->tissue) {
            return $this->tissue;
          } else {
            return $this->parentExperiment->getTissue();
          }
        }
    }
    
    //This class and all further sub classes work in the same way as SubExperiment above
    class SubSubExperiment extends AbstractExperiment {
        private $experiment;
        private $parentExperiment;
        private $tissue;
        function __construct($experiment_in, Experiment $parentExperiment_in) { //as above }
        function getExperiment() { //same as above }
        function getParentExperiment() { //same as above }   
        function getTissue() { //same as above }
    }



