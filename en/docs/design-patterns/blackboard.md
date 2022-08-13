---
title: "blackboard"
slug: "blackboard"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## C# Sample
Blackboard.cs


----------


    using System;
    using System.Collections.Generic;
    using System.Text;
    using System.Linq;
    
    
    namespace Blackboard
    {
        public class BlackBoard
        {
            public List<KnowledgeWorker> knowledgeWorkers;
            protected Dictionary<string, ControlData> data;
            public Control control;
    
    
            public BlackBoard()
            {
                this.knowledgeWorkers = new List<KnowledgeWorker>();
                this.control = new Control(this);
                this.data = new Dictionary<string, ControlData>();
            }
    
            public void addKnowledgeWorker(KnowledgeWorker newKnowledgeWorker) 
            {
                newKnowledgeWorker.blackboard = this;
                this.knowledgeWorkers.Add(newKnowledgeWorker);
            }       
    
            public Dictionary<string, ControlData> inspect()
            {
                return (Dictionary<string, ControlData>) this.data.ToDictionary(k => k.Key, k => (ControlData) k.Value.Clone());
            }
            public void update(KeyValuePair<string, ControlData> blackboardEntry) 
            {
                if (this.data.ContainsKey(blackboardEntry.Key))
                {
                    this.data[blackboardEntry.Key] = blackboardEntry.Value;
                }
                else
                    throw new InvalidOperationException(blackboardEntry.Key + " Not Found!");
            }
    
            public void update(string key, ControlData data)
            {
                if (this.data.ContainsKey(key))
                {
                    this.data[key] = data;
                }
                else
                {
                    this.data.Add(key, data);
                }            
            }
    
            public void print()
            {
                System.Console.WriteLine("Blackboard state");
                foreach (KeyValuePair<string, ControlData> cdata in this.data)
                {
                    Console.WriteLine(string.Format("data:{0}", cdata.Key));
                    Console.WriteLine(string.Format("\tProblem:{0}", cdata.Value.problem));
                    if(cdata.Value.input!=null)
                        Console.WriteLine(string.Format("\tInput:{0}", string.Join(",",cdata.Value.input)));
                    if(cdata.Value.output!=null)
                        Console.WriteLine(string.Format("\tOutput:{0}", string.Join(",",cdata.Value.output)));
                }
            }
    
        }
    }

Control.cs


----------

    using System;
    using System.Collections.Generic;
    
    
    namespace Blackboard
    {
        public class Control
        {
            BlackBoard blackBoard = null;
            
            public Control(BlackBoard blackBoard)
            {
                this.blackBoard = blackBoard;
            }
    
            public void loop()
            {
                System.Console.WriteLine("Starting loop");
                if (blackBoard == null)
                    throw new InvalidOperationException("blackboard is null");
                this.nextSource();
                System.Console.WriteLine("Loop ended");
                
            }
    
            /// <summary>
            /// Selects the next source of knowledge (knowledgeworker by inspecting the blackgoard)
            /// </summary>
            void nextSource()
            {
                // observers the blackboard
                foreach (KeyValuePair<string, ControlData> value in this.blackBoard.inspect())
                {
                    if (value.Value.problem == "PrimeNumbers")
                    {
                        foreach (KnowledgeWorker worker in this.blackBoard.knowledgeWorkers)
                        {
                            if (worker.getName() == "PrimeFinder")
                            {
                                Console.WriteLine("Knowledge Worker Found");
                                worker.executeCondition();
                                worker.executeAction();
                                worker.updateBlackboard();
                            }
                        }
                    }                
                }
            }
        }
    }

ControlData.cs


----------

    using System;
    using System.Collections.Generic;
    
    
    namespace Blackboard
    {
        public class ControlData:ICloneable
        {
            public string problem;
            public object[] input;
            public object[] output;
            public string updateby;
            public DateTime updated;
            
    
            public ControlData()
            {
                this.problem = null;
                this.input = this.output = null;
            }
    
            public ControlData(string problem, object[] input) 
            {
                this.problem = problem;
                this.input = input;
                this.updated = DateTime.Now;
            }
    
            public object getResult() 
            {
                return this.output;
            }
    
            public object Clone()
            {
                ControlData clone;
                clone = new ControlData(this.problem, this.input);
                clone.updated = this.updated;
                clone.updateby = this.updateby;
                clone.output = this.output;
                return clone;
            }
        }
    }

KnowledgeWorker.cs


----------


    using System; using System.Collections.Generic;
    
    namespace Blackboard {
        /// <summary>
        /// each knowledgeworker is resposible for knowing the conditions under which it can contribute to a solution.
        /// </summary>
        abstract public  class KnowledgeWorker
        {
            protected Boolean canContribute;        
            protected string Name;                  
            public BlackBoard blackboard = null;    
            protected List<KeyValuePair<string, ControlData>> keys;
            public KnowledgeWorker(BlackBoard blackboard, String Name)
            {
                this.blackboard = blackboard;
                this.Name = Name;
            }
    
            public KnowledgeWorker(String Name)
            {
                this.Name = Name;
            }
    
            public string getName() 
            {
                return this.Name;
            }
    
            abstract public void executeAction();
    
            abstract public void executeCondition();
    
            abstract public void updateBlackboard();
    
        } }



