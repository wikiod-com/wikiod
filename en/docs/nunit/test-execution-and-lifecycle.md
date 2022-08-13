---
title: "Test execution and lifecycle"
slug: "test-execution-and-lifecycle"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Executing tests in a given order
Normally your tests should be created in such a way that execution order is no concern. However there is always going to be an edge case were you need to break that rule.

The one scenario I came across was with R.NET whereby in a given process you can only initialize one R Engine and once disposed you cannot reinitialize. One of my test happened to deal with disposing the engine and if this test were to run before any other test(s) they would fail.

You will find below a code snippet of how I managed to get this to run in order using Nunit.


    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Reflection;
    using NUnit.Framework;
    using RSamples;
     
    public class OrderedTestAttribute : Attribute
    {
        public int Order { get; set; }
 
        public OrderedTestAttribute(int order)
        {
            this.Order = order;
        }
    }
 
    public class TestStructure
    {
        public Action Test;
    }
 
    public class SampleTests
    {
        [TearDown]
        public void CleanUpAfterTest()
        {
            REngineExecutionContext.ClearLog();
        }
 
        [OrderedTest(0)]
        public void Test1(){}
 
        [OrderedTest(1)]
        public void Test2(){}
 
        [OrderedTest(2)]
        public void Test3(){}
 
        [TestCaseSource(sourceName: "TestSource")]
        public void MainTest(TestStructure test)
        {
            test.Test();
        }
 
        public static IEnumerable<TestCaseData> TestSource
        {
            get
            {
                var assembly = Assembly.GetExecutingAssembly();
                Dictionary<int, List<MethodInfo>> methods = assembly
                    .GetTypes()
                    .SelectMany(x => x.GetMethods())
                    .Where(y => y.GetCustomAttributes().OfType<OrderedTestAttribute>().Any())
                    .GroupBy(z => z.GetCustomAttribute<OrderedTestAttribute>().Order)
                    .ToDictionary(gdc => gdc.Key, gdc => gdc.ToList());
 
                foreach (var order in methods.Keys.OrderBy(x => x))
                {
                    foreach (var methodInfo in methods[order])
                    {
                        MethodInfo info = methodInfo;
                        yield return new TestCaseData(
                            new TestStructure
                            {
                                Test = () =>
                                {
                                    object classInstance = Activator.CreateInstance(info.DeclaringType, null);
                                    info.Invoke(classInstance, null);
                                }
                            }).SetName(methodInfo.Name);
                    }
                }
            }
        }
    }

