---
title: "Getting started with xunit"
slug: "getting-started-with-xunit"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation and Setup (Getting Started)
Getting started with xUnit.net, on Platform:

 - .NET Core / ASP.NET Core
 - Desktop CLR
 - Universal Windows Apps



## A simple test written with xUnit.net
    using Xunit;

    namespace MyFirstUnitTests
    {
        public class TestClass
        {
            [Fact]
            public void PassingTest()
            {
                Assert.Equal(4, Add(2, 2));
            }
    
            [Fact]
            public void FailingTest()
            {
                Assert.Equal(5, Add(2, 2));
            }
    
            int Add(int x, int y)
            {
                return x + y;
            }
        }
    }

