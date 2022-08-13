---
title: "WebService without Visual Studio"
slug: "webservice-without-visual-studio"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

A very basic ASP.Net example of the bare minimum of code to create a WebService.

In a separate StackOverflow Documentation post, we'll look at consuming this Calculator WebService.

## Calculator WebService
    <%@ WebService Language="C#" Class="Util" %>
    using System;
    using System.Web.Services;
    
    public class Util: WebService
    {
        [WebMethod]
        public int CalculatorAdd(int operandA, int operandB)
        {
            return operandA + operandB;
        }
    
        [WebMethod]
        public int CalculatorSubtract(int operandA, int operandB)
        {
            return operandA - operandB;
        }
    
        [WebMethod]
        public long CalculatorMultiply(int operandA, int operandB)
        {
            return operandA * operandB;
        }
        
        [WebMethod]
        public long CalculatorDivide(int operandNumerator, int operandDenominator)
        {
            if (operandDenominator == 0)
                return System.Int64.MaxValue;    // Should really do better error handling overall & return an error
            else
                return operandNumerator / operandDenominator;
        }
    }

