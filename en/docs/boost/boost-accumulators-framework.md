---
title: "Boost Accumulators Framework"
slug: "boost-accumulators-framework"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Computing mean and variance
    #include <iostream>
    #include <boost/accumulators/accumulators.hpp>
    #include <boost/accumulators/statistics/stats.hpp>
    #include <boost/accumulators/statistics/mean.hpp>
    #include <boost/accumulators/statistics/variance.hpp>
       
    int main()
    {
      using namespace boost::accumulators;
      accumulator_set<int, stats<tag::mean, tag::variance>> acc;
    
      for(int i = 1; i <= 6; i++)
        acc(i);
    
      std::cout << "mean=" << mean(acc) << ", variance=" << variance(acc) << '\n';
      // prints "mean=3.5, variance=2.91667"
    
      return 0;
    }


