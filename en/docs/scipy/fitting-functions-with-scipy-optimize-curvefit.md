---
title: "Fitting functions with scipy.optimize curve_fit"
slug: "fitting-functions-with-scipyoptimize-curve_fit"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

Fitting a function which describes the expected occurence of data points to real data is often required in scientific applications. A possible optimizer for this task is curve_fit from scipy.optimize. In the following, an example of application of curve_fit is given. 

## Fitting a function to data from a histogram
Suppose there is a peak of normally (gaussian) distributed data (mean: 3.0, standard deviation: 0.3) in an exponentially decaying background. This distribution can be fitted with curve_fit within a few steps: 

1.) Import the required libraries. 

2.) Define the fit function that is to be fitted to the data.

3.) Obtain data from experiment or generate data. In this example, random data is generated in order to simulate the background and the signal.

4.) Add the signal and the background. 

5.) Fit the function to the data with curve_fit.

6.) (Optionally) Plot the results and the data.


In this example, the observed y values are the heights of the histogram bins, while the observed x values are the centers of the histogram bins (`binscenters`). It is necessary to pass the name of the fit function, the x values and the y values to `curve_fit`. Furthermore, an optional argument containing rough estimates for the fit parameters can be given with `p0`. `curve_fit` returns `popt` and `pcov`, where `popt` contains the fit results for the parameters, while `pcov` is the covariance matrix, the diagonal elements of which represent the variance of the fitted parameters. 

    # 1.) Necessary imports.    
    import numpy as np
    import matplotlib.pyplot as plt
    from scipy.optimize import curve_fit
    
    # 2.) Define fit function.
    def fit_function(x, A, beta, B, mu, sigma):
        return (A * np.exp(-x/beta) + B * np.exp(-1.0 * (x - mu)**2 / (2 * sigma**2)))
    
    # 3.) Generate exponential and gaussian data and histograms.
    data = np.random.exponential(scale=2.0, size=100000)
    data2 = np.random.normal(loc=3.0, scale=0.3, size=15000)
    bins = np.linspace(0, 6, 61)
    data_entries_1, bins_1 = np.histogram(data, bins=bins)
    data_entries_2, bins_2 = np.histogram(data2, bins=bins)
    
    # 4.) Add histograms of exponential and gaussian data.
    data_entries = data_entries_1 + data_entries_2
    binscenters = np.array([0.5 * (bins[i] + bins[i+1]) for i in range(len(bins)-1)])
    
    # 5.) Fit the function to the histogram data.
    popt, pcov = curve_fit(fit_function, xdata=binscenters, ydata=data_entries, p0=[20000, 2.0, 2000, 3.0, 0.3])
    print(popt)
    
    # 6.)
    # Generate enough x values to make the curves look smooth.
    xspace = np.linspace(0, 6, 100000)
    
    # Plot the histogram and the fitted function.
    plt.bar(binscenters, data_entries, width=bins[1] - bins[0], color='navy', label=r'Histogram entries')
    plt.plot(xspace, fit_function(xspace, *popt), color='darkorange', linewidth=2.5, label=r'Fitted function')
    
    # Make the plot nicer.
    plt.xlim(0,6)
    plt.xlabel(r'x axis')
    plt.ylabel(r'Number of entries')
    plt.title(r'Exponential decay with gaussian peak')
    plt.legend(loc='best')
    plt.show()
    plt.clf()

[![Fit to histogram data][1]][1]


  [1]: https://i.stack.imgur.com/8R4AY.png

