---
title: "Computational Tools"
slug: "computational-tools"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Find The Correlation Between Columns
Suppose you have a DataFrame of numerical values, for example:

    df = pd.DataFrame(np.random.randn(1000, 3), columns=['a', 'b', 'c'])

Then 

    >>> df.corr()
        a    b    c
    a    1.000000    0.018602    0.038098
    b    0.018602    1.000000    -0.014245
    c    0.038098    -0.014245    1.000000

will find the [Pearson correlation](https://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient) between the columns. Note how the diagonal is 1, as each column is (obviously) fully correlated with itself.

[`pd.DataFrame.correlation`](http://pandas.pydata.org/pandas-docs/stable/computation.html) takes an optional `method` parameter, specifying which algorithm to use. The default is `pearson`. To use Spearman correlation, for example, use

    >>> df.corr(method='spearman')
        a    b    c
    a    1.000000    0.007744    0.037209
    b    0.007744    1.000000    -0.011823
    c    0.037209    -0.011823    1.000000

