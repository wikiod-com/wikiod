---
title: "Dealing with categorical variables"
slug: "dealing-with-categorical-variables"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## One-hot encoding with `get_dummies()`
```python
>>> df = pd.DataFrame({'Name':['John Smith', 'Mary Brown'],
                     'Gender':['M', 'F'], 'Smoker':['Y', 'N']})
>>> print(df)
```
```
  Gender        Name Smoker
0      M  John Smith      Y
1      F  Mary Brown      N
```
```python
>>> df_with_dummies = pd.get_dummies(df, columns=['Gender', 'Smoker'])
>>> print(df_with_dummies)
```
```
         Name  Gender_F  Gender_M  Smoker_N  Smoker_Y
0  John Smith       0.0       1.0       0.0       1.0
1  Mary Brown       1.0       0.0       1.0       0.0
```

