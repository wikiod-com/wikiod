---
title: "Initializing Matrices or arrays"
slug: "initializing-matrices-or-arrays"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Matlab has three important functions to create matrices and set their elements to zeroes, ones, or the identity matrix.
(The identity matrix has ones on the main diagonal and zeroes elsewhere.)


## Syntax
 - Z = zeros(sz,datatype,arraytype)
 - X = ones(sz,datatype)
 - I = eye(sz,datatype)

## Parameters
| Parameter | Details |
| --------- | ------- |
| sz | n (for an n x n matrix) |
| sz | n, m (for an n x m matrix) |
| sz | m,n,...,k (for an m-by-n-by-...-by-k matrix) |
| datatype | 'double' (default), 'single', 'int8', 'uint8', 'int16', 'uint16', 'int32', 'uint32', 'int64', or 'uint64' |
| arraytype | 'distributed' |
| arraytype | 'codistributed' |
| arraytype | 'gpuArray' |

These functions will create a matrix of doubles, by default.

## Creating a matrix of 0s
    z1 = zeros(5); % Create a 5-by-5 matrix of zeroes
    z2 = zeros(2,3); % Create a 2-by-3 matrix

## Creating a matrix of 1s


    o1 = ones(5); % Create a 5-by-5 matrix of ones
    o2 = ones(1,3); % Create a 1-by-3 matrix / vector of size 3

## Creating an identity matrix


    i1 = eye(3); % Create a 3-by-3 identity matrix
    i2 = eye(5,6); % Create a 5-by-6 identity matrix

