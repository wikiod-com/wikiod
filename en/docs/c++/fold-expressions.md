---
title: "Fold Expressions"
slug: "fold-expressions"
draft: false
images: []
weight: 9854
type: docs
toc: true
---

Fold Expressions are supported for the following operators

| &nbsp; |&nbsp;  |&nbsp;   | &nbsp;  | &nbsp;  |&nbsp;  |&nbsp;  |&nbsp;  |&nbsp; |&nbsp;  |&nbsp;  |&nbsp;  |
| ----- | ------| ------ | ------ | ------ |------ |------ |------ |------ |------ |------ |------ |
|\+ |\- |\* |\/ |\% |\ˆ |\& | \| | \<\<| \>\>| |
|\+\= |\-\= |\*\= |\/\= |\%\= |\ˆ\= |\&\= |\|\= |\<\<\=| \>\>\=| \=|
|\=\= |\!\= |\< |\> |\<\= |\>\= |\&\& | \|\| |\,| \.\*| \-\>\*|


When folding over an empty sequence, a fold expression is ill-formed, except for the following three operators:

| Operator| Value when parameter pack is empty|
| ------ | ------ |
| \&\&| true|
| \|\|| false|
| \,| void()|


## Unary Folds
Unary folds are used to *fold* [parameter packs][1] over a specific operator. There are 2 kinds of unary folds:

- Unary **Left** Fold  `(... op pack)` which expands as follows: 

      ((Pack1 op Pack2) op ...) op PackN

- Unary **Right** Fold  `(pack op ...)` which expands as follows: 

      Pack1 op (... (Pack(N-1) op PackN)) 

Here is an example

    template<typename... Ts>
    int sum(Ts... args)
    {
        return (... + args); //Unary left fold
        //return (args + ...); //Unary right fold

        // The two are equivalent if the operator is associative.
        // For +, ((1+2)+3) (left fold) == (1+(2+3)) (right fold)
        // For -, ((1-2)-3) (left fold) != (1-(2-3)) (right fold)
    }

    int result = sum(1, 2, 3); //  6


  [1]: https://www.wikiod.com/docs/c%2B%2B/7668/parameter-packs#t=201610282037074790782

## Binary Folds
Binary folds are basically [unary folds](https://www.wikiod.com/docs/c%2b%2b/2676/fold-expressions/8931/unary-folds#t=201608161845476575023), with an extra argument.

There are 2 kinds of binary folds:

- Binary **Left** Fold - `(value op ... op pack)` - Expands as follows:

      (((Value op Pack1) op Pack2) op ...) op PackN

- Binary **Right** Fold `(pack op ... op value)` - Expands as follows:

      Pack1 op (... op (Pack(N-1) op (PackN op Value)))

Here is an example:

    template<typename... Ts>
    int removeFrom(int num, Ts... args)
    {
        return (num - ... - args); //Binary left fold
        // Note that a binary right fold cannot be used
        // due to the lack of associativity of operator-
    }

    int result = removeFrom(1000, 5, 10, 15); //'result' is 1000 - 5 - 10 - 15 = 970

## Folding over a comma
It is a common operation to need to perform a particular function over each element in a parameter pack. With C++11, the best we can do is:

    template <class... Ts>
    void print_all(std::ostream& os, Ts const&... args) {
        using expander = int[];
        (void)expander{0,
            (void(os << args), 0)...
        };
    }

But with a fold expression, the above simplifies nicely to:

    template <class... Ts>
    void print_all(std::ostream& os, Ts const&... args) {
        (void(os << args), ...);
    }

No cryptic boilerplate required. 

