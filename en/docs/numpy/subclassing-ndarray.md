---
title: "subclassing ndarray"
slug: "subclassing-ndarray"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Syntax
* `def __array_prepare__(self, out_arr: ndarray, context: Tuple[ufunc, Tuple, int] = None) -> ndarray:  # called on the way into a ufunc`

* `def __array_wrap__(self, out_arr: ndarray, context: Tuple[ufunc, Tuple, int] = None) -> ndarray: # called on the way out of a ufunc`
* `__array_priority__: int  # used to determine which argument to invoke the above methods on when a ufunc is called`
* `def __array_finalize__(self, obj: ndarray):  # called whenever a new instance of this class comes into existence, even if this happens by routes other than __new__`

## Tracking an extra property on arrays
    class MySubClass(np.ndarray):
        def __new__(cls, input_array, info=None):
            obj = np.asarray(input_array).view(cls)
            obj.info = info
            return obj
    
        def __array_finalize__(self, obj):
            # handles MySubClass(...)
            if obj is None:
                pass

            # handles my_subclass[...] or my_subclass.view(MySubClass) or ufunc output
            elif isinstance(obj, MySubClass):
                self.info = obj.info

            # handles my_arr.view(MySubClass)
            else:
                self.info = None

        def __array_prepare__(self, out_arr, context=None):
            # called before a ufunc runs
            if context is not None:
                func, args, which_return_val = context

            return super().__array_prepare__(out_arr, context)

        def __array_wrap__(self, out_arr, context=None):
            # called after a ufunc runs
            if context is not None:
                func, args, which_return_val = context

            return super().__array_wrap__(out_arr, context)

For the `context` tuple, `func` is a ufunc object such as `np.add`, `args` is a `tuple`, and `which_return_val` is an integer specifying which return value of the ufunc is being processed

