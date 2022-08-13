---
title: "ExUnit"
slug: "exunit"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Asserting Exceptions
Use `assert_raise` to test if an exception was raised. `assert_raise` takes in an Exception and a function to be executed.

      test "invalid block size" do
        assert_raise(MerkleTree.ArgumentError, (fn() -> MerkleTree.new ["a", "b", "c"] end))
      end

Wrap any code you want to test in an anonymous function and pass it to `assert_raise`.



