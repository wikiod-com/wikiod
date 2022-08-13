---
title: "RSpec Matcher"
slug: "rspec-matcher"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

rspec-expectations ships with a number of built-in matchers. Each matcher can be used
with expect(..).to or expect(..).not_to to define positive and negative expectations
respectively on an object. 

## Equality matchers
**compare using eq `(==)`**

    RSpec.describe "a string" do
      it "is equal to another string of the same value" do
        expect("this string").to eq("this string")
      end

      it "is not equal to another string of a different value" do
        expect("this string").not_to eq("a different string")
      end
    end

    RSpec.describe "an integer" do
      it "is equal to a float of the same value" do
        expect(5).to eq(5.0)
      end
    end
When I run `rspec` then the output should contain "3 examples, 0 failures" 

**compare using `==`** 

    RSpec.describe "a string" do
      it "is equal to another string of the same value" do
        expect("this string").to be == "this string"
      end

      it "is not equal to another string of a different value" do
        expect("this string").not_to be == "a different string"
      end
    end

    RSpec.describe "an integer" do
      it "is equal to a float of the same value" do
        expect(5).to be == 5.0
      end
    end
When I run `rspec` then the output should contain "3 examples, 0 failures" 

**compare using `eql (eql?)`**

     RSpec.describe "an integer" do
      it "is equal to another integer of the same value" do
        expect(5).to eql(5)
      end
    
      it "is not equal to another integer of a different value" do
        expect(5).not_to eql(6)
      end
    
      it "is not equal to a float of the same value" do
        expect(5).not_to eql(5.0)
      end
    end
When I run `rspec` then the output should contain "3 examples, 0 failures"

**compare using `equal (equal?)`**

    RSpec.describe "a string" do
      it "is equal to itself" do
        string = "this string"
        expect(string).to equal(string)
      end
    
      it "is not equal to another string of the same value" do
        expect("this string").not_to equal("this string")
      end
    
      it "is not equal to another string of a different value" do
        expect("this string").not_to equal("a different string")
      end
    end
When I run `rspec` then the output should contain "3 examples, 0 failures"

**compare using be `(equal?)`**

    RSpec.describe "a string" do
      it "is equal to itself" do
        string = "this string"
        expect(string).to be(string)
      end
    
      it "is not equal to another string of the same value" do
        expect("this string").not_to be("this string")
      end
    
      it "is not equal to another string of a different value" do
        expect("this string").not_to be("a different string")
      end
    end
 When I run `rspec` then the output should contain "3 examples, 0 failures"   
    

