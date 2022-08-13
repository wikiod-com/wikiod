---
title: "RSpec Expectations"
slug: "rspec-expectations"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

RSpec::Expectations lets you express expected outcomes on an object using an example-based DSL syntax.

This topic gives examples of how to expect test results in RSpec using `expect .to` and the many built-in matchers.

This functionality is provided by [the rspec-expectations gem][1].


  [1]: https://github.com/rspec/rspec-expectations/

## Basic Usage
Given a `class` as follows:

    class Cube
      attr_reader :height, :width, :depth

      def initialize(args)
        @height = args[:height] || args[:y] || 1
        @width  = args[:width]  || args[:x] || 1
        @depth  = args[:depth]  || args[:z] || 1
      end

      def volume
        height * width * depth
      end
    end

The following example passes if `cube.volume` equals 60 and fails if it doesn't. It uses the most commonly used built-in matcher, `eq`, which just tests for equality.

    RSpec.describe Cube do
      it "calculates it's volume" do
        cube = Cube.new(x: 3, y: 4, z: 5)
        expect(cube.volume).to eq(60)
      end
    end


