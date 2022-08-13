---
title: "RSpec Core"
slug: "rspec-core"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Running examples with a given tag
Adding tags to "describe" or "it" blocks allows you to run only those examples with a given tag. Use the `--tag` (or `-t`) option to run examples that match a specified tag. The tag can be a simple name or a name:value pair.

- If a simple name is supplied, only examples with `:name => true` will run. For example, `rspec <spec_file> --tag smoke` would run the example tagged with "Smoke".

      describe '#Tests' do
        it 'runs the smoke test', :smoke => true do
        end
        
        it 'runs the regression tests', :regression => true do
        end
    
        it 'runs the acceptance tests', :acceptance => true do
        end
      end
 
- If a `name:value` pair is given, examples with `name => value` will run,where value is always a string. For example, `rspec <spec_file> --tag testId:101` would run the example tagged with `testId` "101".

      describe '#Tests' do
        it 'runs the test with id 99', :testId => 99 do
        end
    
        it 'runs the test with id 101', :testId => 101 do
        end
      end


