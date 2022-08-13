---
title: "Queue"
slug: "queue"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Syntax
- q = Queue.new
- q.push object
- q << object # same as #push
- q.pop #=> object


## Multiple Workers One Sink
We want to gather data created by multiple Workers.

First we create a Queue:

    sink = Queue.new

Then 16 workers all generating a random number and pushing it into sink:

    (1..16).to_a.map do
      Thread.new do
        sink << rand(1..100)
      end
    end.map(&:join)

And to get the data, convert a Queue to an Array:

    data = [].tap { |a| a << sink.pop until sink.empty? }

## Converting a Queue into an Array
    q = Queue.new
    q << 1
    q << 2
    
    a = Array.new
    a << q.pop until q.empty?

Or a [one liner](http://stackoverflow.com/a/34481350/2647317):

    [].tap { |array| array < queue.pop until queue.empty? }

## One Source Multiple Workers
We want to process data in parallel.

Let's populate source with some data:

    source = Queue.new
    data = (1..100)
    data.each { |e| source << e }

Then create some workers to process data:

    (1..16).to_a.map do
      Thread.new do
        until source.empty?
          item = source.pop
          sleep 0.5
          puts "Processed: #{item}"
        end
      end
    end.map(&:join)

## One Source - Pipeline of Work - One Sink
We want to process data in parallel and push it down the line to be processed by other workers.

Since Workers both consume and produce data we have to create two queues:

    first_input_source = Queue.new
    first_output_sink  = Queue.new
    100.times { |i| first_input_source << i }

First wave of workers read an item from `first_input_source`, process the item, and write results in `first_output_sink`:

    (1..16).to_a.map do
      Thread.new do
        loop do
          item = first_input_source.pop
          first_output_source << item ** 2
          first_output_source << item ** 3
        end
      end
    end

Second wave of workers uses `first_output_sink` as its input source and reads, process then writes to another output sink:

    second_input_source = first_output_sink
    second_output_sink  = Queue.new
    
    (1..32).to_a.map do
      Thread.new do
        loop do
          item = second_input_source.pop
          second_output_sink << item * 2
          second_output_sink << item * 3
        end
      end
    end

Now `second_output_sink` is the sink, let's convert it to an array:

    sleep 5 # workaround in place of synchronization
    sink = second_output_sink
    [].tap { |a| a << sink.pop until sink.empty? }


## Pushing Data into a Queue - #push
    q = Queue.new
    q << "any object including another queue"
    # or
    q.push :data

* There is no high water mark, queues can infinitely grow.
* `#push` never blocks

## Pulling Data from a Queue - #pop
    q = Queue.new
    q << :data
    q.pop #=> :data

* `#pop` will block until there is some data available.
* `#pop` can be used for synchronization.

## Synchronization - After a Point in Time
    syncer = Queue.new
    
    a = Thread.new do
      syncer.pop
      puts "this happens at end"
    end
    
    b = Thread.new do
      puts "this happens first"
      STDOUT.flush
      syncer << :ok
    end
    
    [a, b].map(&:join)

## Merging Two Queues
* To avoid infinitely blocking, reading from queues shouldn't happen on the thread merge is happening on.
* To avoid synchronization or infinitely waiting for one of queues while other has data, reading from queues shouldn't happen on same thread.

Let's start by defining and populating two queues:

    q1 = Queue.new
    q2 = Queue.new
    (1..100).each { |e| q1 << e }
    (101..200).each { |e| q2 << e }

We should create another queue and push data from other threads into it:

    merged = Queue.new
    
    [q1, q2].map do |q|
      Thread.new do
        loop do
          merged << q.pop
        end
      end
    end

If you know you can completely consume both queues (consumption speed is higher than production, you won't run out of RAM) there is a simpler approach:

    merged = Queue.new
    merged << q1.pop until q1.empty?
    merged << q2.pop until q2.empty?

