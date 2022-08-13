---
title: "ActiveJob"
slug: "activejob"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Active Job is a framework for declaring jobs and making them run on a variety of queuing backends. These jobs can be everything from regularly scheduled clean-ups, to billing charges, to mailings. Anything that can be chopped up into small units of work and run in parallel, really.

## Create the Job

    class GuestsCleanupJob < ApplicationJob
      queue_as :default
     
      def perform(*guests)
        # Do something later
      end
    end

## Enqueue the Job
    # Enqueue a job to be performed as soon as the queuing system is free.
    GuestsCleanupJob.perform_later guest

