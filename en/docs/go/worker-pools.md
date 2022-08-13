---
title: "Worker Pools"
slug: "worker-pools"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Job Queue with Worker Pool
A job queue that maintains a worker pool, useful for doing things like background processing in web servers:

    package main

    import (
      "fmt"
      "runtime"
      "strconv"
      "sync"
      "time"
    )

    // Job - interface for job processing
    type Job interface {
      Process()
    }

    // Worker - the worker threads that actually process the jobs
    type Worker struct {
      done             sync.WaitGroup
      readyPool        chan chan Job
      assignedJobQueue chan Job

      quit chan bool
    }

    // JobQueue - a queue for enqueueing jobs to be processed
    type JobQueue struct {
      internalQueue     chan Job
      readyPool         chan chan Job
      workers           []*Worker
      dispatcherStopped sync.WaitGroup
      workersStopped    sync.WaitGroup
      quit              chan bool
    }

    // NewJobQueue - creates a new job queue
    func NewJobQueue(maxWorkers int) *JobQueue {
      workersStopped := sync.WaitGroup{}
      readyPool := make(chan chan Job, maxWorkers)
      workers := make([]*Worker, maxWorkers, maxWorkers)
      for i := 0; i < maxWorkers; i++ {
        workers[i] = NewWorker(readyPool, workersStopped)
      }
      return &JobQueue{
        internalQueue:     make(chan Job),
        readyPool:         readyPool,
        workers:           workers,
        dispatcherStopped: sync.WaitGroup{},
        workersStopped:    workersStopped,
        quit:              make(chan bool),
      }
    }

    // Start - starts the worker routines and dispatcher routine
    func (q *JobQueue) Start() {
      for i := 0; i < len(q.workers); i++ {
        q.workers[i].Start()
      }
      go q.dispatch()
    }

    // Stop - stops the workers and sispatcher routine
    func (q *JobQueue) Stop() {
      q.quit <- true
      q.dispatcherStopped.Wait()
    }

    func (q *JobQueue) dispatch() {
      q.dispatcherStopped.Add(1)
      for {
        select {
        case job := <-q.internalQueue: // We got something in on our queue
          workerChannel := <-q.readyPool // Check out an available worker
          workerChannel <- job           // Send the request to the channel
        case <-q.quit:
          for i := 0; i < len(q.workers); i++ {
            q.workers[i].Stop()
          }
          q.workersStopped.Wait()
          q.dispatcherStopped.Done()
          return
        }
      }
    }

    // Submit - adds a new job to be processed
    func (q *JobQueue) Submit(job Job) {
      q.internalQueue <- job
    }

    // NewWorker - creates a new worker
    func NewWorker(readyPool chan chan Job, done sync.WaitGroup) *Worker {
      return &Worker{
        done:             done,
        readyPool:        readyPool,
        assignedJobQueue: make(chan Job),
        quit:             make(chan bool),
      }
    }

    // Start - begins the job processing loop for the worker
    func (w *Worker) Start() {
      go func() {
        w.done.Add(1)
        for {
          w.readyPool <- w.assignedJobQueue // check the job queue in
          select {
          case job := <-w.assignedJobQueue: // see if anything has been assigned to the queue
            job.Process()
          case <-w.quit:
            w.done.Done()
            return
          }
        }
      }()
    }

    // Stop - stops the worker
    func (w *Worker) Stop() {
      w.quit <- true
    }

    //////////////// Example //////////////////

    // TestJob - holds only an ID to show state
    type TestJob struct {
      ID string
    }

    // Process - test process function
    func (t *TestJob) Process() {
      fmt.Printf("Processing job '%s'\n", t.ID)
      time.Sleep(1 * time.Second)
    }

    func main() {
      queue := NewJobQueue(runtime.NumCPU())
      queue.Start()
      defer queue.Stop()

      for i := 0; i < 4*runtime.NumCPU(); i++ {
        queue.Submit(&TestJob{strconv.Itoa(i)})
      }
    }


## Simple worker pool
A simple worker pool implementation:
```
package main

import (
    "fmt"
    "sync"
)

type job struct {
    // some fields for your job type
}

type result struct {
    // some fields for your result type
}

func worker(jobs <-chan job, results chan<- result) {
    for j := range jobs {
        var r result
        // do some work
        results <- r
    }
}

func main() {
    // make our channels for communicating work and results
    jobs := make(chan job, 100) // 100 was chosen arbitrarily
    results := make(chan result, 100)
       
    // spin up workers and use a sync.WaitGroup to indicate completion
    wg := sync.WaitGroup
    for i := 0; i < runtime.NumCPU; i++ {
        wg.Add(1)
        go func() {
            defer wg.Done()
            worker(jobs, results)
        }()
    }
    
    // wait on the workers to finish and close the result channel
    // to signal downstream that all work is done
    go func() {
        defer close(results)
        wg.Wait()
    }()

    // start sending jobs
    go func() {
        defer close(jobs)
        for {
            jobs <- getJob()   // I haven't defined getJob() and noMoreJobs()
            if noMoreJobs() {  // they are just for illustration
                break
            }
        }
    }()

    // read all the results
    for r := range results {
        fmt.Println(r)
    }
}
```

