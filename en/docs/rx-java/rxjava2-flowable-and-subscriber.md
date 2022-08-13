---
title: "RxJava2 Flowable and Subscriber"
slug: "rxjava2-flowable-and-subscriber"
draft: false
images: []
weight: 9907
type: docs
toc: true
---

This topic shows examples and documentation with regard to the reactive concepts of Flowable and Subscriber that were introduced in rxjava version2

the example needs rxjava2 as a dependency, the maven coordinates for the used version are:

        <dependency>
            <groupId>io.reactivex.rxjava2</groupId>
            <artifactId>rxjava</artifactId>
            <version>2.0.8</version>
        </dependency>


## producer consumer example with backpressure support in the producer
The `TestProducer`from this example produces `Integer`objects in a given range and pushes them to its `Subscriber`. It extends the `Flowable<Integer>` class. For a new subscriber, it creates a `Subscription` object whose `request(long)` method is used to create and publish the Integer values.

It is important for the `Subscription` that is passed to the `subscriber` that the `request()` method which calls `onNext()`on the subscriber can be recursively called from within this `onNext()` call. To prevent a stack overflow, the shown implementation uses the `outStandingRequests` counter and the `isProducing` flag.

    class TestProducer extends Flowable<Integer> {
        static final Logger logger = LoggerFactory.getLogger(TestProducer.class);
        final int from, to;
    
        public TestProducer(int from, int to) {
            this.from = from;
            this.to = to;
        }
    
        @Override
        protected void subscribeActual(Subscriber<? super Integer> subscriber) {
            subscriber.onSubscribe(new Subscription() {
    
                /** the next value. */
                public int next = from;
                /** cancellation flag. */
                private volatile boolean cancelled = false;
                private volatile boolean isProducing = false;
                private AtomicLong outStandingRequests = new AtomicLong(0);
    
                @Override
                public void request(long n) {
                    if (!cancelled) {
    
                        outStandingRequests.addAndGet(n);
    
                        // check if already fulfilling request to prevent call  between request() an subscriber .onNext()
                        if (isProducing) {
                            return;
                        }
    
                        // start producing
                        isProducing = true;
    
                        while (outStandingRequests.get() > 0) {
                            if (next > to) {
                                logger.info("producer finished");
                                subscriber.onComplete();
                                break;
                            }
                            subscriber.onNext(next++);
                            outStandingRequests.decrementAndGet();
                        }
                        isProducing = false;
                    }
                }
    
                @Override
                public void cancel() {
                    cancelled = true;
                }
            });
        }
    }

The Consumer in this example extends `DefaultSubscriber<Integer>` and on start and after consuming an Integer requests the next one. On consuming the Integer values, there is a little delay, so the backpressure will be built up for the producer.

    class TestConsumer extends DefaultSubscriber<Integer> {
    
        private static final Logger logger = LoggerFactory.getLogger(TestConsumer.class);
    
        @Override
        protected void onStart() {
            request(1);
        }
    
        @Override
        public void onNext(Integer i) {
            logger.info("consuming {}", i);
            if (0 == (i % 5)) {
                try {
                    Thread.sleep(500);
                } catch (InterruptedException ignored) {
                    // can be ignored, just used for pausing
                }
            }
            request(1);
        }
    
        @Override
        public void onError(Throwable throwable) {
            logger.error("error received", throwable);
        }
    
        @Override
        public void onComplete() {
            logger.info("consumer finished");
        }
    }

in the following main method of a test class the producer and consumer are created and wired up:

    public static void main(String[] args) {
        try {
            final TestProducer testProducer = new TestProducer(1, 1_000);
            final TestConsumer testConsumer = new TestConsumer();

            testProducer
                    .subscribeOn(Schedulers.computation())
                    .observeOn(Schedulers.single())
                    .blockingSubscribe(testConsumer);

        } catch (Throwable t) {
            t.printStackTrace();
        }
    }

When running the example, the logfile shows that the consumer runs continuously, while the producer only gets active when the internal Flowable buffer of rxjava2 needs to be refilled.

