---
title: "Timers"
slug: "timers"
draft: false
images: []
weight: 9936
type: docs
toc: true
---

## POSIX Timer with SIGEV_THREAD notification
This example demonstrates POSIX Timer usage with `CLOCK_REALTIME` clock and `SIGEV_THREAD` notification method.

<!-- language: c -->

    #include <stdio.h>  /* for puts() */
    #include <string.h> /* for memset() */
    #include <unistd.h> /* for sleep() */
    #include <stdlib.h> /* for EXIT_SUCCESS */
    
    #include <signal.h> /* for `struct sigevent` and SIGEV_THREAD */
    #include <time.h>   /* for timer_create(), `struct itimerspec`,
                         * timer_t and CLOCK_REALTIME 
                         */
    
    void thread_handler(union sigval sv) {
            char *s = sv.sival_ptr;
    
            /* Will print "5 seconds elapsed." */
            puts(s);
    }
    
    int main(void) {
            char info[] = "5 seconds elapsed.";
            timer_t timerid;
            struct sigevent sev;
            struct itimerspec trigger;
    
            /* Set all `sev` and `trigger` memory to 0 */
            memset(&sev, 0, sizeof(struct sigevent));
            memset(&trigger, 0, sizeof(struct itimerspec));
    
            /* 
             * Set the notification method as SIGEV_THREAD:
             *
             * Upon timer expiration, `sigev_notify_function` (thread_handler()),
             * will be invoked as if it were the start function of a new thread.
             *
             */
            sev.sigev_notify = SIGEV_THREAD;
            sev.sigev_notify_function = &thread_handler;
            sev.sigev_value.sival_ptr = &info;
    
            /* Create the timer. In this example, CLOCK_REALTIME is used as the
             * clock, meaning that we're using a system-wide real-time clock for
             * this timer.
             */
            timer_create(CLOCK_REALTIME, &sev, &timerid);
    
            /* Timer expiration will occur withing 5 seconds after being armed
             * by timer_settime().
             */
            trigger.it_value.tv_sec = 5;
    
            /* Arm the timer. No flags are set and no old_value will be retrieved.
             */
            timer_settime(timerid, 0, &trigger, NULL);
    
            /* Wait 10 seconds under the main thread. In 5 seconds (when the
             * timer expires), a message will be printed to the standard output
             * by the newly created notification thread.
             */
            sleep(10);
    
            /* Delete (destroy) the timer */
            timer_delete(timerid);
    
            return EXIT_SUCCESS;
    }



