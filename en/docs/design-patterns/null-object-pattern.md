---
title: "Null Object pattern"
slug: "null-object-pattern"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

Null Object is an object with no referenced value or with defined neutral behaviour. Its purpose is to remove the need of null pointer/reference check.

## Null Object Pattern (C++)
Assuming a abstract class:

    class ILogger {
        virtual ~ILogger() = default;
        virtual Log(const std::string&) = 0;
    };
    
Instead of

    void doJob(ILogger* logger) {
        if (logger) {
             logger->Log("[doJob]:Step 1");
        }
        // ...
        if (logger) {
             logger->Log("[doJob]:Step 2");
        }
        // ...
        if (logger) {
             logger->Log("[doJob]:End");
        }
    }

    void doJobWithoutLogging()
    {
        doJob(nullptr);
    }


You may create a Null Object Logger:

    class NullLogger : public ILogger
    {
        void Log(const std::string&) override { /* Empty */ }
    };

and then change `doJob` in the following:

    void doJob(ILogger& logger) {
        logger.Log("[doJob]:Step1");
        // ...
        logger.Log("[doJob]:Step 2");
        // ...
        logger.Log("[doJob]:End");
    }

    void doJobWithoutLogging()
    {
        NullLogger logger;
        doJob(logger);
    }


## Null Object Java using enum
Given an interface:

    public interface Logger {
       void log(String message);
    }

Rather than usage:

    public void doJob(Logger logger) {
        if (logger != null) {
           logger.log("[doJob]:Step 1");
        }
        // ...
        if (logger != null) {
           logger.log("[doJob]:Step 2");
        }
        // ...
        if (logger != null) {
           logger.log("[doJob]:Step 3");
        }
    }
    
    public void doJob() {
        doJob(null); // Without Logging
    }

Because null objects have no state, it makes sense to use a enum singleton for it, so given a null object implemented like so:

    public enum NullLogger implements Logger {
        INSTANCE;
    
        @Override
        public void log(String message) {
            // Do nothing
        }
    }

You can then avoid the null checks.

    public void doJob(Logger logger) {
        logger.log("[doJob]:Step 1");
        // ...
        logger.log("[doJob]:Step 2");
        // ...
        logger.log("[doJob]:Step 3");
    }
    
    public void doJob() {
        doJob(NullLogger.INSTANCE);
    }


