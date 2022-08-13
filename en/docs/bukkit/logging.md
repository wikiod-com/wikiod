---
title: "Logging"
slug: "logging"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Using the Bukkit Logger
    public class MyClass {
    
    public void foo() {
        Logger logger = Bukkit.getLogger();
        
        logger.info("A log message");
        logger.log(Level.INFO, "Another message");
        logger.fine("A fine message");
        
            // logging an exception
            try {
                // code might throw an exception
            } catch (SomeException ex) {
                // log a warning printing "Something went wrong"
                // together with the exception message and stacktrace
                logger.log(Level.WARNING, "Something went wrong", ex);
            }
    
            String s = "Hello World!";
    
            // logging an object
            LOG.log(Level.FINER, "String s: {0}", s);
    
            // logging several objects
            LOG.log(Level.FINEST, "String s: {0} has length {1}", new Object[]{s, s.length()});
        }
    
    }

## Logging Levels
Java Logging Api has 7 [levels][1]. The levels in descending order are:

- `SEVERE` (highest value)
- `WARNING`
- `INFO`
- `CONFIG`
- `FINE`
- `FINER`
- `FINEST` (lowest value)

The default level is `INFO` (but this depends on the system and used a virtual machine).

**Note**:
There are also levels `OFF` (can be used to turn logging off) and `ALL` (the oposite of `OFF`).

Code example for this:

    import java.util.logging.Logger;
    
    public class Levels {
        private static final Logger logger = Bukkit.getLogger();
    
        public static void main(String[] args) {

            logger.severe("Message logged by SEVERE");
            logger.warning("Message logged by WARNING");
            logger.info("Message logged by INFO");
            logger.config("Message logged by CONFIG");
            logger.fine("Message logged by FINE");
            logger.finer("Message logged by FINER");
            logger.finest("Message logged by FINEST");
    
            // All of above methods are really just shortcut for
            // public void log(Level level, String msg):
            logger.log(Level.FINEST, "Message logged by FINEST");
        }
    }

By default running this class will output only messages with level higher then `CONFIG`:

    Jul 23, 2016 9:16:11 PM LevelsExample main
    SEVERE: Message logged by SEVERE
    Jul 23, 2016 9:16:11 PM LevelsExample main
    WARNING: Message logged by WARNING
    Jul 23, 2016 9:16:11 PM LevelsExample main
    INFO: Message logged by INFO


  [1]: https://docs.oracle.com/javase/8/docs/api/java/util/logging/Level.html

