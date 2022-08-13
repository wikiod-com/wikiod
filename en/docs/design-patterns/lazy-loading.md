---
title: "lazy loading"
slug: "lazy-loading"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

eager loading is expensive or the object to be loaded might not be needed at all


## JAVA lazy loading
Call from main()

    // Simple lazy loader - not thread safe
    HolderNaive holderNaive = new HolderNaive();
    Heavy heavy = holderNaive.getHeavy();

**Heavy.class**    

    /**
     * 
     * Heavy objects are expensive to create.
     *
     */
    public class Heavy {
    
      private static final Logger LOGGER = LoggerFactory.getLogger(Heavy.class);
    
      /**
       * Constructor
       */
      public Heavy() {
        LOGGER.info("Creating Heavy ...");
        try {
          Thread.sleep(1000);
        } catch (InterruptedException e) {
          LOGGER.error("Exception caught.", e);
        }
        LOGGER.info("... Heavy created");
      }
    }

HolderNaive.class


    /**
     * 
     * Simple implementation of the lazy loading idiom. However, this is not thread safe.
     *
     */
    public class HolderNaive {
    
      private static final Logger LOGGER = LoggerFactory.getLogger(HolderNaive.class);
    
      private Heavy heavy;
    
      /**
       * Constructor
       */
      public HolderNaive() {
        LOGGER.info("HolderNaive created");
      }
    
      /**
       * Get heavy object
       */
      public Heavy getHeavy() {
        if (heavy == null) {
          heavy = new Heavy();
        }
        return heavy;
      }
    }



