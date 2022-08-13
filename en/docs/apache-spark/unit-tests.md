---
title: "Unit tests"
slug: "unit-tests"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## Word count unit test (Scala + JUnit)
For example we have `WordCountService` with `countWords` method:

    class WordCountService {
        def countWords(url: String): Map[String, Int] = {
            val sparkConf = new SparkConf().setMaster("spark://somehost:7077").setAppName("WordCount"))
            val sc = new SparkContext(sparkConf)
            val textFile = sc.textFile(url)
            textFile.flatMap(line => line.split(" "))
                    .map(word => (word, 1))
                    .reduceByKey(_ + _).collect().toMap
        }
    }

This service seems very ugly and not adapted for unit testing. SparkContext should be injected to this service. It can be reached with your favourite DI framework but for simplicity it will be implemented using constructor:

    class WordCountService(val sc: SparkContext) {
        def countWords(url: String): Map[String, Int] = {
            val textFile = sc.textFile(url)
            textFile.flatMap(line => line.split(" "))
                    .map(word => (word, 1))
                    .reduceByKey(_ + _).collect().toMap
        }
    }

Now we can create simple JUnit test and inject testable sparkContext to WordCountService:

    class WordCountServiceTest {
        val sparkConf = new SparkConf().setMaster("local[*]").setAppName("WordCountTest")
        val testContext = new SparkContext(sparkConf)
        val wordCountService = new WordCountService(testContext)
    
        @Test
        def countWordsTest() {
            val testFilePath = "file://my-test-file.txt"
    
            val counts = wordCountService.countWords(testFilePath)
    
            Assert.assertEquals(counts("dog"), 121)
            Assert.assertEquals(counts("cat"), 191)
        }
    }



