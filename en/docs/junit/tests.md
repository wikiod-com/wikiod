---
title: "Tests"
slug: "tests"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

| Parameter | Context | Details  
| --------- | ------- | ------- |
| @BeforeClass | Static | Executed when the class is first created |
| @Before | Instance | Executed before _each test_ in the class |
| @Test | Instance | Should be declared each method to test |
| @After | Instance | Executed after _each test_ in the class |
| @AfterClass | Static | Executed before destruction of the class |

**Example Test Class Format**

    public class TestFeatureA {
    
        @BeforeClass
        public static void setupClass() {}
        
        @Before
        public void setupTest() {}
        
        @Test
        public void testA() {}
        
        @Test
        public void testB() {}
        
        @After
        public void tearDownTest() {}
        
        @AfterClass
        public static void tearDownClass() {}
        
        }
    }

## Performance measurement
If you need to check if your testing method takes too long to execute, you can do that by mentioning your expected execution time using timeout property of @Test annotation. If the test execution takes longer than that number of milliseconds it causes a test method to fail.

    public class StringConcatenationTest {

        private static final int TIMES = 10_000;
        
        // timeout in milliseconds
        @Test(timeout = 20)
        public void testString(){

            String res = "";

            for (int i = 0; i < TIMES; i++) {
                res += i;
            }

            System.out.println(res.length());
        }
    
        @Test(timeout = 20)
        public void testStringBuilder(){

            StringBuilder res = new StringBuilder();

            for (int i = 0; i < TIMES; i++) {
                res.append(i);
            }

            System.out.println(res.length());
        }
    
        @Test(timeout = 20)
        public void testStringBuffer(){

            StringBuffer res = new StringBufferr();

            for (int i = 0; i < TIMES; i++) {
                res.append(i);
            }

            System.out.println(res.length());
        }

    }

In most cases without [JVM warm-up][1] `testString` will fail.
But `testStringBuffer` and `testStringBuilder` should pass this test successfully.


  [1]: http://stackoverflow.com/q/36198278/5091346

## Unit testing using JUnit


## Fixtures


## Unit testing using Theories


