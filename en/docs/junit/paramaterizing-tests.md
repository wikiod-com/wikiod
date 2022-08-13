---
title: "Paramaterizing Tests"
slug: "paramaterizing-tests"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Sometimes you have a test you need to run multiple times, each time with different data. Parameterizing the test allows you to do this in an easy and maintainable way. 

## Syntax
 - @RunWith(Parameterized.class) //annotation for test class 
   
   @Parameters//annotation for data

One benefit to using parameters is that if one set of data fails, execution will just move to the next set of data instead of stopping the whole test. 

## Using a Constructor
    import static org.junit.Assert.assertThat;
    import static org.hamcrest.CoreMatchers.is;
    import java.util.*;
    import org.junit.*;
    import org.junit.runner.RunWith;
    import org.junit.runners.Parameterized;
    import org.junit.runners.Parameterized.Parameters;
    
    @RunWith(Parameterized.class)
    public class SimpleParmeterizedTest {
        @Parameters
        public static Collection<Object[]> data(){
            return Arrays.asList(new Object[][]{
                    {5, false}, {6, true}, {8, true}, {11, false}    
            });
        }
        
        private int input;
        private boolean expected;
        
        public SimpleParmeterizedTest(int input, boolean expected){
            this.input = input;
            this.expected = expected;
        }
        
        @Test
        public void testIsEven(){
            assertThat(isEven(input), is(expected));
        }
    }

In data() you supply the data to be used in the tests. Junit will iterate through the data and run the test with each set of data. 

