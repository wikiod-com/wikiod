---
title: "Parametrized tests"
slug: "parametrized-tests"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Data providers
Data providers allow creating multiple test inputs to be run within a test. Let's consider a test which verifies that numbers are doubled correctly. To create data provider provide a static method which returns either `Object[][]` or `Iterator<Object[]>` (the latter allows for lazy computation of the test inputs) annotated with `@DataProvider` annotation, with property `name` being a unique string identifying the provider.

    import org.testng.annotations.DataProvider;
    
    public class DoublingDataProvider {
        public final static String DOUBLING_DATA_PROVIDER = "doublingDataProvider";
    
        @DataProvider(name = DOUBLING_DATA_PROVIDER)
        public static Object[][] doubling() {
            return new Object[][]{
                    new Object[]{1, 2},
                    new Object[]{2, 4},
                    new Object[]{3, 6}
            };
        }
    }

In the above case each `Object[]` represents a set of data for a single test case - here the number to be doubled, followed by the expected value after doubling.

To use the data provider fill the `dataProvider` property of the test with the name of the provider. If the provider method was defined outside of the test class or its base classes, you also have to specify the `dataProviderClass` property. The test method should take parameters corresponding to the elements of the test case description - here it's two ints.

    import org.testng.annotations.Test;
    
    import static org.testng.Assert.assertEquals;
    
    public class DoublingTest {
    
        @Test(dataProvider = DoublingDataProvider.DOUBLING_DATA_PROVIDER, dataProviderClass = DoublingDataProvider.class)
        public void testDoubling(int number, int expectedResult) {
            assertEquals(number * 2, expectedResult);
        }
    }

