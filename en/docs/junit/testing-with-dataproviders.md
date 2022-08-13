---
title: "Testing with DataProviders"
slug: "testing-with-dataproviders"
draft: false
images: []
weight: 9914
type: docs
toc: true
---

## Installation and usage
**Installation:**

In order to use DataProviders, you need junit-dataprovider .jar : 

[Github][1]


[Direct download][2]

Hamcrest-core-1.3.jar :

[Github][3]

[Direct download][4]

And add both of this .jar to your project.

**Usage:**

Add this `import` to your code:

    import com.tngtech.java.junit.dataprovider.DataProvider;
    import com.tngtech.java.junit.dataprovider.DataProviderRunner;
    import com.tngtech.java.junit.dataprovider.UseDataProvider;

Before the declaration of your class:

    @RunWith(DataProviderRunner.class)

So it looks like this:

    @RunWith(DataProviderRunner.class)
    public class example {
        //code
    }

**How to create DataProviders:**

Before whichever function you want it to be a DataProvider, add this decorator:

    @DataProvider

So it would look like this:


    @DataProvider
    public static Object[][] testExampleProvider() {
        return new Object[][]{
            {"param1", "param2", number1}
            {"param1", "param2", number1}
            //You can put as many parameters as you want
        };
    }

**How to use DataProviders:**

Before any function you want it to get those params that we return from the DataProvider, add this decorator:

    @UseDataProvider("testExampleProvider")

So your function to test looks like this:

    @Test
    @UseDataProvider("testExampleProvider")
    public void testAccount(String param1, String param2, int number) {
        //System.out.println("exampleOfDataProviders");
        //assertEquals(...);
        //assertEquals(...);
    }

  [1]: https://github.com/TNG/junit-dataprovider
  [2]: http://search.maven.org/#search%7Cga%7C1%7Ca%3A%22junit-dataprovider%22
  [3]: https://github.com/hamcrest/JavaHamcrest
  [4]: http://search.maven.org/#search%7Cga%7C1%7Cg%3Aorg.hamcrest

