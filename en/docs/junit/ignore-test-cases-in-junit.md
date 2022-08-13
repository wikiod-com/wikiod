---
title: "Ignore test cases in Junit"
slug: "ignore-test-cases-in-junit"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Sometimes you want to ignore some of the test cases you have in Junit. For instance, they are partially done and you want to come back to them later. 

## Ignore test case in Junit
 1. Go to the test method you want to ignore
 2. Before the `@Test` annotation, enter `@Ignore`
 3. optional: You can add description why are you ignoring this test method, something like: `@Ignore ("ignoring this test case for now")`

a sample method would be: 

    @Ignore ("not going to test this method now")
    @Test
    public void test() {
         assertfalse(true);
    }

