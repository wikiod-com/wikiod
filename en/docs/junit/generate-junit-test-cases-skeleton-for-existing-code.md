---
title: "Generate Junit test cases skeleton for existing code"
slug: "generate-junit-test-cases-skeleton-for-existing-code"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

Sometimes you need to generate the skeleton for the test cases based on the classes you have in your project.

## Generate Junit test cases skeleton for existing code in Eclipse
Here are the steps to generate test skeleton for existing code: 

 1. Open **Eclipse**, and choose the project you want to create test cases for
 2. In the **Package Explorer**, select the java class you want to generate the Junit test for
 3. Go to **File** -> **New** -> **Junit Test Cases**
 4. Change the **Source folder** to point to the test using Browse (Note: It is better to separate the source code from the testing code)
 5. Change the **Package** based on the destination package you want
 6. In the **Class under test**, make sure you enter the class you want to generate the test cases for. 
 7. Click **Next**
 8. Select the methods you want to test for
 9. Click **Finish**

Now, you will have a Junit class generated for testing the source class you have


