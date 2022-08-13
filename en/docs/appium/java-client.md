---
title: "Java client"
slug: "java-client"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

[Java Client API][1]

[Java Client Source Code][2]


  [1]: http://appium.github.io/java-client/
  [2]: https://github.com/appium/java-client

## Android Play Store automation (Real device)
**File Structure:**

 - pom.xml
 - src/test/java/PlayStoreAutomation.java

**Launch command:**

> mvn test -Dtest=PlayStoreAutomation

## PlayStoreAutomation.java ##


    import org.junit.AfterClass;
    import org.junit.BeforeClass;
    import org.junit.Test;
    import io.appium.java_client.android.AndroidDriver;
    import io.appium.java_client.android.AndroidKeyCode;
    import io.appium.java_client.MobileElement;
    import org.openqa.selenium.remote.DesiredCapabilities;
    import org.openqa.selenium.By;
    import java.util.concurrent.TimeUnit;
    import java.net.URL;
    
    public class PlayStoreAutomation {
        public static AndroidDriver<MobileElement> driver;
    
        @BeforeClass
        public static void setUp() throws Exception {
            DesiredCapabilities capabilities = new DesiredCapabilities();
            capabilities.setCapability("platformName", "Android");
            capabilities.setCapability("deviceName", "Android Device");
            capabilities.setCapability("appPackage", "com.android.vending");
            capabilities.setCapability("appActivity", "com.google.android.finsky.activities.MainActivity");
    
            driver = new AndroidDriver<MobileElement>(new URL("http://localhost:4723/wd/hub"), capabilities);
            driver.manage().timeouts().implicitlyWait(10, TimeUnit.SECONDS);
        }
    
        @AfterClass
        public static void tearDown() {
            driver.quit();
        }
    
        @Test
        public void testPlayStore() throws Exception {
            driver.findElement(By.id("com.android.vending:id/text_container")).sendKeys("Google");
            driver.pressKeyCode(AndroidKeyCode.ENTER);
    
            // First item in the search result by Xpath
            driver.findElement(By.xpath("//android.support.v7.widget.RecyclerView[1]/android.widget.LinearLayout[1]")).click();
            // Confirm element found
            driver.findElement(By.xpath("//android.widget.TextView[@text='Google']"));
        }
    }

## pom.xml ##
   <?xml version="1.0" encoding="UTF-8"?>
    <project xmlns="http://maven.apache.org/POM/4.0.0"
             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
             xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
        <modelVersion>4.0.0</modelVersion>
    
        <groupId>com.tester.appium</groupId>
        <artifactId>tester-tests</artifactId>
        <version>1.0-SNAPSHOT</version>
    
        <dependencies>
            <dependency>
                <groupId>io.appium</groupId>
                <artifactId>java-client</artifactId>
                <version>4.0.0</version>
            </dependency>
            <dependency>
                <groupId>junit</groupId>
                <artifactId>junit</artifactId>
                <version>4.12</version>
            </dependency>
        </dependencies>
    
        <build>
            <plugins>
                <plugin>
                    <artifactId>maven-compiler-plugin</artifactId>
                    <version>3.1</version>
                    <configuration>
                        <source>1.8</source>
                        <target>1.8</target>
                    </configuration>
                </plugin>
                <plugin>
                    <groupId>org.apache.maven.plugins</groupId>
                    <artifactId>maven-surefire-plugin</artifactId>
                    <version>2.10</version>
                    <configuration>
                        <reportsDirectory>${project.build.directory}/reports</reportsDirectory>
                    </configuration>
                </plugin>
            </plugins>
        </build>
    </project>
    

