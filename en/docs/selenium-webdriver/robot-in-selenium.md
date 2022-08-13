---
title: "Robot In Selenium"
slug: "robot-in-selenium"
draft: false
images: []
weight: 9802
type: docs
toc: true
---

## Syntax
 - delay(int ms)
 - keyPress(int keycode)
 - keyRelease(int keycode)
 - mouseMove(int x, int y)
 - mousePress(int buttons)
 - mouseRelease(int buttons)
 - mouseWheel(int wheelAmt)

## Parameters
| Parameter | Details |
| ------ | ------ |
|ms|Time to sleep in milliseconds|
| keycode   | Constant to press the specified key for example to press `A` code is `VK_A`. Please refer for more details :https://docs.oracle.com/javase/7/docs/api/java/awt/event/KeyEvent.html |
|x,y|Screen coordintates|
|buttons|The Button mask; a combination of one or more mouse button masks|
|wheelAmt|Number of notches to move mouse wheel, negative value to move up/away from user positive value to move down/towards user|

This section contains details about implementation of Robot API with Selenium Webdriver. The Robot class is used to generate native system input when selenium is not capable to do that for example pressing right key of mouse, pressing F1 key etc.

## Keypress event using Robot API (JAVA)

    import java.awt.AWTException;
    import java.awt.Robot;
    import java.awt.event.KeyEvent;
    
    public class KeyBoardExample {
        public static void main(String[] args) {
            try {
                Robot robot = new Robot();
                robot.delay(3000);
                robot.keyPress(KeyEvent.VK_Q); //VK_Q for Q
            } catch (AWTException e) {
                e.printStackTrace();
            }
        }
    }

**With Selenium**

Sometimes we need to press any key in order to test the key press event on web application. For an instance to test the ENTER key on login form we can write something like below with Selenium WebDriver


    import java.awt.AWTException;
    import java.awt.Robot;
    import java.awt.event.KeyEvent;
    import org.openqa.selenium.By;
    import org.openqa.selenium.WebDriver;
    import org.openqa.selenium.firefox.FirefoxDriver;
    import org.testng.annotations.Test;
    
    public class LoginTest {
        
        @Test
        public void testEnterKey() throws InterruptedException
        {
            WebDriver driver=new FirefoxDriver();        
            Robot robot=null;        
            driver.get("test-url");
            driver.manage().window().maximize();
            driver.findElement(By.xpath("xpath-expression")).click();
            driver.findElement(By.xpath("xpath-expression")).sendKeys("username");
            driver.findElement(By.xpath("xpath-expression")).sendKeys("password");        
            try {
                robot=new Robot();
            } catch (AWTException e) {
                e.printStackTrace();
            }
            //Keyboard Activity Using Robot Class
            robot.keyPress(KeyEvent.VK_ENTER);
        }
    }



## Mouse Event using Robot API (JAVA)
**Mouse movement:**

    import java.awt.Robot;
     
    public class MouseClass {
     public static void main(String[] args) throws Exception {
         Robot robot = new Robot();
     
         // SET THE MOUSE X Y POSITION
         robot.mouseMove(300, 550);
         }
    }

**Press left/right button of mouse:**

    import java.awt.Robot;
    import java.awt.event.InputEvent;
    
    public class MouseEvent {
     public static void main(String[] args) throws Exception {
         Robot robot = new Robot();
    
         // LEFT CLICK
         robot.mousePress(InputEvent.BUTTON1_MASK);
         robot.mouseRelease(InputEvent.BUTTON1_MASK);
    
         // RIGHT CLICK
         robot.mousePress(InputEvent.BUTTON3_MASK);
         robot.mouseRelease(InputEvent.BUTTON3_MASK);
         }
    }

**Click and scroll the wheel:**

    import java.awt.Robot;
    import java.awt.event.InputEvent;
     
    public class MouseClass {
     public static void main(String[] args) throws Exception {
         Robot robot = new Robot();
     
         // MIDDLE WHEEL CLICK
         robot.mousePress(InputEvent.BUTTON3_DOWN_MASK);
         robot.mouseRelease(InputEvent.BUTTON3_DOWN_MASK);
     
         // SCROLL THE MOUSE WHEEL
         robot.mouseWheel(-100);
         }
    }


