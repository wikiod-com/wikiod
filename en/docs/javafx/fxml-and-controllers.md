---
title: "FXML and Controllers"
slug: "fxml-and-controllers"
draft: false
images: []
weight: 9705
type: docs
toc: true
---

## Syntax
- xmlns:fx="http://javafx.com/fxml" &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; // namespace declaration 

## Setting Properties
There are multiple ways of adding data to a object in fxml:

# `<property>` tag

A tag with the name of a property can be added as child of an element used for creating a instance. The child of this tag is assigned to the property using the setter or added to the contents of the property (readonly list/map properties).

# Default property

A class can be annotated with the `@DefaultProperty` annotation. In this case elements can be directly added as child element without using a element with the name of the property.

# `property="value"` attribute

Properties can be assigned using the property name as attribute name and the value as attribute value. This has the same effect as adding the following element as child of the tag:

<!-- language: lang-xml -->

    <property>
        <String fx:value="value" />
    </property>

# static setters
Properties can be set using `static` setters too. These are `static` methods named `setProperty` that take the element as first parameter and the value to set as second parameter. Those methods can recide in any class and can be used using `ContainingClass.property` instead of the usual property name.

**Note:** Currently it seems to be neccesary to have a corresponding static getter method (i.e. a static method named `getProperty` taking the element as parameter in the same class as the static setter) for this to work unless the value type is `String`.

# Type Coercion
The following mechanism is used to get a object of the correct class during assignments, e.g. to suit the parameter type of a setter method.

If the classes are assignable, then the value itself is used.

Otherwise the value is converted as follows

| Target type | value used (source value `s`) |
| ------ | ------ |
| `Boolean`, `boolean`   | `Boolean.valueOf(s)` |
| `char`, `Character`   | `s.toString.charAt(0)` |
| other primitive type or wrapper type   | appropriate method for target type, in case the `s` is a `Number`, the `valueOf(s.toString())` for the wrapper type otherwise |
| `BigInteger` | `BigInteger.valueOf(s.longValue())` is `s` is a `Number`, `new BigInteger(s.toString())` otherwise |
| `BigDecimal` | `BigDecimal.valueOf(s.doubleValue())` is `s` is a `Number`, `new BigDecimal(s.toString())` otherwise |
| Number | `Double.valueOf(s.toString())` if `s.toString()` contains a `.`, `Long.valueOf(s.toString())` otherwise |
| `Class` | `Class.forName(s.toString())` invoked using the context `ClassLoader` of the current thread without initializing the class |
| enum | The result of the `valueOf` method, additionally converted to an all uppercase `String` seperated by `_` inserted before each uppercase letter, if `s` is a `String` that starts with a lowercase letter |
| other | the value returned by a `static` `valueOf` method in the targetType, that has a parameter matching the type of `s` or a superclass of that type |

**Note:** This behavior isn't well-documented and could be subject to change.

# Example

    public enum Location {
        WASHINGTON_DC,
        LONDON;
    }
<!---->

    package fxml.sample;
    
    import java.math.BigInteger;
    import java.util.ArrayList;
    import java.util.HashMap;
    import java.util.List;
    import java.util.Map;
    import javafx.beans.DefaultProperty;
    
    @DefaultProperty("items")
    public class Sample {
        
        private Location loaction;
    
        public Location getLoaction() {
            return loaction;
        }
    
        public void setLoaction(Location loaction) {
            this.loaction = loaction;
        }
    
        public int getNumber() {
            return number;
        }
    
        public void setNumber(int number) {
            this.number = number;
        }
        
        int number;
    
        private final List<Object> items = new ArrayList<>();
    
        public List<Object> getItems() {
            return items;
        }
        
        private final Map<String, Object> map = new HashMap<>();
    
        public Map<String, Object> getMap() {
            return map;
        }
        
        private BigInteger serialNumber;
    
        public BigInteger getSerialNumber() {
            return serialNumber;
        }
    
        public void setSerialNumber(BigInteger serialNumber) {
            this.serialNumber = serialNumber;
        }
    
        @Override
        public String toString() {
            return "Sample{" + "loaction=" + loaction + ", number=" + number + ", items=" + items + ", map=" + map + ", serialNumber=" + serialNumber + '}';
        }
        
    }
<!---->

    package fxml.sample;
    
    public class Container {
    
        public static int getNumber(Sample sample) {
            return sample.number;
        }
    
        public static void setNumber(Sample sample, int number) {
            sample.number = number;
        }
    
        private final String value;
    
        private Container(String value) {
            this.value = value;
        }
    
        public static Container valueOf(String s) {
            return new Container(s);
        }
    
        @Override
        public String toString() {
            return "42" + value;
        }
    
    }

Printing the result of loading the below `fxml` file yields

<!-- language: none -->

    Sample{loaction=WASHINGTON_DC, number=5, items=[42a, 42b, 42c, 42d, 42e, 42f], map={answer=42, g=9.81, hello=42A, sample=Sample{loaction=null, number=33, items=[], map={}, serialNumber=null}}, serialNumber=4299}

<!-- language: lang-xml -->
    
    <?xml version="1.0" encoding="UTF-8"?>
    
    <?import java.lang.*?>
    <?import fxml.sample.*?>
    
    <Sample xmlns:fx="http://javafx.com/fxml/1" Container.number="5" loaction="washingtonDc">
        
        <!-- set serialNumber property (type coercion) -->
        <serialNumber>
            <Container fx:value="99"/>
        </serialNumber>
        
        <!-- Add elements to default property-->
        <Container fx:value="a"/>
        <Container fx:value="b"/>
        <Container fx:value="c"/>
        <Container fx:value="d"/>
        <Container fx:value="e"/>
        <Container fx:value="f"/>
        
        <!-- fill readonly map property -->
        <map g="9.81">
            <hello>
                <Container fx:value="A"/>
            </hello>
            <answer>
                <Container fx:value=""/>
            </answer>
            <sample>
                <Sample>
                    <!-- static setter-->
                    <Container.number>
                        <Integer fx:value="33" />
                    </Container.number>
                </Sample>
            </sample>
        </map>
    </Sample>



## Passing parameters to FXML - using a controllerFactory
**Problem:** Some data needs to be passed to a scene loaded from a fxml.

**Solution**

Specify a controller factory that is responsible for creating the controllers. Pass the data to the controller instance created by the factory.

**FXML**

<!-- language: lang-xml -->

    <?xml version="1.0" encoding="UTF-8"?>
    
    <?import javafx.scene.text.*?>
    <?import javafx.scene.layout.*?>
    
    <VBox xmlns:fx="http://javafx.com/fxml/1" fx:controller="valuepassing.TestController">
        <children>
            <Text fx:id="target" />
        </children>
    </VBox>

**Controller**

    package valuepassing;

    import javafx.fxml.FXML;
    import javafx.scene.text.Text;
    
    public class TestController {
    
        private final String data;
    
        public TestController(String data) {
            this.data = data;
        }
        
        @FXML
        private Text target;
        
        public void initialize() {
            // handle data once the fields are injected
            target.setText(data);
        }
    
    }

**Code used for loading the fxml**

   String data = "Hello World!";

    Map<Class, Callable<?>> creators = new HashMap<>();
    creators.put(TestController.class, new Callable<TestController>() {

        @Override
        public TestController call() throws Exception {
            return new TestController(data);
        }

    });

    FXMLLoader loader = new FXMLLoader(getClass().getResource("test.fxml"));

    loader.setControllerFactory(new Callback<Class<?>, Object>() {

        @Override
        public Object call(Class<?> param) {
            Callable<?> callable = creators.get(param);
            if (callable == null) {
                try {
                    // default handling: use no-arg constructor
                    return param.newInstance();
                } catch (InstantiationException | IllegalAccessException ex) {
                    throw new IllegalStateException(ex);
                }
            } else {
                try {
                    return callable.call();
                } catch (Exception ex) {
                    throw new IllegalStateException(ex);
                }
            }
        }
    });

    Parent root = loader.load();

---

This may seem complex, but it can be useful, if the fxml should be able to decide, which controller class it needs.


## Instance creation in FXML
The following class is used to demonstrate, how instances of classes can be created:

<!-- if version [lt JavaFX 8] -->
The annotation in `Person(@NamedArg("name") String name)` has to be removed, since the `@NamedArg` annotation is unavailable.
<!-- end version if -->

    package fxml.sample;
    
    import javafx.beans.NamedArg;
    import javafx.beans.property.SimpleStringProperty;
    import javafx.beans.property.StringProperty;

    public class Person {

        public static final Person JOHN = new Person("John");
    
        public Person() {
            System.out.println("Person()");
        }
        
        public Person(@NamedArg("name") String name) {
            System.out.println("Person(String)");
            this.name.set(name);
        }
        
        public Person(Person person) {
            System.out.println("Person(Person)");
            this.name.set(person.getName());
        }

        private final StringProperty name = new SimpleStringProperty();
    
        public final String getName() {
            System.out.println("getter");
            return this.name.get();
        }
    
        public final void setName(String value) {
            System.out.println("setter");
            this.name.set(value);
        }
    
        public final StringProperty nameProperty() {
            System.out.println("property getter");
            return this.name;
        }
        
        public static Person valueOf(String value) {
            System.out.println("valueOf");
            return new Person(value);
        }
        
        public static Person createPerson() {
            System.out.println("createPerson");
            return new Person();
        }
        
    }

Assume the `Person` class has already been initialized before loading the fxml.

### A note on imports

In the following fxml example the imports section will be left out. However the fxml should start with

<!-- language: lang-xml -->

    <?xml version="1.0" encoding="UTF-8"?>

followed by an imports section importing all classes used in the fxml file. Those imports are similar to non-static imports, but are added as processing instructions. **Even classes from the `java.lang` package need to be imported.**

In this case the following imports are should be added:

<!-- language: lang-xml -->

    <?import java.lang.*?>
    <?import fxml.sample.Person?>

&nbsp;

<!-- if version [gte JavaFX 8] -->
## `@NamedArg` annotated constructor
If there is a constructor where every parameter is annotated with `@NamedArg` and all values of the `@NamedArg` annotations are present in the fxml, the constructor will be used with those parameters.

<!-- language: lang-xml -->

    <Person name="John"/>
<!-- language: lang-xml -->   
    
    <Person xmlns:fx="http://javafx.com/fxml">
        <name>
            <String fx:value="John"/>
        </name>
    </Person>

Both result in the following console output, if loaded:

<!-- language: none -->

    Person(String)

<!-- end version if -->

&nbsp;

## No args constructor

If there is no suitable `@NamedArg` annotated constructor is available, the constructor that takes no parameters will be used.

Remove the `@NamedArg` annotation from the constructor and try loading.

<!-- language: lang-xml -->

    <Person name="John"/>

This will use the constructor without parameters.

Output:

<!-- language: none -->

    Person()
    setter

&nbsp;

## `fx:value` attribute

The `fx:value` attribute can be used to pass it's value to a `static` `valueOf` method taking a `String` parameter and returning the instance to use.

Example

<!-- language: lang-xml -->

    <Person xmlns:fx="http://javafx.com/fxml" fx:value="John"/>

Output:

<!-- language: none -->

    valueOf
    Person(String)

&nbsp;

## `fx:factory`

The `fx:factory` attribute allows creation of objects using arbitrary `static` methods that do not take parameters.

Example

<!-- language: lang-xml -->

    <Person xmlns:fx="http://javafx.com/fxml" fx:factory="createPerson">
        <name>
            <String fx:value="John"/>
        </name>
    </Person>

Output:

<!-- language: none -->

    createPerson
    Person()
    setter

&nbsp;

## `<fx:copy>`

Using `fx:copy` a copy constructor can be invoked. Specifying the `fx:id` of another The `source` attribute of the tag will invoke the copy constructor with that object as parameter.

Example:

<!-- language: lang-xml -->

    <ArrayList xmlns:fx="http://javafx.com/fxml">
        <Person fx:id="p1" fx:constant="JOHN"/>
        <fx:copy source="p1"/>
    </ArrayList>

Output

<!-- language: none -->

    Person(Person)
    getter

&nbsp;

## `fx:constant`

`fx:constant` allows getting a value from a `static final` field.

Example

<!-- language: lang-xml -->

    <Person xmlns:fx="http://javafx.com/fxml" fx:constant="JOHN"/>

won't produce any output, since this just references `JOHN` which was created when initializing the class.




## Example FXML
   A Simple FXML document outlining an `AnchorPane` containing a button and a label node:
 
<!-- language: lang-xml -->

    <?xml version="1.0" encoding="UTF-8"?>
    
    <?import java.lang.*?>
    <?import java.util.*?>
    <?import javafx.scene.*?>
    <?import javafx.scene.control.*?>
    <?import javafx.scene.layout.*?>
    
    <AnchorPane id="AnchorPane" prefHeight="200" prefWidth="320" xmlns:fx="http://javafx.com/fxml/1" 
            fx:controller="com.example.FXMLDocumentController">
        <children>
            <Button layoutX="126" layoutY="90" text="Click Me!" onAction="#handleButtonAction" fx:id="button" />
            <Label layoutX="126" layoutY="120" minHeight="16" minWidth="69" fx:id="label" />
        </children>
    </AnchorPane>

This example FXML file is associated with a controller class. The association between the FXML and the controller class, in this case, is made by specifying the class name as the value of the `fx:controller` attribute in the root element of the FXML: `fx:controller="com.example.FXMLDocumentController"`. The controller class allows for Java code to be executed in response to user actions on the UI elements defined in the FXML file:

    package com.example ;

    import java.net.URL;
    import java.util.ResourceBundle;
    import javafx.event.ActionEvent;
    import javafx.fxml.FXML;
    import javafx.fxml.Initializable;
    import javafx.scene.control.Label;
    
    public class FXMLDocumentController {
        
        @FXML
        private Label label;
        
        @FXML
        private void handleButtonAction(ActionEvent event) {
            System.out.println("You clicked me!");
            label.setText("Hello World!");
        }
        
        @Override
        public void initialize(URL url, ResourceBundle resources) {
            // Initialization code can go here. 
            // The parameters url and resources can be omitted if they are not needed
        }    
        
    }


An `FXMLLoader` can be used to load the FXML file:


    public class MyApp extends Application {

        @Override
        public void start(Stage stage) throws Exception {

            FXMLLoader loader = new FXMLLoader();
            loader.setLocation(getClass().getResource("FXMLDocument.fxml"));
            Parent root = loader.load();
            
            Scene scene = new Scene(root);
            
            stage.setScene(scene);
            stage.show();
        }

    }

The `load` method performs several actions, and it is useful to understand the order in which they happen. In this simple example:

 1. The `FXMLLoader` reads and parses the FXML file. It creates objects corresponding to the elements defined in the file, and makes note of any `fx:id` attributes defined on them.
 2. Since the root element of the FXML file defined a `fx:controller` attribute, the `FXMLLoader` creates a *new instance* of the class that it specifies. By default this happens by invoking the no-argument constructor on the class specified.
 3. Any elements with `fx:id` attributes defined which have fields in the controller with matching field names, and which are either `public` (not recommended) or annotated `@FXML` (recommended) are "injected" into those corresponding fields. So in this example, since there is a `Label` in the FXML file with `fx:id="label"` and a field in the controller defined as

        @FXML
        private Label label ;

    the `label` field is initialized with the `Label` instance created by the `FXMLLoader`.

 4. Event handlers are registered with any elements in the FXML file with `onXXX="#..."` properties defined. These event handlers invoke the specified method in the controller class. In this example, since the `Button` has `onAction="#handleButtonAction"`, and the controller defines a method

        @FXML
        private void handleButtonAction(ActionEvent event) { ... }

    when an action is fired on the button (e.g. the user presses it), this method is invoked. The method must have `void` return type, and can either define a parameter matching the event type (`ActionEvent` in this example), or can define no parameters.

 5. Finally, if the controller class defines an `initialize` method, this method is invoked. Notice this happens after the `@FXML` fields have been injected, so they can be safely accessed in this method and will be initialized with the instances corresponding to the elements in the FXML file. The `initialize()` method can either take no parameters, or can take a `URL` and a `ResourceBundle`. In the latter case, these parameters will be populated by the `URL` representing the location of the FXML file, and any `ResourceBundle` set on the `FXMLLoader` via `loader.setResources(...)`. Either of these can be `null` if they were not set.

## Nested Controllers
There is no need to create the whole UI in a single FXML using a single controller.

The `<fx:include>` tag can be used to include one fxml file into another. The controller of the included fxml can be injected into the controller of the including file just as any other object created by the `FXMLLoader`.

This is done by adding the `fx:id` attribute to the `<fx:include>` element. This way the controller of the included fxml will be injected to the field with the name `<fx:id value>Controller`.

**Examples:**

| fx:id value | field name for injection |
| ------ | ------ |
| foo   | fooController   |
| answer42   | answer42Controller   |
| xYz   | xYzController   |

---

**Sample fxmls**

**Counter**

This is a fxml containing a `StackPane` with a `Text` node. The controller for this fxml file allows getting the current counter value as well as incrementing the counter:

**counter.fxml**

<!-- language: lang-xml -->

    <?xml version="1.0" encoding="UTF-8"?>
    
    <?import javafx.scene.text.*?>
    <?import javafx.scene.layout.*?>
    
    <StackPane prefHeight="200" prefWidth="200" xmlns:fx="http://javafx.com/fxml/1" fx:controller="counter.CounterController">
        <children>
            <Text fx:id="counter" />
        </children>
    </StackPane>

**CounterController**

    package counter;
    
    import javafx.fxml.FXML;
    import javafx.scene.text.Text;
    
    public class CounterController {
        @FXML
        private Text counter;
    
        private int value = 0;
        
        public void initialize() {
            counter.setText(Integer.toString(value));
        }
        
        public void increment() {
            value++;
            counter.setText(Integer.toString(value));
        }
        
        public int getValue() {
            return value;
        }
        
    }

---

**Including fxml**

<!-- language: lang-xml -->

    <?xml version="1.0" encoding="UTF-8"?>
    
    <?import javafx.scene.control.*?>
    <?import javafx.scene.layout.*?>
    
    <BorderPane prefHeight="500" prefWidth="500" xmlns="http://javafx.com/javafx/8" xmlns:fx="http://javafx.com/fxml/1" fx:controller="counter.OuterController">
        <left>
            <Button BorderPane.alignment="CENTER" text="increment" onAction="#increment" />
        </left>
        <center>
            <!-- content from counter.fxml included here -->
            <fx:include fx:id="count" source="counter.fxml" />
        </center>
    </BorderPane>

**OuterController**

The controller of the included fxml is injected to this controller. Here the handler for the `onAction` event for the `Button` is used to increment the counter.

    package counter;
    
    import javafx.fxml.FXML;
    
    public class OuterController {
    
        // controller of counter.fxml injected here
        @FXML
        private CounterController countController;
    
        public void initialize() {
            // controller available in initialize method
            System.out.println("Current value: " + countController.getValue());
        }
    
        @FXML
        private void increment() {
            countController.increment();
        }
    
    }

The fxmls can be loaded like this, assuming the code is called from a class in the same package as `outer.fxml`:

    Parent parent = FXMLLoader.load(getClass().getResource("outer.fxml"));

## Define Blocks and <fx:reference>
Sometimes a element needs to be created outside of the usual object structure in the fxml.

This is where *Define Blocks* come into play:

Contents inside a `<fx:define>` element are not added to the object created for the parent element.

Every child element of the `<fx:define>` needs a `fx:id` attribute.

Objects created this way can be later referenced using the `<fx:reference>` element or by using expression binding.

The `<fx:reference>` element can be used to reference any element with a `fx:id` attribute that is handled before the `<fx:reference>` element is handled by using the same value as the `fx:id` attribute of the referenced element in the `source` attribute of the `<fx:reference>` element.

<!-- language: lang-xml -->

    <?xml version="1.0" encoding="UTF-8"?>
    
    <?import javafx.scene.text.*?>
    <?import java.lang.*?>
    <?import javafx.scene.*?>
    <?import javafx.scene.control.*?>
    <?import javafx.scene.layout.*?>
    
    
    <VBox xmlns:fx="http://javafx.com/fxml/1" prefHeight="300.0" prefWidth="300.0" xmlns="http://javafx.com/javafx/8">
        <children>
            <fx:define>
                <String fx:value="My radio group" fx:id="text" />
            </fx:define>
            <Text>
                <text>
                    <!-- reference text defined above using fx:reference -->
                    <fx:reference source="text"/>
                </text>
            </Text>
            <RadioButton text="Radio 1">
                <toggleGroup>
                    <ToggleGroup fx:id="group" />
                </toggleGroup>
            </RadioButton>
            <RadioButton text="Radio 2">
                <toggleGroup>
                    <!-- reference ToggleGroup created for last RadioButton -->
                    <fx:reference source="group"/>
                </toggleGroup>
            </RadioButton>
            <RadioButton text="Radio 3" toggleGroup="$group" />
            
            <!-- reference text defined above using expression binding -->
            <Text text="$text" />
        </children>
    </VBox>



## Passing data to FXML - accessing existing controller
 **Problem:** Some data needs to be passed to a scene loaded from a fxml.

**Solution**

Specify a controller using the `fx:controller` attribute and get the controller instance created during the loading process from the `FXMLLoader` instance used to load the fxml.

Add methods for passing the data to the controller instance and handle the data in those methods.

**FXML**

<!-- language: lang-xml -->

    <?xml version="1.0" encoding="UTF-8"?>
    
    <?import javafx.scene.text.*?>
    <?import javafx.scene.layout.*?>
    
    <VBox xmlns:fx="http://javafx.com/fxml/1" fx:controller="valuepassing.TestController">
        <children>
            <Text fx:id="target" />
        </children>
    </VBox>

**Controller**

    package valuepassing;
    
    import javafx.fxml.FXML;
    import javafx.scene.text.Text;
    
    public class TestController {
    
        @FXML
        private Text target;
    
        public void setData(String data) {
            target.setText(data);
        }
    
    }

**Code used for loading the fxml**

    String data = "Hello World!";

    FXMLLoader loader = new FXMLLoader(getClass().getResource("test.fxml"));
    Parent root = loader.load();
    TestController controller = loader.<TestController>getController();
    controller.setData(data);

## Passing data to FXML - Specifying the controller instance
 **Problem:** Some data needs to be passed to a scene loaded from a fxml.

**Solution**

Set the controller using the `FXMLLoader` instance used later to load the fxml.

Make sure the controller contains the relevant data before loading the fxml.

**Note:** in this case the fxml file must not contain the `fx:controller` attribute.

**FXML**

<!-- language: lang-xml -->

    <?xml version="1.0" encoding="UTF-8"?>
    
    <?import javafx.scene.text.*?>
    <?import javafx.scene.layout.*?>
    
    <VBox xmlns:fx="http://javafx.com/fxml/1">
        <children>
            <Text fx:id="target" />
        </children>
    </VBox>

**Controller**

    import javafx.fxml.FXML;
    import javafx.scene.text.Text;
    
    public class TestController {
    
        private final String data;
    
        public TestController(String data) {
            this.data = data;
        }
        
        @FXML
        private Text target;
        
        public void initialize() {
            // handle data once the fields are injected
            target.setText(data);
        }
    
    }

**Code used for loading the fxml**

    String data = "Hello World!";

    FXMLLoader loader = new FXMLLoader(getClass().getResource("test.fxml"));
    
    TestController controller = new TestController(data);
    loader.setController(controller);
    
    Parent root = loader.load();

