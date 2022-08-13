---
title: "Properties & Observable"
slug: "properties--observable"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

Properties are observable and listeners can be added to them. They are consistently used for properties of `Node`s.

## Types of properties and naming
### Standard properties

Depending on the type of the property, there are up to 3 methods for a single property. Let `<property>` denote the name of a property and `<Property>` the name of the property with an uppercase first letter. And let `T` be the type of the property; for primitive wrappers we use the primitive type here, e.g. `String` for `StringProperty` and `double` for `ReadOnlyDoubleProperty`.

| Method name | Parameters | Return type | Purpose |
| ------ | ------ |  ------ |  ------ |
| `<property>Property`   | `()` | The property itself, e.g.<br> `DoubleProperty`, `ReadOnlyStringProperty`, `ObjectProperty<VPos>` | return the property itself for adding listeners / binding |
| `get<Property>` | `()` | `T` | return the value wrapped in the property |
| `set<Property>` | `(T)` | `void` | set the value of the property |

Note that the setter does not exist for readonly properties.

### Readonly list properties

Readonly list properties are properties that provide only a getter method. The type of such a property is `ObservableList`, preferably with a type agrument specified. The value of this property never changes; the content of the `ObservableList` may be changed instead.

### Readonly map properties

Similar to readonly list properties readonly map properties only provide a getter and the content may be modified instead of the property value. The getter returns a `ObservableMap`.

## StringProperty example
The following example shows the declaration of a property (`StringProperty` in this case) and demonstrates how to add a `ChangeListener` to it.

    import java.text.MessageFormat;
    import javafx.beans.property.SimpleStringProperty;
    import javafx.beans.property.StringProperty;
    import javafx.beans.value.ChangeListener;
    import javafx.beans.value.ObservableValue;
    
    public class Person {
    
        private final StringProperty name = new SimpleStringProperty();
    
        public final String getName() {
            return this.name.get();
        }
    
        public final void setName(String value) {
            this.name.set(value);
        }
    
        public final StringProperty nameProperty() {
            return this.name;
        }
        
        public static void main(String[] args) {
            Person person = new Person();
            person.nameProperty().addListener(new ChangeListener<String>() {
    
                @Override
                public void changed(ObservableValue<? extends String> observable, String oldValue, String newValue) {
                    System.out.println(MessageFormat.format("The name changed from \"{0}\" to \"{1}\"", oldValue, newValue));
                }
                
            });
            
            person.setName("Anakin Skywalker");
            person.setName("Darth Vader");
        }
        
    }

## ReadOnlyIntegerProperty example
This example shows how to use a readonly wrapper property to create a property that cannot be written to. In this case `cost` and `price` can be modified, but `profit` will always be `price - cost`.

    import java.text.MessageFormat;
    import javafx.beans.property.IntegerProperty;
    import javafx.beans.property.ReadOnlyIntegerProperty;
    import javafx.beans.property.ReadOnlyIntegerWrapper;
    import javafx.beans.property.SimpleIntegerProperty;
    import javafx.beans.value.ChangeListener;
    import javafx.beans.value.ObservableValue;
    
    public class Product {
        
        private final IntegerProperty price = new SimpleIntegerProperty();
        private final IntegerProperty cost = new SimpleIntegerProperty();
        private final ReadOnlyIntegerWrapper profit = new ReadOnlyIntegerWrapper();
    
        public Product() {
            // the property itself can be written to
            profit.bind(price.subtract(cost));
        }
    
        public final int getCost() {
            return this.cost.get();
        }
    
        public final void setCost(int value) {
            this.cost.set(value);
        }
    
        public final IntegerProperty costProperty() {
            return this.cost;
        }
    
        public final int getPrice() {
            return this.price.get();
        }
    
        public final void setPrice(int value) {
            this.price.set(value);
        }
    
        public final IntegerProperty priceProperty() {
            return this.price;
        }
    
        public final int getProfit() {
            return this.profit.get();
        }
    
        public final ReadOnlyIntegerProperty profitProperty() {
            // return a readonly view of the property
            return this.profit.getReadOnlyProperty();
        }
        
        public static void main(String[] args) {
            Product product = new Product();
            product.profitProperty().addListener(new ChangeListener<Number>() {
    
                @Override
                public void changed(ObservableValue<? extends Number> observable, Number oldValue, Number newValue) {
                    System.out.println(MessageFormat.format("The profit changed from {0}$ to {1}$", oldValue, newValue));
                }
            
            });
            product.setCost(40);
            product.setPrice(50);
            product.setCost(20);
            product.setPrice(30);
        }
        
    }

