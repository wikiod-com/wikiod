---
title: "Internationalization in JavaFX"
slug: "internationalization-in-javafx"
draft: false
images: []
weight: 9568
type: docs
toc: true
---

## Loading Resource Bundle
JavaFX provides an easy way to internationalize your user interfaces. While creating a view from an FXML file you can provide the `FXMLLoader` with a resource bundle:

    Locale locale = new Locale("en", "UK");
    ResourceBundle bundle = ResourceBundle.getBundle("strings", locale);
    
    Parent root = FXMLLoader.load(getClass().getClassLoader()
                                      .getResource("ui/main.fxml"), bundle);

This provided bundle is automatically used to translate all texts in your FXML file that start with a `%`. 
Lets say your properties file `strings_en_UK.properties` contains the following line:

    ui.button.text=I'm a Button

If you have a button definition in your FXML like this:

    <Button text="%ui.button.text"/>

It will automatically receive the translation for the key `ui.button.text`.

## Switching language dynamically when the application is running
This examples shows how to build a JavaFX application, where the language can be switched dynamically while the application is running.

These are the message bundle files used in the example:

**messages_en.properties**:

    window.title=Dynamic language change
    button.english=English
    button.german=German
    label.numSwitches=Number of language switches: {0}

**messages_de.properties**:

    window.title=Dynamischer Sprachwechsel
    button.english=Englisch
    button.german=Deutsch
    label.numSwitches=Anzahl Sprachwechsel: {0}

The basic idea is to have a utility class I18N (as an alternative this might be implemented a singleton). 

    import javafx.beans.binding.Bindings;
    import javafx.beans.binding.StringBinding;
    import javafx.beans.property.ObjectProperty;
    import javafx.beans.property.SimpleObjectProperty;
    import javafx.scene.control.Button;
    import javafx.scene.control.Label;
    
    import java.text.MessageFormat;
    import java.util.ArrayList;
    import java.util.Arrays;
    import java.util.List;
    import java.util.Locale;
    import java.util.ResourceBundle;
    import java.util.concurrent.Callable;
    
    /**
     * I18N utility class..
     */
    public final class I18N {
    
        /** the current selected Locale. */
        private static final ObjectProperty<Locale> locale;
    
        static {
            locale = new SimpleObjectProperty<>(getDefaultLocale());
            locale.addListener((observable, oldValue, newValue) -> Locale.setDefault(newValue));
        }
    
        /**
         * get the supported Locales.
         *
         * @return List of Locale objects.
         */
        public static List<Locale> getSupportedLocales() {
            return new ArrayList<>(Arrays.asList(Locale.ENGLISH, Locale.GERMAN));
        }
    
        /**
         * get the default locale. This is the systems default if contained in the supported locales, english otherwise.
         *
         * @return
         */
        public static Locale getDefaultLocale() {
            Locale sysDefault = Locale.getDefault();
            return getSupportedLocales().contains(sysDefault) ? sysDefault : Locale.ENGLISH;
        }
    
        public static Locale getLocale() {
            return locale.get();
        }
    
        public static void setLocale(Locale locale) {
            localeProperty().set(locale);
            Locale.setDefault(locale);
        }
    
        public static ObjectProperty<Locale> localeProperty() {
            return locale;
        }
    
        /**
         * gets the string with the given key from the resource bundle for the current locale and uses it as first argument
         * to MessageFormat.format, passing in the optional args and returning the result.
         *
         * @param key
         *         message key
         * @param args
         *         optional arguments for the message
         * @return localized formatted string
         */
        public static String get(final String key, final Object... args) {
            ResourceBundle bundle = ResourceBundle.getBundle("messages", getLocale());
            return MessageFormat.format(bundle.getString(key), args);
        }
    
        /**
         * creates a String binding to a localized String for the given message bundle key
         *
         * @param key
         *         key
         * @return String binding
         */
        public static StringBinding createStringBinding(final String key, Object... args) {
            return Bindings.createStringBinding(() -> get(key, args), locale);
        }
    
        /**
         * creates a String Binding to a localized String that is computed by calling the given func
         *
         * @param func
         *         function called on every change
         * @return StringBinding
         */
        public static StringBinding createStringBinding(Callable<String> func) {
            return Bindings.createStringBinding(func, locale);
        }
    
        /**
         * creates a bound Label whose value is computed on language change.
         *
         * @param func
         *         the function to compute the value
         * @return Label
         */
        public static Label labelForValue(Callable<String> func) {
            Label label = new Label();
            label.textProperty().bind(createStringBinding(func));
            return label;
        }
    
        /**
         * creates a bound Button for the given resourcebundle key
         *
         * @param key
         *         ResourceBundle key
         * @param args
         *         optional arguments for the message
         * @return Button
         */
        public static Button buttonForKey(final String key, final Object... args) {
            Button button = new Button();
            button.textProperty().bind(createStringBinding(key, args));
            return button;
        }
    }


This class has a static field `locale` which is a Java `Locale`object wrapped in a JavaFX `ObjectProperty`, so that bindings can be created for this property. The first methods are the standard methods to get and set a JavaFX property.

The `get(final String key, final Object... args)` is the core method that is used for the real extraction of a message from a `ResourceBundle`.

The two methods named `createStringBinding` create a `StringBinding` that is bound to the `locale`field and so the bindings will change whenever the `locale` property changes. The first one uses it's arguments to retrieve and format a message by using the `get` method mentioned above, the second one is passed in a `Callable`, which must produce the new string value.

The last two methods are methods to create JavaFX components. The first method is used to create a `Label` and uses a `Callable` for it's internal string binding. The second one creates a `Button` and uses a key value for the retrieval of the String binding.

Of course many more different objects could be created like `MenuItem` or `ToolTip`  but these two should be enough for an example.

This code shows how this class is used within the application:

    import javafx.application.Application;
    import javafx.geometry.Insets;
    import javafx.scene.Scene;
    import javafx.scene.control.Button;
    import javafx.scene.control.Label;
    import javafx.scene.layout.BorderPane;
    import javafx.scene.layout.HBox;
    import javafx.stage.Stage;
    
    import java.util.Locale;
    
    /**
     * Sample application showing dynamic language switching,
     */
    public class I18nApplication extends Application {
    
        /** number of language switches. */
        private Integer numSwitches = 0;
    
        @Override
        public void start(Stage primaryStage) throws Exception {
    
            primaryStage.titleProperty().bind(I18N.createStringBinding("window.title"));
    
            // create content
            BorderPane content = new BorderPane();
    
            // at the top two buttons
            HBox hbox = new HBox();
            hbox.setPadding(new Insets(5, 5, 5, 5));
            hbox.setSpacing(5);
    
            Button buttonEnglish = I18N.buttonForKey("button.english");
            buttonEnglish.setOnAction((evt) -> switchLanguage(Locale.ENGLISH));
            hbox.getChildren().add(buttonEnglish);
    
            Button buttonGerman = I18N.buttonForKey("button.german");
            buttonGerman.setOnAction((evt) -> switchLanguage(Locale.GERMAN));
            hbox.getChildren().add(buttonGerman);
    
            content.setTop(hbox);
    
            // a label to display the number of changes, recalculating the text on every change
            final Label label = I18N.labelForValue(() -> I18N.get("label.numSwitches", numSwitches));
            content.setBottom(label);
    
            primaryStage.setScene(new Scene(content, 400, 200));
            primaryStage.show();
        }
    
        /**
         * sets the given Locale in the I18N class and keeps count of the number of switches.
         *
         * @param locale
         *         the new local to set
         */
        private void switchLanguage(Locale locale) {
            numSwitches++;
            I18N.setLocale(locale);
        }
    }

The application shows three different ways of using the `StringBinding` created by the `I18N`class:

 1. the window title is bound by directly using a `StringBinding`.
 1. the buttons use the helper method with the message keys
 1. the label uses the helper method with a `Callable`. This `Callable` uses the `I18N.get()` method to get a formatted translated string containing the actual count of switches.

On clicking a button, the counter is increased and the `I18N`s locale property is set, which in turn triggers the string bindings changing and so setting the UI's string to new values.

## Controller
A Resource bundles contain locale-specific objects. You can pass the bundle to the `FXMLLoader` during its creation. The controller must implement `Initializable` interface and override `initialize(URL location, ResourceBundle resources)` method. The second parameter to this method is `ResourceBundle` which is passed from the FXMLLoader to the controller and can be used by the controller to further translate texts or modify other locale-dependant information.

    public class MyController implements Initializable {
    
        @Override
        public void initialize(URL location, ResourceBundle resources) {
            label.setText(resources.getString("country"));
        }
    }

