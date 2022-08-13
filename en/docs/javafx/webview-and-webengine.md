---
title: "WebView and WebEngine"
slug: "webview-and-webengine"
draft: false
images: []
weight: 9949
type: docs
toc: true
---

The `WebView`is the JavaFX Node that is integrated into the JavaFX component tree. It manages a `WebEngine` and displays it's content.

The `WebEngine` is the underlying Browser Engine, which basically does the whole work.

## Loading a page
    WebView wv = new WebView();
    WebEngine we = wv.getEngine();
    we.load("https://stackoverflow.com");

[`WebView`](https://docs.oracle.com/javase/8/javafx/api/javafx/scene/web/WebView.html) is the UI shell around the [`WebEngine`](https://docs.oracle.com/javase/8/javafx/api/javafx/scene/web/WebEngine.html). Nearly all controls for non UI interaction with a page are done through the `WebEngine` class.


## Get the page history of a WebView
    WebHistory history = webView.getEngine().getHistory();

The history is basically a list of entries. Each entry represents a visited page and it provides access to relevant page info, such as URL, title, and the date the page was last visited. 

The list can be obtained by using the `getEntries()` method. The history and the corresponding list of entries change as `WebEngine` navigates across the web. The list may expand or shrink depending on browser actions. These changes can be listened to by the ObservableList API that the list exposes. 

The index of the history entry associated with the currently visited page is represented by the `currentIndexProperty()`. The current index can be used to navigate to any entry in the history by using the `go(int)` method. The `maxSizeProperty()` sets the maximum history size, which is the size of the history list

Below is an example of how to [obtain and process the List of Web History Items][1].

A `ComboBox` (comboBox) is used to store the history items. By using a `ListChangeListener` on the `WebHistory` the `ComboBox` gets updated to the current `WebHistory`. On the `ComboBox` is an `EventHandler` which redirects to the selected page.

    final WebHistory history = webEngine.getHistory();
    
    comboBox.setItems(history.getEntries());
    comboBox.setPrefWidth(60);
    comboBox.setOnAction(new EventHandler<ActionEvent>() {
        @Override
        public void handle(ActionEvent ev) {
            int offset =
                    comboBox.getSelectionModel().getSelectedIndex()
                    - history.getCurrentIndex();
            history.go(offset);
        }
    });

    history.currentIndexProperty().addListener(new ChangeListener<Number>() {

        @Override
        public void changed(ObservableValue<? extends Number> observable, Number oldValue, Number newValue) {
            // update currently selected combobox item
            comboBox.getSelectionModel().select(newValue.intValue());
        }
    });

    // set converter for value shown in the combobox:
    //   display the urls
    comboBox.setConverter(new StringConverter<WebHistory.Entry>() {

        @Override
        public String toString(WebHistory.Entry object) {
            return object == null ? null : object.getUrl();
        }

        @Override
        public WebHistory.Entry fromString(String string) {
            throw new UnsupportedOperationException();
        }
    });

  [1]: http://docs.oracle.com/javafx/8/webview/history.htm#CEGDEBHI 

## send Javascript alerts from the displayed web page to the Java applications log.
    private final Logger logger = Logger.getLogger(getClass().getCanonicalName());

    WebView webView = new WebView();
    webEngine = webView.getEngine();

    webEngine.setOnAlert(event -> logger.warning(() -> "JS alert: " + event.getData())); 


## Communication between Java app and Javascript in the web page
When using a WebView to display your own custom webpage and this webpage contains Javascript, it might be necessary to establish a two-way communication between the Java program and the Javascript in the web page.

This example shows how to setup such a communication.

The webpage shall display an input field and a button. On clicking the button, the value from the input field is sent to the Java application, which processes it. After processing a result is sent to the Javascript which in turn displays the result on the web page.

The basic principle is that for communication from Javascript to Java an object is created in Java which is set into the webpage. And for the other direction, an object is created in Javascript and extracted from the webpage.

The following code shows the Java part, I kept it all in one file:

    package com.sothawo.test;
    
    import javafx.application.Application;
    import javafx.concurrent.Worker;
    import javafx.scene.Scene;
    import javafx.scene.web.WebEngine;
    import javafx.scene.web.WebView;
    import javafx.stage.Stage;
    import netscape.javascript.JSObject;
    
    import java.io.File;
    import java.net.URL;
    
    /**
     * @author P.J. Meisch (pj.meisch@sothawo.com).
     */
    public class WebViewApplication extends Application {
    
        /** for communication to the Javascript engine. */
        private JSObject javascriptConnector;
    
        /** for communication from the Javascript engine. */
        private JavaConnector javaConnector = new JavaConnector();;
    
        @Override
        public void start(Stage primaryStage) throws Exception {
            URL url = new File("./js-sample.html").toURI().toURL();
    
            WebView webView = new WebView();
            final WebEngine webEngine = webView.getEngine();
    
            // set up the listener
            webEngine.getLoadWorker().stateProperty().addListener((observable, oldValue, newValue) -> {
                if (Worker.State.SUCCEEDED == newValue) {
                    // set an interface object named 'javaConnector' in the web engine's page
                    JSObject window = (JSObject) webEngine.executeScript("window");
                    window.setMember("javaConnector", javaConnector);
    
                    // get the Javascript connector object. 
                    javascriptConnector = (JSObject) webEngine.executeScript("getJsConnector()");
                }
            });
    
            Scene scene = new Scene(webView, 300, 150);
            primaryStage.setScene(scene);
            primaryStage.show();
    
            // now load the page
            webEngine.load(url.toString());
        }
    
        public class JavaConnector {
            /**
             * called when the JS side wants a String to be converted.
             *
             * @param value
             *         the String to convert
             */
            public void toLowerCase(String value) {
                if (null != value) {
                    javascriptConnector.call("showResult", value.toLowerCase());
                }
            }
        }
    }

When the page has loaded, a `JavaConnector` object (which is defined by the inner class and created as a field) is set into the web page by these calls:

    JSObject window = (JSObject) webEngine.executeScript("window");
    window.setMember("javaConnector", javaConnector);

The `javascriptConnector` object is retrieved from the webpage with

    javascriptConnector = (JSObject) webEngine.executeScript("getJsConnector()");
    
When the `toLowerCase(String)` method from the `JavaConnector` is called, the passed in value is converted and then sent back via the `javascriptConnector` object.

And this is the html and javascript code:

    <!DOCTYPE html>
    <html lang="en">
        <head>
            <meta charset="UTF-8">
            <title>Sample</title>
        </head>
        <body>
            <main>
        
                <div><input id="input" type="text"></div>
                <button onclick="sendToJava();">to lower case</button>
                <div id="result"></div>
        
            </main>
        
            <script type="text/javascript">
                function sendToJava () {
                    var s = document.getElementById('input').value;
                    javaConnector.toLowerCase(s);
                };
        
                var jsConnector = {
                    showResult: function (result) {
                        document.getElementById('result').innerHTML = result;
                    }
                };
        
                function getJsConnector() {
                    return jsConnector;
                };
            </script>
        </body>
    </html>

The `sendToJava` function calls the method of the `JavaConnector` which was set by the Java code:

    function sendToJava () {
        var s = document.getElementById('input').value;
        javaConnector.toLowerCase(s);
    };
    
and the function called by the Java code to retrieve the `javascriptConnector` just returns the `jsConnector` object:
    
    var jsConnector = {
        showResult: function (result) {
            document.getElementById('result').innerHTML = result;
        }
    };
        
    function getJsConnector() {
        return jsConnector;
    };

The argument type of the calls between Java and Javascript are not limited to Strings. More info on the possible types and conversion are found in the [JSObject API doc][1].


  [1]: http://docs.oracle.com/javase/8/javafx/api/netscape/javascript/JSObject.html

