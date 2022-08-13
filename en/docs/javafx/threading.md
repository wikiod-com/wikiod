---
title: "Threading"
slug: "threading"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Updating the UI using Platform.runLater
Long-running operations must not be run on the JavaFX application thread, since this prevents JavaFX from updating the UI, resulting in a frozen UI.

Furthermore any change to a `Node` that is part of a "live" scene graph **must** happen on the JavaFX application thread. `Platform.runLater` can be used to execute those updates on the JavaFX application thread.

The following example demonstrates how to update a `Text` `Node` repeatedly from a different thread:

    import javafx.application.Application;
    import javafx.application.Platform;
    import javafx.scene.Scene;
    import javafx.scene.layout.StackPane;
    import javafx.scene.text.Text;
    import javafx.stage.Stage;
    
    public class CounterApp extends Application {

        private int count = 0;
        private final Text text = new Text(Integer.toString(count));

        private void incrementCount() {
            count++;
            text.setText(Integer.toString(count));
        }

        @Override
        public void start(Stage primaryStage) {
            StackPane root = new StackPane();
            root.getChildren().add(text);

            Scene scene = new Scene(root, 200, 200);

            // longrunning operation runs on different thread
            Thread thread = new Thread(new Runnable() {

                @Override
                public void run() {
                    Runnable updater = new Runnable() {

                        @Override
                        public void run() {
                            incrementCount();
                        }
                    };

                    while (true) {
                        try {
                            Thread.sleep(1000);
                        } catch (InterruptedException ex) {
                        }

                        // UI update is run on the Application thread
                        Platform.runLater(updater);
                    }
                }

            });
            // don't let thread prevent JVM shutdown
            thread.setDaemon(true);
            thread.start();
    
            primaryStage.setScene(scene);
            primaryStage.show();
        }

        public static void main(String[] args) {
            launch(args);
        }

    }



## Grouping UI updates
The following code makes the UI unresponsive for a short while after the button click, since too many `Platform.runLater` calls are used. (Try scrolling the `ListView` immediately after the button click.)

    @Override
    public void start(Stage primaryStage) {
        ObservableList<Integer> data = FXCollections.observableArrayList();
        ListView<Integer> listView = new ListView<>(data);
        
        Button btn = new Button("Say 'Hello World'");
        btn.setOnAction((ActionEvent event) -> {
            new Thread(() -> {
                for (int i = 0; i < 100000; i++) {
                    final int index = i;
                    Platform.runLater(() -> data.add(index));
                }
            }).start();
        });

        Scene scene = new Scene(new VBox(listView, btn));

        primaryStage.setScene(scene);
        primaryStage.show();
    }

To prevent this instead of using a large number of updates, the following code uses a `AnimationTimer` to run the update only once per frame:

    import java.util.ArrayList;
    import java.util.Arrays;
    import java.util.List;
    import java.util.logging.Level;
    import java.util.logging.Logger;
    import javafx.animation.AnimationTimer;
    
    public class Updater {
    
        @FunctionalInterface
        public static interface UpdateTask {
    
            public void update() throws Exception;
        }
    
        private final List<UpdateTask> updates = new ArrayList<>();
    
        private final AnimationTimer timer = new AnimationTimer() {
    
            @Override
            public void handle(long now) {
                synchronized (updates) {
                    for (UpdateTask r : updates) {
                        try {
                            r.update();
                        } catch (Exception ex) {
                            Logger.getLogger(Updater.class.getName()).log(Level.SEVERE, null, ex);
                        }
                    }
                    updates.clear();
                    stop();
                }
            }
        };
    
        public void addTask(UpdateTask... tasks) {
            synchronized (updates) {
                updates.addAll(Arrays.asList(tasks));
                timer.start();
            }
        }
    
    }

which allows grouping the updates using the `Updater` class:


    private final Updater updater = new Updater();

    ...

            // Platform.runLater(() -> data.add(index));
            updater.addTask(() -> data.add(index));

## How to use JavaFX Service
Instead of running intensive tasks into `JavaFX Thread` that should be done into a `Service`.So what basically is a [Service][1]?

A Service is a class which is creating a new `Thread` every time you are starting it and is passing a [Task][2] to it to do some work.The Service can return or not a value.

> Below is a typical example of JavaFX Service which is doing some work
> and return a `Map<String,String>(`):


    public class WorkerService extends Service<Map<String, String>> {
    
        /**
         * Constructor
         */
        public WorkerService () {
    
            // if succeeded
            setOnSucceeded(s -> {
                //code if Service succeeds
            });
    
            // if failed
            setOnFailed(fail -> {
                //code it Service fails
            });
    
            //if cancelled
            setOnCancelled(cancelled->{
                //code if Service get's cancelled
            });
        }
    
        /**
        * This method starts the Service
        */
        public void startTheService(){
            if(!isRunning()){
               //...
               reset();
               start();
            }
    
        }
    
        @Override
        protected Task<Map<String, String>> createTask() {
            return new Task<Map<String, String>>() {
                @Override
                protected Void call() throws Exception {
    
                        //create a Map<String, String>
                        Map<String,String> map  = new HashMap<>();
    
                       //create other variables here
    
                       try{
                            //some code here
                            //.....do your manipulation here
    
                            updateProgress(++currentProgress, totalProgress);
                        }
    
                    } catch (Exception ex) {                  
                        return null; //something bad happened so you have to do something instead of returning null
                    }
    
                    return map;
                }
            };
        }
    
    }


  [1]: https://docs.oracle.com/javase/8/javafx/api/javafx/concurrent/Service.html
  [2]: https://docs.oracle.com/javase/8/javafx/api/javafx/concurrent/Task.html

