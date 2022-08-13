---
title: "TableView"
slug: "tableview"
draft: false
images: []
weight: 9932
type: docs
toc: true
---

## PropertyValueFactory
 `PropertyValueFactory` can be used as `cellValueFactory` in a `TableColumn`. It uses reflection to access methods that match a certain pattern to retrieve the data from a `TableView` item:

**Example**

    TableColumn<Person, String> nameColumn = ...
    PropertyValueFactory<Person, String> valueFactory = new PropertyValueFactory<>("name");
    nameColumn.setCellValueFactory(valueFactory);

The name of the method that is used to get the data depends on the constructor paramerter for `PropertyValueFactory`.

* **Property method:**
 This kind of method is expected to return a `ObservableValue` containing the data. Changes can be observed. They need to match the pattern `<constructor parameter>Property` and take no parameters.
* **Getter method:**
 This kind of method expects to return the value directly (`String` in the above example). The method name needs to match the pattern `get<Constructor parameter>`. Note that here `<Constructor parameter>` begins with a *uppercase letter*. This method shouldn't take parameters.

Sample names of methods

| constructor parameter (without quotes) | name of property method | name of getter method | 
| ------ | ------ | ---- |
| foo   | fooProperty   | getFoo |
| fooBar   | fooBarProperty   | getFooBar |
| XYZ   | XYZProperty   | getXYZ |
| listIndex   | listIndexProperty   | getListIndex |
| aValue   | aValueProperty   | getAValue |

## Customizing TableCell look depending on item
Sometimes a column should show different content than just the `toString` value of the cell item. In this case the `TableCell`s created by the `cellFactory` of the `TableColumn` is customized to change the layout based on the item.

**Important Note:** `TableView` only creates the `TableCell`s that are shown in the UI. The items inside the cells can change and even become empty. The programmer needs to take care to undo any changes to the `TableCell` that were done when a item was added when it's removed. Otherwise content may still be displayed in a cell where "it doesn't belong".

In the below example setting an item results in the text being set as well as the image displayed in the `ImageView`:

    image.setImage(item.getEmoji());
    setText(item.getValue());

If the item becomes `null` or the cell becomes empty, those changes are undone by setting the values back to `null`:

    setText(null);
    image.setImage(null);

---

The following example shows a emoji in addition to text in a `TableCell`.

The `updateItem` method is called every time the item of a `Cell` is changed. Overriding this method allows to react to changes and adjust the look of the cell. Adding a listener to the `itemProperty()` of a cell would be an alternative, but in many cases `TableCell` is extended.

**Item type**

    import javafx.scene.image.Image;
    
    // enum providing image and text for certain feelings
    public enum Feeling {
        HAPPY("happy", "https://upload.wikimedia.org/wikipedia/commons/thumb/8/80/Emojione_1F600.svg/64px-Emojione_1F600.svg.png"),
        SAD("sad", "https://upload.wikimedia.org/wikipedia/commons/thumb/4/42/Emojione_1F62D.svg/64px-Emojione_1F62D.svg.png")
        ;
        private final Image emoji;
        private final String value;
    
        Feeling(String value, String url) {
            // load image in background
            emoji = new Image(url, true);
            this.value = value;
        }
    
        public Image getEmoji() {
            return emoji;
        }
    
        public String getValue() {
            return value;
        }
        
    }

**Code in Application class**

    import javafx.application.Application;
    import javafx.beans.property.ObjectProperty;
    import javafx.beans.property.SimpleObjectProperty;
    import javafx.collections.FXCollections;
    import javafx.event.ActionEvent;
    import javafx.event.EventHandler;
    import javafx.scene.Node;
    import javafx.scene.Scene;
    import javafx.scene.control.Button;
    import javafx.scene.control.TableCell;
    import javafx.scene.control.TableColumn;
    import javafx.scene.control.TableView;
    import javafx.scene.control.cell.PropertyValueFactory;
    import javafx.scene.image.ImageView;
    import javafx.scene.layout.HBox;
    import javafx.scene.layout.VBox;
    import javafx.stage.Stage;
    import javafx.util.Callback;
    
    public class EmotionTable extends Application {
    
        public static class Item {
    
            private final ObjectProperty<Feeling> feeling;
    
            public Item(Feeling feeling) {
                this.feeling = new SimpleObjectProperty<>(feeling);
            }
    
            public final Feeling getFeeling() {
                return this.feeling.get();
            }
    
            public final void setFeeling(Feeling value) {
                this.feeling.set(value);
            }
    
            public final ObjectProperty<Feeling> feelingProperty() {
                return this.feeling;
            }
    
        }
    
        @Override
        public void start(Stage primaryStage) {
            TableView<Item> table = new TableView<>(FXCollections.observableArrayList(
                    new Item(Feeling.HAPPY),
                    new Item(Feeling.HAPPY),
                    new Item(Feeling.HAPPY),
                    new Item(Feeling.SAD),
                    null,
                    new Item(Feeling.HAPPY),
                    new Item(Feeling.HAPPY),
                    new Item(Feeling.SAD)
            ));
    
            EventHandler<ActionEvent> eventHandler = new EventHandler<ActionEvent>() {
    
                @Override
                public void handle(ActionEvent event) {
                    // change table items depending on userdata of source
                    Node source = (Node) event.getSource();
                    Feeling targetFeeling = (Feeling) source.getUserData();
                    for (Item item : table.getItems()) {
                        if (item != null) {
                            item.setFeeling(targetFeeling);
                        }
                    }
                }
    
            };
    
            TableColumn<Item, Feeling> feelingColumn = new TableColumn<>("Feeling");
    
            feelingColumn.setCellValueFactory(new PropertyValueFactory<>("feeling"));

            // use custom tablecell to display emoji image
            feelingColumn.setCellFactory(new Callback<TableColumn<Item, Feeling>, TableCell<Item, Feeling>>() {
    
                @Override
                public TableCell<Item, Feeling> call(TableColumn<Item, Feeling> param) {
                    return new EmojiCell<>();
                }
            });
    
            table.getColumns().add(feelingColumn);
    
            Button sunshine = new Button("sunshine");
            Button rain = new Button("rain");
    
            sunshine.setOnAction(eventHandler);
            rain.setOnAction(eventHandler);
    
            sunshine.setUserData(Feeling.HAPPY);
            rain.setUserData(Feeling.SAD);
    
            Scene scene = new Scene(new VBox(10, table, new HBox(10, sunshine, rain)));
    
            primaryStage.setScene(scene);
            primaryStage.show();
        }
    
        public static void main(String[] args) {
            launch(args);
        }
    
    }

**Cell class**

    import javafx.scene.control.TableCell;
    import javafx.scene.image.ImageView;
    
    public class EmojiCell<T> extends TableCell<T, Feeling> {
    
        private final ImageView image;
    
        public EmojiCell() {
            // add ImageView as graphic to display it in addition
            // to the text in the cell
            image = new ImageView();
            image.setFitWidth(64);
            image.setFitHeight(64);
            image.setPreserveRatio(true);
    
            setGraphic(image);
            setMinHeight(70);
        }
    
        @Override
        protected void updateItem(Feeling item, boolean empty) {
            super.updateItem(item, empty);
    
            if (empty || item == null) {
                // set back to look of empty cell
                setText(null);
                image.setImage(null);
            } else {
                // set image and text for non-empty cell
                image.setImage(item.getEmoji());
                setText(item.getValue());
            }
        }
    }

## Sample TableView with 2 columns
**Table Item**

The following class contains 2 properties a name (`String`) and the size (`double`). Both properties are wrapped in JavaFX properties to allow the `TableView` to observe changes.

    import javafx.beans.property.DoubleProperty;
    import javafx.beans.property.SimpleDoubleProperty;
    import javafx.beans.property.SimpleStringProperty;
    import javafx.beans.property.StringProperty;
    
    public class Person {
        
        public Person(String name, double size) {
            this.size = new SimpleDoubleProperty(this, "size", size);
            this.name = new SimpleStringProperty(this, "name", name);
        }
        
        private final StringProperty name;
        private final DoubleProperty size;
    
        public final String getName() {
            return this.name.get();
        }
    
        public final void setName(String value) {
            this.name.set(value);
        }
    
        public final StringProperty nameProperty() {
            return this.name;
        }
    
        public final double getSize() {
            return this.size.get();
        }
    
        public final void setSize(double value) {
            this.size.set(value);
        }
    
        public final DoubleProperty sizeProperty() {
            return this.size;
        }
    
    }

**Sample Application**

This application shows a `TableView` with 2 columns; one for the name and one for the size of a `Person`. Selecting one of the `Person`s adds the data to `TextField`s below the `TableView` and allow the user to edit the data. Note once the edit is commited, the `TableView` is automatically updated.

To every for every `TableColumn` added to the `TableView` a `cellValueFactory` is assigned. This factory is responsible for converting table items (`Person`s) to `ObservableValue`s that contain the value that should be displayed in the table cell and that allows the `TableView` to listen to any changes for this value.

    import javafx.application.Application;
    import javafx.beans.value.ChangeListener;
    import javafx.beans.value.ObservableValue;
    import javafx.collections.FXCollections;
    import javafx.collections.ObservableList;
    import javafx.event.ActionEvent;
    import javafx.event.EventHandler;
    import javafx.scene.Scene;
    import javafx.scene.control.Button;
    import javafx.scene.control.Label;
    import javafx.scene.control.TableColumn;
    import javafx.scene.control.TableView;
    import javafx.scene.control.TextField;
    import javafx.scene.control.TextFormatter;
    import javafx.scene.layout.HBox;
    import javafx.scene.layout.VBox;
    import javafx.stage.Stage;
    import javafx.util.Callback;
    import javafx.util.StringConverter;
    
    public class TableSample extends Application {
    
        @Override
        public void start(Stage primaryStage) {
            // data for the tableview. modifying this list automatically updates the tableview
            ObservableList<Person> data = FXCollections.observableArrayList(
                    new Person("John Doe", 1.75),
                    new Person("Mary Miller", 1.70),
                    new Person("Frank Smith", 1.80),
                    new Person("Charlotte Hoffman", 1.80)
            );
    
            TableView<Person> tableView = new TableView<>(data);
    
            // table column for the name of the person
            TableColumn<Person, String> nameColumn = new TableColumn<>("Name");
            nameColumn.setCellValueFactory(new Callback<TableColumn.CellDataFeatures<Person, String>, ObservableValue<String>>() {
    
                @Override
                public ObservableValue<String> call(TableColumn.CellDataFeatures<Person, String> param) {
                    return param.getValue().nameProperty();
                }
            });
    
            // column for the size of the person
            TableColumn<Person, Number> sizeColumn = new TableColumn<>("Size");
            sizeColumn.setCellValueFactory(new Callback<TableColumn.CellDataFeatures<Person, Number>, ObservableValue<Number>>() {
    
                @Override
                public ObservableValue<Number> call(TableColumn.CellDataFeatures<Person, Number> param) {
                    return param.getValue().sizeProperty();
                }
            });
    
            // add columns to tableview
            tableView.getColumns().addAll(nameColumn, sizeColumn);
    
            TextField name = new TextField();
    
            TextField size = new TextField();

            // convert input from textfield to double
            TextFormatter<Double> sizeFormatter = new TextFormatter<Double>(new StringConverter<Double>() {
    
                @Override
                public String toString(Double object) {
                    return object == null ? "" : object.toString();
                }
    
                @Override
                public Double fromString(String string) {
                    if (string == null || string.isEmpty()) {
                        return null;
                    } else {
                        try {
                            double val = Double.parseDouble(string);
                            return val < 0 ? null : val;
                        } catch (NumberFormatException ex) {
                            return null;
                        }
                    }
                }
    
            });
            size.setTextFormatter(sizeFormatter);
    
            Button commit = new Button("Change Item");
            commit.setOnAction(new EventHandler<ActionEvent>() {
    
                @Override
                public void handle(ActionEvent event) {
                    Person p = tableView.getSelectionModel().getSelectedItem();
                    p.setName(name.getText());
                    Double value = sizeFormatter.getValue();
                    p.setSize(value == null ? -1d : value);
                }
    
            });
    
            // listen for changes in the selection to update the data in the textfields
            tableView.getSelectionModel().selectedItemProperty().addListener(new ChangeListener<Person>() {
    
                @Override
                public void changed(ObservableValue<? extends Person> observable, Person oldValue, Person newValue) {
                    commit.setDisable(newValue == null);
                    if (newValue != null) {
                        sizeFormatter.setValue(newValue.getSize());
                        name.setText(newValue.getName());
                    }
                }
    
            });
    
            HBox editors = new HBox(5, new Label("Name:"), name, new Label("Size: "), size, commit);
    
            VBox root = new VBox(10, tableView, editors);
    
            Scene scene = new Scene(root);
    
            primaryStage.setScene(scene);
            primaryStage.show();
        }
    
        public static void main(String[] args) {
            launch(args);
        }
    
    }

## Add Button to Tableview
You can add a button or another javafx component to Tableview using column `setCellFactory(Callback value)` method.

**Sample Application**

In this application we are going to add a button to TableView. When clicked to this column button, data on the same row as button is selected and its information printed.

In the `addButtonToTable()` method, `cellFactory` callback is responsible adding button to related column. We define the callable cellFactory and implement its override `call(...)` method to get `TableCell` with button and then this `cellFactory` set to related column `setCellFactory(..)` method. In our sample this is `colBtn.setCellFactory(cellFactory)`. SSCCE is below:
     
    import javafx.application.Application;
    import javafx.beans.property.SimpleIntegerProperty;
    import javafx.beans.property.SimpleStringProperty;
    import javafx.collections.FXCollections;
    import javafx.collections.ObservableList;
    import javafx.event.ActionEvent;
    import javafx.scene.Group;
    import javafx.scene.Scene;
    import javafx.scene.control.Button;
    import javafx.scene.control.TableCell;
    import javafx.scene.control.TableColumn;
    import javafx.scene.control.TableView;
    import javafx.scene.control.cell.PropertyValueFactory;
    import javafx.stage.Stage;
    import javafx.util.Callback;

    public class TableViewSample extends Application {

        private final TableView<Data> table = new TableView<>();
        private final ObservableList<Data> tvObservableList = FXCollections.observableArrayList();

        public static void main(String[] args) {
            launch(args);
        }

        @Override
        public void start(Stage stage) {

            stage.setTitle("Tableview with button column");
            stage.setWidth(600);
            stage.setHeight(600);

            setTableappearance();

            fillTableObservableListWithSampleData();
            table.setItems(tvObservableList);

            TableColumn<Data, Integer> colId = new TableColumn<>("ID");
            colId.setCellValueFactory(new PropertyValueFactory<>("id"));

            TableColumn<Data, String> colName = new TableColumn<>("Name");
            colName.setCellValueFactory(new PropertyValueFactory<>("name"));

            table.getColumns().addAll(colId, colName);

            addButtonToTable();

            Scene scene = new Scene(new Group(table));

            stage.setScene(scene);
            stage.show();
        }

        private void setTableappearance() {
            table.setColumnResizePolicy(TableView.CONSTRAINED_RESIZE_POLICY);
            table.setPrefWidth(600);
            table.setPrefHeight(600);
        }

        private void fillTableObservableListWithSampleData() {

            tvObservableList.addAll(new Data(1, "app1"),
                                    new Data(2, "app2"), 
                                    new Data(3, "app3"), 
                                    new Data(4, "app4"),
                                    new Data(5, "app5"));
        }

        private void addButtonToTable() {
            TableColumn<Data, Void> colBtn = new TableColumn("Button Column");

            Callback<TableColumn<Data, Void>, TableCell<Data, Void>> cellFactory = new Callback<TableColumn<Data, Void>, TableCell<Data, Void>>() {
                @Override
                public TableCell<Data, Void> call(final TableColumn<Data, Void> param) {
                    final TableCell<Data, Void> cell = new TableCell<Data, Void>() {

                        private final Button btn = new Button("Action");

                        {
                            btn.setOnAction((ActionEvent event) -> {
                                Data data = getTableView().getItems().get(getIndex());
                                System.out.println("selectedData: " + data);
                            });
                        }

                        @Override
                        public void updateItem(Void item, boolean empty) {
                            super.updateItem(item, empty);
                            if (empty) {
                                setGraphic(null);
                            } else {
                                setGraphic(btn);
                            }
                        }
                    };
                    return cell;
                }
            };

            colBtn.setCellFactory(cellFactory);

            table.getColumns().add(colBtn);

        }

        public class Data {

            private int id;
            private String name;

            private Data(int id, String name) {
                this.id = id;
                this.name = name;
            }

            public int getId() {
                return id;
            }

            public void setId(int ID) {
                this.id = ID;
            }

            public String getName() {
                return name;
            }

            public void setName(String nme) {
                this.name = nme;
            }

            @Override
            public String toString() {
                return "id: " + id + " - " + "name: " + name;
            }

        }
    }

Screenshot:
[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/6o5xQ.png

