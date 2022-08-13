---
title: "Windows"
slug: "windows"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Creating Custom Dialog
You can create custom dialogs which contains many component and perform many functionality on it. It behaves like second stage on owner stage.  
In the following example an application that shows person in the main stage tableview and creates a person in a dialog (AddingPersonDialog) prepared. GUIs created by SceneBuilder, but they can be created by pure java codes. 

Sample Application:

**AppMain.java**

    package customdialog;

    import javafx.application.Application;
    import javafx.fxml.FXMLLoader;
    import javafx.scene.Parent;
    import javafx.scene.Scene;
    import javafx.stage.Stage;
    
    public class AppMain extends Application {
    
        @Override
        public void start(Stage primaryStage) throws Exception {
            Parent root = FXMLLoader.load(getClass().getResource("AppMain.fxml"));
            Scene scene = new Scene(root, 500, 500);
            primaryStage.setScene(scene);
            primaryStage.show();
        }

        public static void main(String[] args) {
            launch(args);
        }
    
    }

**AppMainController.java**

    package customdialog;

    import javafx.collections.FXCollections;
    import javafx.collections.ObservableList;
    import javafx.event.ActionEvent;
    import javafx.fxml.FXML;
    import javafx.fxml.FXMLLoader;
    import javafx.fxml.Initializable;
    import javafx.scene.Parent;
    import javafx.scene.Scene;
    import javafx.scene.control.TableColumn;
    import javafx.scene.control.TableView;
    import javafx.scene.control.cell.PropertyValueFactory;
    import javafx.stage.Modality;
    import javafx.stage.Stage;
    
    public class AppMainController implements Initializable {
    
        @FXML
        private TableView<Person> tvData;
        @FXML
        private TableColumn colId;
        @FXML
        private TableColumn colName;
        @FXML
        private TableColumn colAge;
    
        private ObservableList<Person> tvObservableList = FXCollections.observableArrayList();
    
        @FXML
        void onOpenDialog(ActionEvent event) throws IOException {
            FXMLLoader fxmlLoader = new FXMLLoader(getClass().getResource("AddPersonDialog.fxml"));
            Parent parent = fxmlLoader.load();
            AddPersonDialogController dialogController = fxmlLoader.<AddPersonDialogController>getController();
            dialogController.setAppMainObservableList(tvObservableList);

            Scene scene = new Scene(parent, 300, 200);
            Stage stage = new Stage();
            stage.initModality(Modality.APPLICATION_MODAL);
            stage.setScene(scene);
            stage.showAndWait();
        }
    
        @Override
        public void initialize(URL location, ResourceBundle resources) {
            colId.setCellValueFactory(new PropertyValueFactory<>("id"));
            colName.setCellValueFactory(new PropertyValueFactory<>("name"));
            colAge.setCellValueFactory(new PropertyValueFactory<>("age"));
            tvData.setItems(tvObservableList);
        }
    
    }

**AppMain.fxml**

<!-- language: lang-xml -->

    <?xml version="1.0" encoding="UTF-8"?>

    <?import javafx.scene.control.Button?>
    <?import javafx.scene.control.TableColumn?>
    <?import javafx.scene.control.TableView?>
    <?import javafx.scene.layout.AnchorPane?>
    <?import javafx.scene.layout.VBox?>
    
    <AnchorPane maxHeight="400.0" minHeight="400.0" minWidth="500.0" xmlns="http://javafx.com/javafx/8.0.111" xmlns:fx="http://javafx.com/fxml/1" fx:controller="customdialog.AppMainController">
       <children>
          <VBox alignment="CENTER" layoutX="91.0" layoutY="85.0" spacing="10.0" AnchorPane.bottomAnchor="30.0" AnchorPane.leftAnchor="30.0" AnchorPane.rightAnchor="30.0" AnchorPane.topAnchor="30.0">
             <children>
                <Button mnemonicParsing="false" onAction="#onOpenDialog" text="Add Person" />
                <TableView fx:id="tvData" prefHeight="300.0" prefWidth="400.0">
                  <columns>
                    <TableColumn fx:id="colId" prefWidth="75.0" text="ID" />
                    <TableColumn fx:id="colName" prefWidth="75.0" text="Name" />
                      <TableColumn fx:id="colAge" prefWidth="75.0" text="Age" />
                  </columns>
                   <columnResizePolicy>
                      <TableView fx:constant="CONSTRAINED_RESIZE_POLICY" />
                   </columnResizePolicy>
                </TableView>
             </children>
          </VBox>
       </children>
    </AnchorPane>

**AddPersonDialogController.java**

    package customdialog;

    import javafx.collections.ObservableList;
    import javafx.event.ActionEvent;
    import javafx.fxml.FXML;
    import javafx.scene.Node;
    import javafx.scene.control.TextField;
    import javafx.stage.Stage;
    
    public class AddPersonDialogController  {
        
        @FXML
        private TextField tfId;
    
        @FXML
        private TextField tfName;
    
        @FXML
        private TextField tfAge;
        
        private ObservableList<Person> appMainObservableList;
    
        @FXML
        void btnAddPersonClicked(ActionEvent event) {
            System.out.println("btnAddPersonClicked");
            int id = Integer.valueOf(tfId.getText().trim());
            String name = tfName.getText().trim();
            int iAge = Integer.valueOf(tfAge.getText().trim());
            
            Person data = new Person(id, name, iAge);
            appMainObservableList.add(data);
            
            closeStage(event);
        }
    
        public void setAppMainObservableList(ObservableList<Person> tvObservableList) {
            this.appMainObservableList = tvObservableList;
            
        }

        private void closeStage(ActionEvent event) {
            Node  source = (Node)  event.getSource(); 
            Stage stage  = (Stage) source.getScene().getWindow();
            stage.close();
        }
    
    }

**AddPersonDialog.fxml**

<!-- language: lang-xml -->

    <?xml version="1.0" encoding="UTF-8"?>

    <?import javafx.geometry.Insets?>
    <?import javafx.scene.control.Button?>
    <?import javafx.scene.control.Label?>
    <?import javafx.scene.control.TextField?>
    <?import javafx.scene.layout.AnchorPane?>
    <?import javafx.scene.layout.HBox?>
    <?import javafx.scene.layout.VBox?>
    <?import javafx.scene.text.Text?>
    
    <AnchorPane minHeight="300.0" minWidth="400.0" xmlns="http://javafx.com/javafx/8.0.111" xmlns:fx="http://javafx.com/fxml/1" fx:controller="customdialog.AddPersonDialogController">
       <children>
          <VBox alignment="CENTER" layoutX="131.0" layoutY="50.0" prefHeight="200.0" prefWidth="100.0" AnchorPane.bottomAnchor="5.0" AnchorPane.leftAnchor="5.0" AnchorPane.rightAnchor="5.0" AnchorPane.topAnchor="5.0">
             <children>
                <Text strokeType="OUTSIDE" strokeWidth="0.0" text="Adding Person Dialog" />
                <HBox alignment="CENTER" prefHeight="50.0" prefWidth="200.0" spacing="10.0">
                   <children>
                      <Label alignment="CENTER_RIGHT" minWidth="100.0" text="Id" />
                      <TextField fx:id="tfId" HBox.hgrow="ALWAYS" />
                   </children>
                   <padding>
                      <Insets right="30.0" />
                   </padding>
                </HBox>
                <HBox alignment="CENTER" prefHeight="50.0" prefWidth="200.0" spacing="10.0">
                   <children>
                      <Label alignment="CENTER_RIGHT" minWidth="100.0" text="Name" />
                      <TextField fx:id="tfName" HBox.hgrow="ALWAYS" />
                   </children>
                   <padding>
                      <Insets right="30.0" />
                   </padding>
                </HBox>
                <HBox alignment="CENTER" prefHeight="50.0" prefWidth="200.0" spacing="10.0">
                   <children>
                      <Label alignment="CENTER_RIGHT" minWidth="100.0" text="Age" />
                      <TextField fx:id="tfAge" HBox.hgrow="ALWAYS" />
                   </children>
                   <padding>
                      <Insets right="30.0" />
                   </padding>
                </HBox>
                <HBox alignment="CENTER_RIGHT">
                   <children>
                      <Button mnemonicParsing="false" onAction="#btnAddPersonClicked" text="Add" />
                   </children>
                   <opaqueInsets>
                      <Insets />
                   </opaqueInsets>
                   <padding>
                      <Insets right="30.0" />
                   </padding>
                </HBox>
             </children>
          </VBox>
       </children>
    </AnchorPane>

**Person.java**

    package customdialog;

    import javafx.beans.property.SimpleIntegerProperty;
    import javafx.beans.property.SimpleStringProperty;
    
    public class Person {
    
        private SimpleIntegerProperty id;
        private SimpleStringProperty name;
        private SimpleIntegerProperty age;
    
        public Person(int id, String name, int age)  {
            this.id = new SimpleIntegerProperty(id);
            this.name = new SimpleStringProperty(name);
            this.age = new SimpleIntegerProperty(age);
        }
    
        public int getId() {
            return id.get();
        }
    
        public void setId(int ID) {
            this.id.set(ID);
        }
    
        public String getName() {
            return name.get();
        }
    
        public void setName(String nme) {
            this.name.set(nme);
        }
    
        public int getAge() {
            return age.get();
        }
    
        public void setAge(int age) {
            this.age.set(age);
        }
    
        @Override
        public String toString() {
            return "id: " + id.get() + " - " + "name: " + name.get()+ "age: "+ age.get();
        }
    
    }

**Screenshot**
[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/fdChY.png

## Creating a new Window
To show some content in a new window, a `Stage` needs to be created. After creation and initialisation `show` or `showAndWait` needs to be called on the `Stage` object:

    // create sample content
    Rectangle rect = new Rectangle(100, 100, 200, 300);
    Pane root = new Pane(rect);
    root.setPrefSize(500, 500);
    
    Parent content = root;
    
    // create scene containing the content
    Scene scene = new Scene(content);
    
    Stage window = new Stage();
    window.setScene(scene);
    
    // make window visible
    window.show();

**Note:** This code needs to be executed on the JavaFX application thread.

## Creating Custom Dialog
You can create custom dialogs which contains many component and perform many functionality on it. It behaves like second stage on owner stage.  
In the following example an application that shows person in the main stage tableview and creates a person in a dialog (AddingPersonDialog) prepared. GUIs created by SceneBuilder, but they can be created by pure java codes. 

Sample Application:

**AppMain.java**

    package customdialog;

    import javafx.application.Application;
    import javafx.fxml.FXMLLoader;
    import javafx.scene.Parent;
    import javafx.scene.Scene;
    import javafx.stage.Stage;
    
    public class AppMain extends Application {
    
        @Override
        public void start(Stage primaryStage) throws Exception {
            Parent root = FXMLLoader.load(getClass().getResource("AppMain.fxml"));
            Scene scene = new Scene(root, 500, 500);
            primaryStage.setScene(scene);
            primaryStage.show();
        }

        public static void main(String[] args) {
            launch(args);
        }
    
    }

**AppMainController.java**

    package customdialog;

    import javafx.collections.FXCollections;
    import javafx.collections.ObservableList;
    import javafx.event.ActionEvent;
    import javafx.fxml.FXML;
    import javafx.fxml.FXMLLoader;
    import javafx.fxml.Initializable;
    import javafx.scene.Parent;
    import javafx.scene.Scene;
    import javafx.scene.control.TableColumn;
    import javafx.scene.control.TableView;
    import javafx.scene.control.cell.PropertyValueFactory;
    import javafx.stage.Modality;
    import javafx.stage.Stage;
    
    public class AppMainController implements Initializable {
    
        @FXML
        private TableView<Person> tvData;
        @FXML
        private TableColumn colId;
        @FXML
        private TableColumn colName;
        @FXML
        private TableColumn colAge;
    
        private ObservableList<Person> tvObservableList = FXCollections.observableArrayList();
    
        @FXML
        void onOpenDialog(ActionEvent event) throws IOException {
            FXMLLoader fxmlLoader = new FXMLLoader(getClass().getResource("AddPersonDialog.fxml"));
            Parent parent = fxmlLoader.load();
            AddPersonDialogController dialogController = fxmlLoader.<AddPersonDialogController>getController();
            dialogController.setAppMainObservableList(tvObservableList);

            Scene scene = new Scene(parent, 300, 200);
            Stage stage = new Stage();
            stage.initModality(Modality.APPLICATION_MODAL);
            stage.setScene(scene);
            stage.showAndWait();
        }
    
        @Override
        public void initialize(URL location, ResourceBundle resources) {
            colId.setCellValueFactory(new PropertyValueFactory<>("id"));
            colName.setCellValueFactory(new PropertyValueFactory<>("name"));
            colAge.setCellValueFactory(new PropertyValueFactory<>("age"));
            tvData.setItems(tvObservableList);
        }
    
    }

**AppMain.fxml**

<!-- language: lang-xml -->

    <?xml version="1.0" encoding="UTF-8"?>

    <?import javafx.scene.control.Button?>
    <?import javafx.scene.control.TableColumn?>
    <?import javafx.scene.control.TableView?>
    <?import javafx.scene.layout.AnchorPane?>
    <?import javafx.scene.layout.VBox?>
    
    <AnchorPane maxHeight="400.0" minHeight="400.0" minWidth="500.0" xmlns="http://javafx.com/javafx/8.0.111" xmlns:fx="http://javafx.com/fxml/1" fx:controller="customdialog.AppMainController">
       <children>
          <VBox alignment="CENTER" layoutX="91.0" layoutY="85.0" spacing="10.0" AnchorPane.bottomAnchor="30.0" AnchorPane.leftAnchor="30.0" AnchorPane.rightAnchor="30.0" AnchorPane.topAnchor="30.0">
             <children>
                <Button mnemonicParsing="false" onAction="#onOpenDialog" text="Add Person" />
                <TableView fx:id="tvData" prefHeight="300.0" prefWidth="400.0">
                  <columns>
                    <TableColumn fx:id="colId" prefWidth="75.0" text="ID" />
                    <TableColumn fx:id="colName" prefWidth="75.0" text="Name" />
                      <TableColumn fx:id="colAge" prefWidth="75.0" text="Age" />
                  </columns>
                   <columnResizePolicy>
                      <TableView fx:constant="CONSTRAINED_RESIZE_POLICY" />
                   </columnResizePolicy>
                </TableView>
             </children>
          </VBox>
       </children>
    </AnchorPane>

**AddPersonDialogController.java**

    package customdialog;

    import javafx.collections.ObservableList;
    import javafx.event.ActionEvent;
    import javafx.fxml.FXML;
    import javafx.scene.Node;
    import javafx.scene.control.TextField;
    import javafx.stage.Stage;
    
    public class AddPersonDialogController  {
        
        @FXML
        private TextField tfId;
    
        @FXML
        private TextField tfName;
    
        @FXML
        private TextField tfAge;
        
        private ObservableList<Person> appMainObservableList;
    
        @FXML
        void btnAddPersonClicked(ActionEvent event) {
            System.out.println("btnAddPersonClicked");
            int id = Integer.valueOf(tfId.getText().trim());
            String name = tfName.getText().trim();
            int iAge = Integer.valueOf(tfAge.getText().trim());
            
            Person data = new Person(id, name, iAge);
            appMainObservableList.add(data);
            
            closeStage(event);
        }
    
        public void setAppMainObservableList(ObservableList<Person> tvObservableList) {
            this.appMainObservableList = tvObservableList;
            
        }

        private void closeStage(ActionEvent event) {
            Node  source = (Node)  event.getSource(); 
            Stage stage  = (Stage) source.getScene().getWindow();
            stage.close();
        }
    
    }

**AddPersonDialog.fxml**

<!-- language: lang-xml -->

    <?xml version="1.0" encoding="UTF-8"?>

    <?import javafx.geometry.Insets?>
    <?import javafx.scene.control.Button?>
    <?import javafx.scene.control.Label?>
    <?import javafx.scene.control.TextField?>
    <?import javafx.scene.layout.AnchorPane?>
    <?import javafx.scene.layout.HBox?>
    <?import javafx.scene.layout.VBox?>
    <?import javafx.scene.text.Text?>
    
    <AnchorPane minHeight="300.0" minWidth="400.0" xmlns="http://javafx.com/javafx/8.0.111" xmlns:fx="http://javafx.com/fxml/1" fx:controller="customdialog.AddPersonDialogController">
       <children>
          <VBox alignment="CENTER" layoutX="131.0" layoutY="50.0" prefHeight="200.0" prefWidth="100.0" AnchorPane.bottomAnchor="5.0" AnchorPane.leftAnchor="5.0" AnchorPane.rightAnchor="5.0" AnchorPane.topAnchor="5.0">
             <children>
                <Text strokeType="OUTSIDE" strokeWidth="0.0" text="Adding Person Dialog" />
                <HBox alignment="CENTER" prefHeight="50.0" prefWidth="200.0" spacing="10.0">
                   <children>
                      <Label alignment="CENTER_RIGHT" minWidth="100.0" text="Id" />
                      <TextField fx:id="tfId" HBox.hgrow="ALWAYS" />
                   </children>
                   <padding>
                      <Insets right="30.0" />
                   </padding>
                </HBox>
                <HBox alignment="CENTER" prefHeight="50.0" prefWidth="200.0" spacing="10.0">
                   <children>
                      <Label alignment="CENTER_RIGHT" minWidth="100.0" text="Name" />
                      <TextField fx:id="tfName" HBox.hgrow="ALWAYS" />
                   </children>
                   <padding>
                      <Insets right="30.0" />
                   </padding>
                </HBox>
                <HBox alignment="CENTER" prefHeight="50.0" prefWidth="200.0" spacing="10.0">
                   <children>
                      <Label alignment="CENTER_RIGHT" minWidth="100.0" text="Age" />
                      <TextField fx:id="tfAge" HBox.hgrow="ALWAYS" />
                   </children>
                   <padding>
                      <Insets right="30.0" />
                   </padding>
                </HBox>
                <HBox alignment="CENTER_RIGHT">
                   <children>
                      <Button mnemonicParsing="false" onAction="#btnAddPersonClicked" text="Add" />
                   </children>
                   <opaqueInsets>
                      <Insets />
                   </opaqueInsets>
                   <padding>
                      <Insets right="30.0" />
                   </padding>
                </HBox>
             </children>
          </VBox>
       </children>
    </AnchorPane>

**Person.java**

    package customdialog;

    import javafx.beans.property.SimpleIntegerProperty;
    import javafx.beans.property.SimpleStringProperty;
    
    public class Person {
    
        private SimpleIntegerProperty id;
        private SimpleStringProperty name;
        private SimpleIntegerProperty age;
    
        public Person(int id, String name, int age)  {
            this.id = new SimpleIntegerProperty(id);
            this.name = new SimpleStringProperty(name);
            this.age = new SimpleIntegerProperty(age);
        }
    
        public int getId() {
            return id.get();
        }
    
        public void setId(int ID) {
            this.id.set(ID);
        }
    
        public String getName() {
            return name.get();
        }
    
        public void setName(String nme) {
            this.name.set(nme);
        }
    
        public int getAge() {
            return age.get();
        }
    
        public void setAge(int age) {
            this.age.set(age);
        }
    
        @Override
        public String toString() {
            return "id: " + id.get() + " - " + "name: " + name.get()+ "age: "+ age.get();
        }
    
    }

**Screenshot**
[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/fdChY.png

