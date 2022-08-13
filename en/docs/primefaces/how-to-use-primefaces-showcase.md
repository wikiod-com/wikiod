---
title: "How to use Primefaces showcase"
slug: "how-to-use-primefaces-showcase"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## The example of Primefaces panelGrid component in its Showcase.
The showcase of Primefaces components you can find [here][1] and documentation is [here][2]


  [1]: http://www.primefaces.org/showcase/
  [2]: http://www.primefaces.org/documentation

**Frontend** needs to be saved as a XHTML file. This file can contain JSF, JSTL, JSP, HTML, CSS, jQuery, javaScript and its framework and more front-end technologies. 
Please, do not mix JSF and JSP technologies together. It is not a good approach.

**Note** you have to define namespaces such as c, f, h, p, pe and so on in the beginning .

    <h:form id="form">
        <p:dataGrid var="car" value="#{dataGridView.cars}" columns="3" layout="grid"
            rows="12" paginator="true" id="cars"
            paginatorTemplate="{CurrentPageReport}  {FirstPageLink} {PreviousPageLink} {PageLinks} {NextPageLink} {LastPageLink} {RowsPerPageDropdown}"
            rowsPerPageTemplate="6,12,16">
     
            <f:facet name="header">
                Cars for Sale
            </f:facet>
     
            <p:panel header="#{car.id}" style="text-align:center">
                <h:panelGrid columns="1" style="width:100%">
                    <p:graphicImage name="demo/images/car/#{car.brand}.gif"/> 
     
                    <h:outputText value="#{car.year}" />
     
                    <p:commandLink update=":form:carDetail" oncomplete="PF('carDialog').show()" title="View Detail">
                        <h:outputText styleClass="ui-icon ui-icon-search" style="margin:0 auto;" />
                        <f:setPropertyActionListener value="#{car}" target="#{dataGridView.selectedCar}" />
                    </p:commandLink>
                </h:panelGrid>
            </p:panel>
     
        </p:dataGrid>
     
        <p:dialog header="Car Info" widgetVar="carDialog" modal="true" showEffect="fade" hideEffect="fade" resizable="false">
            <p:outputPanel id="carDetail" style="text-align:center;">
                <p:panelGrid  columns="2" rendered="#{not empty dataGridView.selectedCar}" columnClasses="label,value">
                    <f:facet name="header">
                        <p:graphicImage name="demo/images/car/#{dataGridView.selectedCar.brand}-big.gif"/> 
                    </f:facet>
     
                    <h:outputText value="Id:" />
                    <h:outputText value="#{dataGridView.selectedCar.id}" />
     
                    <h:outputText value="Year" />
                    <h:outputText value="#{dataGridView.selectedCar.year}" />
     
                    <h:outputText value="Color:" />
                    <h:outputText value="#{dataGridView.selectedCar.color}" style="color:#{dataGridView.selectedCar.color}"/>
     
                    <h:outputText value="Price" />
                    <h:outputText value="$#{dataGridView.selectedCar.price}" />
                </p:panelGrid>
            </p:outputPanel>
        </p:dialog>
    </h:form>

Back-end needs to be saved as a JSF files. This file contains mostly Java, but there can be javaScript calls to Front-end. The annotation of class might be replaced with Spring configuration.
**The View class** that it is used for serving data to Frontend EL (expression language).

    package org.primefaces.showcase.view.data;
     
        import java.io.Serializable;
        import java.util.List;
        import javax.annotation.PostConstruct;
        import javax.faces.bean.ManagedBean;
        import javax.faces.bean.ManagedProperty;
        import javax.faces.bean.ViewScoped;
        import org.primefaces.showcase.domain.Car;
        import org.primefaces.showcase.service.CarService;
         
        @ManagedBean
        @ViewScoped
        public class DataGridView implements Serializable {
             
            private List<Car> cars;
             
            private Car selectedCar;
             
            @ManagedProperty("#{carService}")
            private CarService service;
             
            @PostConstruct
            public void init() {
                cars = service.createCars(48);
            }
         
            public List<Car> getCars() {
                return cars;
            }
         
            public void setService(CarService service) {
                this.service = service;
            }
         
            public Car getSelectedCar() {
                return selectedCar;
            }
         
            public void setSelectedCar(Car selectedCar) {
                this.selectedCar = selectedCar;
            }
        }

**The service class** serves data from DB but it is used in most of examples in PF Showcase to initialize and create data. 
Note that the most useful examples of code in the PF showcase are Frontend.

    package org.primefaces.showcase.service;
     
    import java.util.ArrayList;
    import java.util.List;
    import java.util.UUID;
    import javax.faces.bean.ApplicationScoped;
    import javax.faces.bean.ManagedBean;
    import org.primefaces.showcase.domain.Car;
     
    @ManagedBean(name = "carService")
    @ApplicationScoped
    public class CarService {
         
        private final static String[] colors;
         
        private final static String[] brands;
         
        static {
            colors = new String[10];
            colors[0] = "Black";
            colors[1] = "White";
            colors[2] = "Green";
            colors[3] = "Red";
            colors[4] = "Blue";
            colors[5] = "Orange";
            colors[6] = "Silver";
            colors[7] = "Yellow";
            colors[8] = "Brown";
            colors[9] = "Maroon";
             
            brands = new String[10];
            brands[0] = "BMW";
            brands[1] = "Mercedes";
            brands[2] = "Volvo";
            brands[3] = "Audi";
            brands[4] = "Renault";
            brands[5] = "Fiat";
            brands[6] = "Volkswagen";
            brands[7] = "Honda";
            brands[8] = "Jaguar";
            brands[9] = "Ford";
        }
         
        public List<Car> createCars(int size) {
            List<Car> list = new ArrayList<Car>();
            for(int i = 0 ; i < size ; i++) {
                list.add(new Car(getRandomId(), getRandomBrand(), getRandomYear(), getRandomColor(), getRandomPrice(), getRandomSoldState()));
            }
             
            return list;
        }
         
        private String getRandomId() {
            return UUID.randomUUID().toString().substring(0, 8);
        }
         
        private int getRandomYear() {
            return (int) (Math.random() * 50 + 1960);
        }
         
        private String getRandomColor() {
            return colors[(int) (Math.random() * 10)];
        }
         
        private String getRandomBrand() {
            return brands[(int) (Math.random() * 10)];
        }
         
        public int getRandomPrice() {
            return (int) (Math.random() * 100000);
        }
         
        public boolean getRandomSoldState() {
            return (Math.random() > 0.5) ? true: false;
        }
     
        public List<String> getColors() {
            return Arrays.asList(colors);
        }
         
        public List<String> getBrands() {
            return Arrays.asList(brands);
        }
    }

NOTE: Please, improve this documentation if you have enough relevant experience.

