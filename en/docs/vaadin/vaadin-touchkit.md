---
title: "Vaadin TouchKit"
slug: "vaadin-touchkit"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Setup
    @Theme("mobiletheme")
    @Widgetset("com.example.myapp.MyAppWidgetSet")
    @Title("My Mobile App")
    public class SimplePhoneUI extends UI {

    @Override
    protected void init(VaadinRequest request) {

        // Define a view

        class MyView extends NavigationView {

         public MyView() {
             super("Planet Details");
             CssLayout content = new CssLayout();
             setContent(content);
             VerticalComponentGroup group = new VerticalComponentGroup();
             content.addComponent(group);
             group.addComponent(new TextField("Planet"));
             group.addComponent(new NumberField("Found"));
             group.addComponent(new Switch("Probed"));
             setRightComponent(new Button("OK"));
            }
       }
       // Use it as the content root
       setContent(new MyView());
    }

