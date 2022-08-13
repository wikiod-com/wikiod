---
title: "MigLayout"
slug: "miglayout"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Wrapping elements
This example demonstrates how to place 3 buttons in total with 2 buttons being in the first row. Then a wrap occurs, so the last button is in a new row.

The constraints are simple strings, in this case "wrap" while placing the component.
 

    public class ShowMigLayout {
    
        // Create the elements
        private final JFrame demo = new JFrame();
        private final JPanel panel = new JPanel();
        private final JButton button1 = new JButton("First Button");
        private final JButton button2 = new JButton("Second Button");
        private final JButton button3 = new JButton("Third Button");
    
        public static void main(String[] args) {
            ShowMigLayout showMigLayout = new ShowMigLayout();
            SwingUtilities.invokeLater(showMigLayout::createAndShowGui);
        }
    
        public void createAndShowGui() {
            // Set the position and the size of the frame
            demo.setBounds(400, 400, 250, 120);
    
            // Tell the panel to use the MigLayout as layout manager
            panel.setLayout(new MigLayout());
    
            panel.add(button1);
            // Notice the wrapping
            panel.add(button2, "wrap");
            panel.add(button3);
    
            demo.add(panel);
            demo.setVisible(true);
        }
    }

**Output:**

[![Wrapped buttons][1]][1]


  [1]: http://i.stack.imgur.com/UTwqZ.png

