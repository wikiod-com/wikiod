---
title: "GridLayout"
slug: "gridlayout"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## How GridLayout works
A `GridLayout` is a layout manager which places components inside a grid with equal cell sizes. You can set the number of rows, columns, the horizontal gap and the vertical gap using the following methods:

 - `setRows(int rows)`
 - `setColumns(int columns)`
 - `setHgap(int hgap)`
 - `setVgap(int vgap)`

or you can set them with the following constructors:

 - `GridLayout(int rows, int columns)`
 - `GridLayout(int rows, int columns, int hgap, int vgap)`

If the number of rows or columns is unknown, you can set the respective variable to `0`. For example:

`new GridLayout(0, 3)`

This will cause the `GridLayout` to have 3 columns and as many rows as needed.


----------

The following example demonstrates how a `GridLayout` lays out components with different values for rows, columns, horizontal gap, vertical gap and screen size.

    import java.awt.BorderLayout;
    import java.awt.Color;
    import java.awt.EventQueue;
    import java.awt.GridLayout;
    
    import javax.swing.BorderFactory;
    import javax.swing.Box;
    import javax.swing.JFrame;
    import javax.swing.JLabel;
    import javax.swing.JPanel;
    import javax.swing.JSpinner;
    import javax.swing.SpinnerNumberModel;
    import javax.swing.WindowConstants;
    import javax.swing.event.ChangeEvent;
    import javax.swing.event.ChangeListener;
    
    public class GridLayoutExample {
    
        private GridLayout gridLayout;
        private JPanel gridPanel, contentPane;
        private JSpinner rowsSpinner, columnsSpinner, hgapSpinner, vgapSpinner;
    
        public void createAndShowGUI() {
            gridLayout = new GridLayout(5, 5, 3, 3);
    
            gridPanel = new JPanel(gridLayout);
    
            final ChangeListener rowsColumnsListener = new ChangeListener() {
                @Override
                public void stateChanged(ChangeEvent e) {
                    gridLayout.setRows((int) rowsSpinner.getValue());
                    gridLayout.setColumns((int) columnsSpinner.getValue());
                    fillGrid();
                }
            };
    
            final ChangeListener gapListener = new ChangeListener() {
                @Override
                public void stateChanged(ChangeEvent e) {
                    gridLayout.setHgap((int) hgapSpinner.getValue());
                    gridLayout.setVgap((int) vgapSpinner.getValue());
                    gridLayout.layoutContainer(gridPanel);
                    contentPane.revalidate();
                    contentPane.repaint();
                }
            };
    
            rowsSpinner = new JSpinner(new SpinnerNumberModel(gridLayout.getRows(), 1, 10, 1));
            rowsSpinner.addChangeListener(rowsColumnsListener);
    
            columnsSpinner = new JSpinner(new SpinnerNumberModel(gridLayout.getColumns(), 1, 10, 1));
            columnsSpinner.addChangeListener(rowsColumnsListener);
    
            hgapSpinner = new JSpinner(new SpinnerNumberModel(gridLayout.getHgap(), 0, 50, 1));
            hgapSpinner.addChangeListener(gapListener);
    
            vgapSpinner = new JSpinner(new SpinnerNumberModel(gridLayout.getVgap(), 0, 50, 1));
            vgapSpinner.addChangeListener(gapListener);
    
            JPanel actionPanel = new JPanel();
            actionPanel.add(new JLabel("Rows:"));
            actionPanel.add(rowsSpinner);
            actionPanel.add(Box.createHorizontalStrut(10));
            actionPanel.add(new JLabel("Columns:"));
            actionPanel.add(columnsSpinner);
            actionPanel.add(Box.createHorizontalStrut(10));
            actionPanel.add(new JLabel("Horizontal gap:"));
            actionPanel.add(hgapSpinner);
            actionPanel.add(Box.createHorizontalStrut(10));
            actionPanel.add(new JLabel("Vertical gap:"));
            actionPanel.add(vgapSpinner);
    
            contentPane = new JPanel(new BorderLayout(0, 10));
            contentPane.add(gridPanel);
            contentPane.add(actionPanel, BorderLayout.SOUTH);
            
            fillGrid();
    
            JFrame frame = new JFrame("GridLayout Example");
            frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
            frame.setContentPane(contentPane);
            frame.setSize(640, 480);
            frame.setLocationByPlatform(true);
            frame.setVisible(true);
        }
    
        private void fillGrid() {
            gridPanel.removeAll();
            for (int row = 0; row < gridLayout.getRows(); row++) {
                for (int col = 0; col < gridLayout.getColumns(); col++) {
                    JLabel label = new JLabel("Row: " + row + " Column: " + col);
                    label.setHorizontalAlignment(JLabel.CENTER);
                    label.setBorder(BorderFactory.createLineBorder(Color.GRAY));
                    gridPanel.add(label);
                }
            }
            contentPane.revalidate();
            contentPane.repaint();
        }
    
        public static void main(String[] args) {
            EventQueue.invokeLater(new Runnable() {
                @Override
                public void run() {
                    new GridLayoutExample().createAndShowGUI();
                }
            });
        }
    
    }

