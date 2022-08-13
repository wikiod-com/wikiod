---
title: "Graphics"
slug: "graphics"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Using the Graphics class
# Intro
The [`Graphics`][1] class allows you to draw onto java components such as a [`Jpanel`][2], it can be used to draw strings, lines, shapes and images. This is done by *overriding* the `paintComponent(Graphics g)` method of the [`JComponent`][3] you are drawing on using the [`Graphics`][1] object received as argument to do the drawing:

## class `Board`

    import java.awt.*;
    import javax.swing.*;

    public class Board extends JPanel{
        
        public Board() {
            setBackground(Color.WHITE);
        }

        @override
        public Dimension getPreferredSize() {
            return new Dimension(400, 400);
        }
        
        public void paintComponent(Graphics g) {
            super.paintComponent(g);
            // draws a line diagonally across the screen
            g.drawLine(0, 0, 400, 400);
            // draws a rectangle around "hello there!"
            g.drawRect(140, 180, 115, 25);
        }        
    }

## wrapper class `DrawingCanvas`

    import javax.swing.*;
    
    public class DrawingCanvas extends JFrame {
        
        public DrawingCanvas() {
            
            Board board = new Board();
            
            add(board); // adds the Board to our JFrame    
            pack(); // sets JFrame dimension to contain subcomponents
            
            setResizable(false);
            setTitle("Graphics Test");
            setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
            
            setLocationRelativeTo(null); // centers window on screen
        }
        
        public static void main(String[] args) {
            DrawingCanvas canvas = new DrawingCanvas();
            canvas.setVisible(true);
        }
    }

# Colors
To draw shapes with different colors you must set the color of the [`Graphics`][1] object before each draw call using `setColor`:

    g.setColor(Color.BLUE);  // draws a blue square
    g.fillRect(10, 110, 100, 100);

    g.setColor(Color.RED);  // draws a red circle
    g.fillOval(10, 10, 100, 100);

    g.setColor(Color.GREEN);  // draws a green triangle
    int[] xPoints = {0, 200, 100};
    int[] yPoints = {100, 100, 280};
    g.fillPolygon(xPoints, yPoints, 3);


  [1]: https://docs.oracle.com/javase/8/docs/api/java/awt/Graphics.html
  [2]: https://docs.oracle.com/javase/8/docs/api/javax/swing/JPanel.html
  [3]: https://docs.oracle.com/javase/8/docs/api/javax/swing/JComponent.html

## Drawing images


## Using the Repaint Method to Create Basic Animation
The MyFrame class the extends JFrame and also contains the main method

    import javax.swing.JFrame;


    public class MyFrame extends JFrame{
    
        //main method called on startup
        public static void main(String[] args) throws InterruptedException {
        
            //creates a frame window
            MyFrame frame = new MyFrame();
        
            //very basic game loop where the graphics are re-rendered
            while(true){
                frame.getPanel().repaint();
            
                //The program waits a while before rerendering
                Thread.sleep(12);
            }
        }
    
        //the MyPanel is the other class and it extends JPanel
        private MyPanel panel;
    
        //constructor that sets some basic staring values
        public MyFrame(){
            this.setSize(500, 500);
            this.setLocationRelativeTo(null);
            this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
            //creates the MyPanel with paramaters of x=0 and y=0
            panel = new MyPanel(0,0);
            //adds the panel (which is a JComponent because it extends JPanel) 
            //into the frame 
            this.add(panel);
            //shows the frame window
            this.setVisible(true);
        }
     
        //gets the panel 
        public MyPanel getPanel(){
            return panel;
        }
    
    }

The MyPanel class that extends JPanel and has the paintComponent method

    import java.awt.Graphics;
    import javax.swing.JPanel;


    public class MyPanel extends JPanel{

        //two int variables to store the x and y coordinate 
        private int x;
        private int y;

        //construcor of the MyPanel class
        public MyPanel(int x, int y){
            this.x = x;
            this.y = y;
        }

        /*the method that deals with the graphics 
            this method is called when the component is first loaded, 
             when the component is resized and when the repaint() method is 
            called for this component
        */
        @Override
        public void paintComponent(Graphics g){
            super.paintComponent(g);

            //changes the x and y varible values
            x++;
            y++;

            //draws a rectangle at the x and y values
            g.fillRect(x, y, 50, 50);
        }

    }

