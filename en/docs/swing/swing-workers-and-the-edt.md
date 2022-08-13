---
title: "Swing Workers and the EDT"
slug: "swing-workers-and-the-edt"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Syntax
+ public abstract class SwingWorker<T,V>
+ T - the result type returned by this SwingWorker's doInBackground and get methods.

+ V - the type used for carrying out intermediate results by this SwingWorker's          publish and process methods.

+ T doInBackground() - The abstract function that must be overridden.Return type is T.

## Main and event dispatch thread
Like any other java program, every swing program starts with a main method. The main method is initiated by the main thread. However, Swing components need to be created and updated on the event dispatch thread (or short: EDT). To illustrate the dynamic between the main thread and the EDT take a look at this [Hello World!][1] example. 

The main thread is just used to delegate the creation of the window to the EDT. If the EDT is not initiated yet, the first call to `SwingUtilities.invokeLater` will setup the necessary infrastructure for processing Swing components. Furthermore, the EDT remains active in the background. The main thread is going to die directly after initiating the EDT setup, but the EDT will remain active until the user exits the program. This can be achieved by hitting the close box on the visible `JFrame` instance. This will shutdown the EDT and the program's process is going to entirely.


  [1]: https://www.wikiod.com/swing

## Find the first N even numbers and display the results in a JTextArea where computations are done in background.
    import java.awt.EventQueue;
    import java.awt.GridLayout;
    import java.awt.event.WindowAdapter;
    import java.awt.event.WindowEvent;
    import java.awt.event.WindowListener;
    import java.util.ArrayList;
    import java.util.List;
    
    import javax.swing.JFrame;
    import javax.swing.JTextArea;
    import javax.swing.SwingWorker;
    
    class PrimeNumbersTask extends SwingWorker<List<Integer>, Integer> {
        private final int numbersToFind;
    
        private final JTextArea textArea;
    
        PrimeNumbersTask(JTextArea textArea, int numbersToFind) {
            this.numbersToFind = numbersToFind;
            this.textArea = textArea;
        }
    
        @Override
        public List<Integer> doInBackground() {
            final List<Integer> result = new ArrayList<>();
            boolean interrupted = false;
            for (int i = 0; !interrupted && (i < numbersToFind); i += 2) {
                interrupted = doIntenseComputing();
                result.add(i);
                publish(i); // sends data to process function
            }
            return result;
        }
    
        private boolean doIntenseComputing() {
            try {
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                return true;
            }
            return false;
        }
    
        @Override
        protected void process(List<Integer> chunks) {
            for (int number : chunks) {
                // the process method will be called on the EDT
                // thus UI elementes may be updated in here
                textArea.append(number + "\n");
            }
        }
    }
    
    public class SwingWorkerExample extends JFrame {
        private JTextArea textArea;
    
        public SwingWorkerExample() {
            super("Java SwingWorker Example");
            init();
        }
    
        private void init() {
            setSize(400, 400);
            setLayout(new GridLayout(1, 1));
            textArea = new JTextArea();
            add(textArea);
    
            addWindowListener(new WindowAdapter() {
                public void windowClosing(WindowEvent e) {
                    dispose();
                    System.exit(0);
                }
            });
        }
    
        public static void main(String args[]) throws Exception {
    
            SwingWorkerExample ui = new SwingWorkerExample();
            EventQueue.invokeLater(() -> {
                ui.setVisible(true);
            });
    
            int n = 100;
            PrimeNumbersTask task = new PrimeNumbersTask(ui.textArea, n);
            task.execute(); // run async worker which will do long running task on a
            // different thread
            System.out.println(task.get());
        }
    }

