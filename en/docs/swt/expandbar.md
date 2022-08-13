---
title: "ExpandBar"
slug: "expandbar"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

The `ExpandBar` is a widget which displays a series of horizontal expand bar items. All child controls should be instances of `ExpandItem`.

## Syntax
- ExpandBar(Composite parent, int style)

## Parameters
| Parameter | Details |
| ------ | ------ |
| parent | The parent `Composite` on which the `ExpandBar` will be created |
| style | The style bits |

Per the Eclipse documentation, the `ExpandBar` class is not intended to be subclassed.

## Simple Example
    public class ExpandBarExample {

        private final Display display;
        private final Shell shell;

        public ExpandBarExample() {
            display = new Display();
            shell = new Shell(display);
            shell.setLayout(new FillLayout());

            // Create the ExpandBar on the Shell
            final ExpandBar expandBar = new ExpandBar(shell, SWT.NONE);
            expandBar.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

            // Add a single ExpandItem to the ExpandBar
            final ExpandItem expandItem = new ExpandItem(expandBar, SWT.NONE);
            expandItem.setText("ExpandItem");
            expandItem.setImage(new Image(display, "src/main/resources/sample.gif"));

            // Create a Composite which will hold all of the content in the ExpandItem
            final Composite expandContent = new Composite(expandBar, SWT.NONE);
            expandContent.setLayout(new GridLayout());

            final Label label = new Label(expandContent, SWT.NONE);
            label.setText("Hello, world!");

            // Set the Composite as the control for the ExpandItem
            expandItem.setControl(expandContent);
            // Set the height of the ExpandItem to be the computed size of its content
            expandItem.setHeight(expandContent.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
            expandItem.setExpanded(true);
        }

        public void run() {
            shell.setSize(200, 200);
            shell.open();

            while (!shell.isDisposed()) {
                if (!display.readAndDispatch()) {
                    display.sleep();
                }
            }
            display.dispose();
        }

        public static void main(String... args) {
            new ExpandBarExample().run();
        }

    }

Results in:

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/WbTLH.png

## Dynamic ExpandItem Content
    public class ExpandBarDynamicContentExample {

        private final Display display;
        private final Shell shell;

        public ExpandBarDynamicContentExample() {
            display = new Display();
            shell = new Shell(display);
            shell.setLayout(new FillLayout());

            // Create the ExpandBar on the Shell
            final ExpandBar expandBar = new ExpandBar(shell, SWT.V_SCROLL);
            expandBar.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

            // Add a single ExpandItem to the ExpandBar
            final ExpandItem expandItem = new ExpandItem(expandBar, SWT.NONE);
            expandItem.setText("ExpandItem");
            expandItem.setImage(new Image(display, "src/main/resources/sample.gif"));

            // Create a Composite which will hold all of the content in the ExpandItem
            final Composite expandContent = new Composite(expandBar, SWT.NONE);
            expandContent.setLayout(new GridLayout());

            // Add a button to add some dynamic content
            final Button button = new Button(expandContent, SWT.PUSH);
            button.setText("Add Label");
            button.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    // Add another Label to the ExpandItem content
                    new Label(expandContent, SWT.NONE).setText("Hello, World!");
                    // Re-compute the size of the content
                    expandItem.setHeight(expandContent.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
                }
            });

            // Set the Composite as the control for the ExpandItem
            expandItem.setControl(expandContent);
            // Set the height of the ExpandItem to be the computed size of its content
            expandItem.setHeight(expandContent.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
            expandItem.setExpanded(true);
        }

        public void run() {
            shell.setSize(200, 200);
            shell.open();

            while (!shell.isDisposed()) {
                if (!display.readAndDispatch()) {
                    display.sleep();
                }
            }
            display.dispose();
        }

        public static void main(String... args) {
            new ExpandBarDynamicContentExample().run();
        }

    }

Results in:

[![enter image description here][1]][1]

The key difference here is that we need to re-compute the size of the `ExpandItem` after the content changes. After adding a new `Label` within the `SelectionAdapter`:

    expandItem.setHeight(expandContent.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);

Also notice the use of the `SWT.V_SCROLL` style bit in the `ExpandBar` constructor. This is certainly optional, however it is added to ensure that all content is accessible as the number of `Label` objects grows.


  [1]: https://i.stack.imgur.com/RNe5b.png

