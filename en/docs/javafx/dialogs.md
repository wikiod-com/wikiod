---
title: "Dialogs"
slug: "dialogs"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

Dialogs were added in JavaFX 8 update 40.

## TextInputDialog
`TextInputDialog` allows the to ask the user to input a single `String`.

    TextInputDialog dialog = new TextInputDialog("42");
    dialog.setHeaderText("Input your favourite int.");
    dialog.setTitle("Favourite number?");
    dialog.setContentText("Your favourite int: ");

    Optional<String> result = dialog.showAndWait();

    String s = result.map(r -> {
        try {
            Integer n = Integer.valueOf(r);
            return MessageFormat.format("Nice! I like {0} too!", n);
        } catch (NumberFormatException ex) {
            return MessageFormat.format("Unfortunately \"{0}\" is not a int!", r);
        }
    }).orElse("You really don't want to tell me, huh?");

    System.out.println(s);

## ChoiceDialog
`ChoiceDialog` allows the user to pick one item from a list of options.

    List<String> options = new ArrayList<>();
    options.add("42");
    options.add("9");
    options.add("467829");
    options.add("Other");

    ChoiceDialog<String> dialog = new ChoiceDialog<>(options.get(0), options);
    dialog.setHeaderText("Choose your favourite number.");
    dialog.setTitle("Favourite number?");
    dialog.setContentText("Your favourite number:");

    Optional<String> choice = dialog.showAndWait();

    String s = choice.map(c -> "Other".equals(c) ? 
                  "Unfortunately your favourite number is not available!"
                  : "Nice! I like " + c + " too!")
               .orElse("You really don't want to tell me, huh?");

    System.out.println(s);

## Alert
`Alert` is a simple popup that displays a set of buttons and gets an result depending on the button the user clicked:

### Example

This lets the user decide, if (s)he really wants to close the primary stage:

    @Override
    public void start(Stage primaryStage) {
        Scene scene = new Scene(new Group(), 100, 100);

        primaryStage.setOnCloseRequest(evt -> {
            // allow user to decide between yes and no
            Alert alert = new Alert(Alert.AlertType.CONFIRMATION, "Do you really want to close this application?", ButtonType.YES, ButtonType.NO);

            // clicking X also means no
            ButtonType result = alert.showAndWait().orElse(ButtonType.NO);
            
            if (ButtonType.NO.equals(result)) {
                // consume event i.e. ignore close request 
                evt.consume();
            }
        });
        primaryStage.setScene(scene);
        primaryStage.show();
    }

Note that the button text is automatically adjusted depending on the `Locale`.

## Custom Button text
The text displayed in a button can be customized, by creating a `ButtonType` instance yourself:

    ButtonType answer = new ButtonType("42");
    ButtonType somethingElse = new ButtonType("54");

    Alert alert = new Alert(Alert.AlertType.NONE, "What do you get when you multiply six by nine?", answer, somethingElse);
    ButtonType result = alert.showAndWait().orElse(somethingElse);

    Alert resultDialog = new Alert(Alert.AlertType.INFORMATION,
                                   answer.equals(result) ? "Correct" : "wrong",
                                   ButtonType.OK);

    resultDialog.show();

