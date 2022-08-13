---
title: "Dialogs"
slug: "dialogs"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

This topic explains how to create dynamic dialogs to interact with the user.

## A very simple Dialog
    Dialog dlg;
    DialogGroup dGrp;
    DialogField dfName;

    dlg = new Dialog("Trivial Dialog");
    dGrp = dlg.addGroup("A Group");
    dfName = dlg.addField(extendedTypeStr(Name));

    if (dlg.run())
    {
        info(dfName.value());
    }

Extended data types have to be wrapped in a call to `extendedTypeStr()`.

## Dialog with automatic lookup
    Dialog dlg;
    DialogGroup dGrp;
    DialogField dfCustomer;

    dlg = new Dialog("Simple Dialog");
    dGrp = dlg.addGroup("A Group");
    dfCustomer = dlg.addField(extendedTypeStr(CustAccount));

    if (dlg.run())
    {
        info(dfCustomer.value());
    }

Because `CustAccount` is linked to the `AccountNum` field in the table `CustTable` dynamics will convert the field to a dropdown and populate it with all the records of that table.

## Dialog with Enum field and custom label
    Dialog dlg;
    DialogGroup dGrp;
    DialogField dfGender;

    dlg = new Dialog("Enum Dialog");
    dGrp = dlg.addGroup("A Group");
    dfGender = dlg.addField(enumStr(Gender), "Your Gender");

    if (dlg.run())
    {
        info(dfGender.value());
    }

Enums have to be wrapped inside a call to `enumStr()` since they are not extended data types. Also the second parameter overrides the default label for the dialog field.

## Dialog with Checkbox field and prefilled value
    Dialog dlg;
    DialogGroup dGrp;
    DialogField dialogField;

    dlg = new Dialog("Evil Dialog");
    dGrp = dlg.addGroup("A Group");
    dialogField = dlg.addFieldValue(extendedTypeStr(NoYesId), NoYes::Yes, "I hereby sell my soul");

    if (dlg.run())
    {
        info(dialogField.value());
    }

Since we want their souls we use `addFieldValue` and pre-check the box for them.

