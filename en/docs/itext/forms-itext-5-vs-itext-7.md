---
title: "Forms iText 5 vs iText 7"
slug: "forms-itext-5-vs-itext-7"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

iText 5 is a library that has grown organically. Many developers contributed code. For instance: one developer contributed code to create form fields from scratch, using classes such as `TextField` and `PdfFormField`; another developer contributed code to change existing form fields, using the `AcroField` class and a series of `setFieldProperty()` methods.

In iText 5, the classes used to create form fields cannot be used to change form fields, and vice-versa. There is no relationship whatsoever between the two sets of classes. That's confusing for many users. For instance: some users discover the `TextField` class, and assume they can use that class to change the properties of an existing text field. This isn't the case, they need to use the `AcroFields` class instead.

All of this is fixed in iText 7. We created a new set of classes such as `PdfFormField` and its subclass `PdfTextField` that can be used to create a new field, as well as to update an existing form field.

The iText 7 form field methods can be chained to make your code more compact, and they are much more intuitive than the corresponding methods in iText 5. Making the form functionality more elegant was one of the key reasons to rewrite iText from scratch.

## FormCreation.java (iText 5)
In this iText 5 example, we'll create a text field and we'll add it to a PDF:

[![enter image description here][1]][1]

    public void manipulatePdf(String src, String dest) throws DocumentException, IOException {
        PdfReader reader = new PdfReader(src);
        PdfStamper stamper = new PdfStamper(reader, new FileOutputStream(dest));
        TextField tf = new TextField(stamper.getWriter(),
            new Rectangle(110, 780, 180, 806), "text");
        tf.setBorderColor(BaseColor.BLUE);
        tf.setBorderWidth(2);
        tf.setTextColor(BaseColor.RED);
        tf.setFontSize(12);
        tf.setText("Text field");
        PdfFormField field = tf.getTextField();
        stamper.addAnnotation(field, 1);
        stamper.close();
        reader.close();
    }

Source: [developers.itextpdf.com](http://developers.itextpdf.com/content/itext-7-examples/itext-7-differences-itext-5/forms#2872-formcreation.java)

  [1]: http://i.stack.imgur.com/QIWnl.png

## FormCreation.java (iText 7)
In this iText 7 example, we'll create a text field and we'll add it to a PDF:

[![enter image description here][1]][1]


    public void manipulatePdf(String src, String dest) throws IOException {
        PdfReader reader = new PdfReader(src);
        PdfDocument pdf = new PdfDocument(reader, new PdfWriter(dest));
        PdfAcroForm form = PdfAcroForm.getAcroForm(pdf, true);
        PdfFormField tf = PdfTextFormField.createText(
            pdf, new Rectangle(110, 780, 70, 26), "text", "Text Field")
            .setBorderColor(Color.BLUE)
            .setBorderWidth(2)
            .setColor(Color.RED)
            .setFontSize(12);
        form.addField(tf);
        pdf.close();
    }

Source: [developers.itextpdf.com](http://developers.itextpdf.com/content/itext-7-examples/itext-7-differences-itext-5/forms#2874-formcreation.java)

  [1]: http://i.stack.imgur.com/wWEpy.png

## FormFilling.java (iText 5)
In this iText 5 example, we'll change the properties and the value of a text field:

[![enter image description here][1]][1]

    public void manipulatePdf(String src, String dest) throws DocumentException, IOException {
        PdfReader reader = new PdfReader(src);
        PdfStamper stamper = new PdfStamper(reader, new FileOutputStream(dest));
        AcroFields fields = stamper.getAcroFields();
        fields.setFieldProperty("text", "textcolor", BaseColor.BLUE, null);
        fields.setFieldProperty("text", "bordercolor", BaseColor.RED, null);
        fields.setFieldProperty("text", "fontsize", 14, null);
        fields.setField("text", "Field Text");
        stamper.close();
        reader.close();
    }

Source: [developers.itextpdf.com](http://developers.itextpdf.com/content/itext-7-examples/itext-7-differences-itext-5/forms#2873-formfilling.java)

  [1]: http://i.stack.imgur.com/TpIhz.png

## FormFilling.java (iText 7)
In this iText 7 example, we'll change the properties and the value of a text field:

[![enter image description here][1]][1]

    public void manipulatePdf(String src, String dest) throws IOException {
        PdfReader reader = new PdfReader(src);
        PdfDocument pdf = new PdfDocument(reader, new PdfWriter(dest));
        PdfAcroForm form = PdfAcroForm.getAcroForm(pdf, true);
        PdfFormField tf = form.getFormFields().get("text");
        tf.setBorderColor(Color.RED)
            .setColor(Color.BLUE)
            .setFontSize(14)
            .setValue("Field Text");
        pdf.close();
    }

Source: [developers.itextpdf.com](http://developers.itextpdf.com/content/itext-7-examples/itext-7-differences-itext-5/forms#2875-formfilling.java)

  [1]: http://i.stack.imgur.com/cmBfH.png

