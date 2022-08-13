---
title: "Page events (iText 5) versus Event handlers and Renderers (iText 7)"
slug: "page-events-itext-5-versus-event-handlers-and-renderers-itext-7"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

In iText 5, we introduced the concept of page events to allow developers to add specific behavior when a document is opened, when a new page is opened, when a page ends, and when a document is closed.

In the documentation, we made it very clear that it was **forbidden** to add content in the `onStartPage()` method; content can only be added in the `onEndPage()` method. We also made it very clear that the `Document` object passed to the page event methods was passed for **read-only** purposes only. It was **forbidden** to use `document.add()` even in the `onEndPage()` method.

Unfortunately, many developers completely ignore the documentation, which led to problems such as:

* http://stackoverflow.com/questions/24285547
* http://stackoverflow.com/questions/30602850
* http://stackoverflow.com/questions/17261078
* http://stackoverflow.com/questions/25641419
* ...

I can't remember how many times I got agitated because yet another developer posted a duplicate of these questions. People often wonder why they get a harsh answer, but they don't realize that a minimum of effort from their side would have saved everyone, including themselves, plenty of time. All of these questions could have been answered by saying "Read The (you-know-which) Manual."

Another option was a complete overhaul of iText so that these kind of problems can be avoided.

Due to the organic growth of iText, the page event class had also been extended with functionality that was unrelated to page events. It contained *generic chunk* functionality, it registered the start and the end of paragraphs, and so on.

**What we fixed in iText 7:**

We removed the page event functionality.

For all events with respect to pages, we now implement the `IEventHandler` interface, and we use the `addEventHandler` to add this handler as a `PdfDocumentEvent` to the `PdfDocument`. In the example, we use an `END_PAGE` event, but we could also have used a `START_PAGE` event. It doesn't matter any more whether you add content at the start or at the end. You can read more about this in [Handling events; setting viewer preferences and writer properties](http://developers.itextpdf.com/content/itext-7-building-blocks/chapter-7-handling-events-setting-viewer-preferences-and-writer-properties) which is chapter 7 in the [iText 7: Building Blocks](http://developers.itextpdf.com/content/itext-7-building-blocks/) tutorial.

We improved the building blocks in the sense that we made them more hierarchical (see [Before we start: Overview of the classes and interfaces](http://developers.itextpdf.com/content/itext-7-building-blocks/we-start-overview-classes-and-interfaces) which is the introduction of the [iText 7: Building Blocks](http://developers.itextpdf.com/content/itext-7-building-blocks/) tutorial). We also introduced a set of Renderer classes, one for each building block, and we allow developers to adapt these renderers so that a building block shows a different behavior when rendered. See for instance the renderer example in [Adding AbstractElement objects (part 1)](http://developers.itextpdf.com/content/itext-7-building-blocks/chapter-4-adding-abstractelement-objects-part-1) which is chapter 7 in the [iText 7: Building Blocks](http://developers.itextpdf.com/content/itext-7-building-blocks/) tutorial.

These changes simplify the functionality for developers who don't (want to) know much about PDF and iText, while at the same time offering an abundance of flexibility to those developers who aren't afraid to dig deep into the iText code to create a PDF exactly the way they want it.

**Want to know more?**  [Get the free ebook!](http://pages.itextpdf.com/iText-7-abc.html)

## Text2PdfPageEvents.java (iText 5)
Suppose that we have the following text file: [jekyll_hyde.txt](http://gitlab.itextsupport.com/itext/sandbox/raw/master/resources/text/jekyll_hyde.txt)

How do we convert it to a PDF that looks like this:

[![enter image description here][1]][1]

Note the blue border that is added to the titles, and the page number at the bottom of each page. In iText 5, these elements are added using page events:

    class MyPageEvents extends PdfPageEventHelper {
 
        protected float startpos = -1;
        protected boolean title = true;
 
        public void setTitle(boolean title) {
            this.title = title;
        }
 
        @Override
        public void onEndPage(PdfWriter writer, Document document) {
            Rectangle pagesize = document.getPageSize();
            ColumnText.showTextAligned(
                writer.getDirectContent(),
                Element.ALIGN_CENTER,
                new Phrase(String.valueOf(writer.getPageNumber())),
                (pagesize.getLeft() + pagesize.getRight()) / 2,
                pagesize.getBottom() + 15,
                0);
            if (startpos != -1)
                onParagraphEnd(writer, document,
                    pagesize.getBottom(document.bottomMargin()));
            startpos = pagesize.getTop(document.topMargin());
        }
 
        @Override
        public void onParagraph(PdfWriter writer, Document document,
            float paragraphPosition) {
            startpos = paragraphPosition;
        }
 
        @Override
        public void onParagraphEnd(PdfWriter writer, Document document,
            float paragraphPosition) {
            if (!title) return;
            PdfContentByte canvas = writer.getDirectContentUnder();
            Rectangle pagesize = document.getPageSize();
            canvas.saveState();
            canvas.setColorStroke(BaseColor.BLUE);
            canvas.rectangle(
                pagesize.getLeft(document.leftMargin()),
                paragraphPosition - 3,
                pagesize.getWidth() - document.leftMargin() - document.rightMargin(),
                startpos - paragraphPosition);
            canvas.stroke();
            canvas.restoreState();
        }
    }

We can reuse the code to convert a text file to a PDF from the https://www.wikiod.com/itext/text-to-pdf-itext-5-versus-itext-7#Text2Pdf.java (iText 5) example, and introduce the page event to the `PdfWriter`:

    public void createPdf(String dest)
    throws DocumentException, IOException {
        Document document = new Document();
        PdfWriter writer = PdfWriter.getInstance(document, new FileOutputStream(dest));
        MyPageEvents events = new MyPageEvents();
        writer.setPageEvent(events);
        document.open();
        BufferedReader br = new BufferedReader(new FileReader(TEXT));
        String line;
        Paragraph p;
        Font normal = new Font(FontFamily.TIMES_ROMAN, 12);
        Font bold = new Font(FontFamily.TIMES_ROMAN, 12, Font.BOLD);
        boolean title = true;
        while ((line = br.readLine()) != null) {
            p = new Paragraph(line, title ? bold : normal);
            p.setAlignment(Element.ALIGN_JUSTIFIED);
            events.setTitle(title);
            document.add(p);
            title = line.isEmpty();
        }
        document.close();
    }

Source: [developers.itextpdf.com](http://developers.itextpdf.com/content/itext-7-examples/itext-7-differences-itext-5/event-handlers-and-renderers#2866-text2pdfpageevents.java)

  [1]: http://i.stack.imgur.com/pUSv1.png

## Text2PdfPageEvents1.java (iText 7)
Suppose that you have the following text file: [jekyll_hyde.txt](http://gitlab.itextsupport.com/itext7/samples/raw/develop/sandbox/src/main/resources/txt/jekyll_hyde.txt)

How do we convert it to a PDF that looks like this:

[![enter image description here][1]][1]

Note the page numbers at the bottom of each page. These are added using an `IEventHandler` implementation:

    protected class Footer implements IEventHandler {
 
        @Override
        public void handleEvent(Event event) {
            PdfDocumentEvent docEvent = (PdfDocumentEvent) event;
            PdfDocument pdf = docEvent.getDocument();
            PdfPage page = docEvent.getPage();
            Rectangle pageSize = page.getPageSize();
            PdfCanvas pdfCanvas = new PdfCanvas(
                page.getLastContentStream(), page.getResources(), pdf);
            Canvas canvas = new Canvas(pdfCanvas, pdf, pageSize);
            float x = (pageSize.getLeft() + pageSize.getRight()) / 2;
            float y = pageSize.getBottom() + 15;
            canvas.showTextAligned(
                String.valueOf(pdf.getPageNumber(page)),
                x, y, TextAlignment.CENTER);
        }
    }

We can reuse the https://www.wikiod.com/itext/text-to-pdf-itext-5-versus-itext-7#Text2Pdf.java (iText 7) example with only two minor changes:

    public void createPdf(String dest) throws IOException {
        PdfDocument pdf = new PdfDocument(new PdfWriter(dest));
        pdf.addEventHandler(PdfDocumentEvent.END_PAGE, new Footer());
        Document document = new Document(pdf)
            .setTextAlignment(TextAlignment.JUSTIFIED);
        BufferedReader br = new BufferedReader(new FileReader(TEXT));
        String line;
        PdfFont normal = PdfFontFactory.createFont(FontConstants.TIMES_ROMAN);
        PdfFont bold = PdfFontFactory.createFont(FontConstants.TIMES_BOLD);
        boolean title = true;
        Border border = new SolidBorder(Color.BLUE, 1);
        while ((line = br.readLine()) != null) {
            document.add(new Paragraph(line)
                .setFont(title ? bold : normal)
                .setBorder(title ? border : Border.NO_BORDER));
            title = line.isEmpty();
        }
        document.close();
    }

We add an event handler that will trigger the `handleEvent()` method of the `Footer` class every time a page ends. We also define a border for the `Paragraph` objects that are used for a title.

Source: [developers.itextpdf.com](http://developers.itextpdf.com/content/itext-7-examples/itext-7-differences-itext-5/event-handlers-and-renderers#2867-text2pdfpageevents1.java) and the [iText 7: Building Blocks](http://pages.itextpdf.com/iText-7-abc.html) tutorial.

  [1]: http://i.stack.imgur.com/kkRGg.png

## Text2PdfPageEvents2.java
Suppose that you have the following text file: [jekyll_hyde.txt](http://gitlab.itextsupport.com/itext7/samples/raw/develop/sandbox/src/main/resources/txt/jekyll_hyde.txt)

How do we convert it to a PDF that looks like this:

[![enter image description here][1]][1]

Note that this is very similar to what we had before, but the border of the titles now has rounded corners. We created a custom `ParagraphRenderer` to achieve this, and we created a `TitleParagraph` object that uses that renderer:

    public class TitleParagraph extends Paragraph {
 
        public TitleParagraph(String line) {
            super(line);
            try {
                setFont(PdfFontFactory.createFont(FontConstants.TIMES_BOLD));
            }
            catch (IOException ioe) {
            }
        }
 
        @Override
        protected IRenderer makeNewRenderer() {
            return new ParagraphRenderer(this) {
                @Override
                public void drawBorder(DrawContext drawContext) {
                    Rectangle occupiedAreaBBox = getOccupiedAreaBBox();
                    float[] margins = getMargins();
                    Rectangle rectangle = applyMargins(occupiedAreaBBox, margins, false);
                    PdfCanvas canvas = drawContext.getCanvas();
                    canvas.roundRectangle(rectangle.getX() - 1, rectangle.getY() - 1,
                    rectangle.getWidth() + 2, rectangle.getHeight() + 2, 5).stroke();
                }
            };
        } 
    }

Our code to convert text to PDF is very simple now. We no longer have to set the font to bold for the titles and we no longer have to define a border:

    public void createPdf(String dest) throws IOException {
        PdfDocument pdf = new PdfDocument(new PdfWriter(dest));
        pdf.addEventHandler(PdfDocumentEvent.END_PAGE, new Footer());
        Document document = new Document(pdf)
            .setTextAlignment(TextAlignment.JUSTIFIED);
        BufferedReader br = new BufferedReader(new FileReader(TEXT));
        String line;
        PdfFont normal = PdfFontFactory.createFont(FontConstants.TIMES_ROMAN);
        boolean title = true;
        Border border = new SolidBorder(Color.BLUE, 1);
        while ((line = br.readLine()) != null) {
            if (title)
                document.add(new TitleParagraph(line));
            else
                document.add(new Paragraph(line).setFont(normal));
            title = line.isEmpty();
        }
        document.close();
    }

Source: [developers.itextpdf.com](http://developers.itextpdf.com/content/itext-7-examples/itext-7-differences-itext-5/event-handlers-and-renderers#2868-text2pdfpageevents2.java) and the [iText 7: Building Blocks](http://pages.itextpdf.com/iText-7-abc.html) tutorial.


  [1]: http://i.stack.imgur.com/jq3IX.png

