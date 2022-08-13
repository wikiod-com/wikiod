---
title: "Creating Images Programmatically"
slug: "creating-images-programmatically"
draft: false
images: []
weight: 9870
type: docs
toc: true
---

[`BufferedImage.getGraphics()`](https://docs.oracle.com/javase/8/docs/api/java/awt/image/BufferedImage.html#getGraphics--) always returns [`Graphics2D`](https://docs.oracle.com/javase/8/docs/api/java/awt/Graphics2D.html).

Using a [`VolatileImage`](https://docs.oracle.com/javase/8/docs/api/java/awt/image/VolatileImage.html) may significantly improve the speed of drawing operations, but also has its drawbacks: its contents may be lost at any moment and they may have to be redrawn from scratch.

## Creating a simple image programmatically and displaying it
    class ImageCreationExample {
      
        static Image createSampleImage() {
            // instantiate a new BufferedImage (subclass of Image) instance 
            BufferedImage img = new BufferedImage(640, 480, BufferedImage.TYPE_INT_ARGB);
            
            //draw something on the image
            paintOnImage(img);
            
            return img;
        }
    
        static void paintOnImage(BufferedImage img) {
            // get a drawable Graphics2D (subclass of Graphics) object 
            Graphics2D g2d = (Graphics2D) img.getGraphics();
            
            // some sample drawing
            g2d.setColor(Color.BLACK);
            g2d.fillRect(0, 0, 640, 480);
            g2d.setColor(Color.WHITE);
            g2d.drawLine(0, 0, 640, 480);
            g2d.drawLine(0, 480, 640, 0);
            g2d.setColor(Color.YELLOW);
            g2d.drawOval(200, 100, 240, 280);
            g2d.setColor(Color.RED);
            g2d.drawRect(150, 70, 340, 340);
            
            // drawing on images can be very memory-consuming
            // so it's better to free resources early
            // it's not necessary, though
            g2d.dispose();
        }
    
        public static void main(String[] args) {
            JFrame frame = new JFrame();
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            Image img = createSampleImage();
            ImageIcon icon = new ImageIcon(img);
            frame.add(new JLabel(icon));
            frame.pack();
            frame.setVisible(true);
        }
    }

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/IBEHO.png

## Save an Image to disk

    public static void saveImage(String destination) throws IOException {
        // method implemented in "Creating a simple image Programmatically and displaying it" example
        BufferedImage img = createSampleImage();

        // ImageIO provides several write methods with different outputs
        ImageIO.write(img, "png", new File(destination));
    }


## Setting individual pixel's color in BufferedImage


## Specifying image rendering quality

    static void setupQualityHigh(Graphics2D g2d) {
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
        // many other RenderingHints KEY/VALUE pairs to specify
    }


    static void setupQualityLow(Graphics2D g2d) {
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF);
        g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_SPEED);
    }
    

A comparison of QUALITY and SPEED rendering of the sample image:
[![low-quality render][1]][1]
[![high-quality render][2]][2]


  [1]: http://i.stack.imgur.com/Bg2bS.png
  [2]: http://i.stack.imgur.com/sqHhS.png

## Creating an image with BufferedImage class


## Editing and re-using image with BufferedImage


## How to scale a BufferedImage
     /**
     * Resizes an image using a Graphics2D object backed by a BufferedImage.
     * @param srcImg - source image to scale
     * @param w - desired width
     * @param h - desired height
     * @return - the new resized image
     */
    private BufferedImage getScaledImage(Image srcImg, int w, int h){

        //Create a new image with good size that contains or might contain arbitrary alpha values between and including 0.0 and 1.0.
        BufferedImage resizedImg = new BufferedImage(w, h, BufferedImage.TRANSLUCENT);

        //Create a device-independant object to draw the resized image
        Graphics2D g2 = resizedImg.createGraphics();

        //This could be changed, Cf. https://www.wikiod.com/java/creating-images-programmatically#Specifying image rendering quality
        g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);

        //Finally draw the source image in the Graphics2D with the desired size.
        g2.drawImage(srcImg, 0, 0, w, h, null);

        //Disposes of this graphics context and releases any system resources that it is using
        g2.dispose();
    
        //Return the image used to create the Graphics2D 
        return resizedImg;
    }

