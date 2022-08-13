---
title: "Drawables"
slug: "drawables"
draft: false
images: []
weight: 9952
type: docs
toc: true
---

## Tint a drawable
A drawable can be tinted a certain color. This is useful for supporting different themes within your application, and reducing the number of drawable resource files.

Using framework APIs on SDK 21+:

    Drawable d = context.getDrawable(R.drawable.ic_launcher);
    d.setTint(Color.WHITE);

Using android.support.v4 library on SDK 4+:

    
    //Load the untinted resource
    final Drawable drawableRes = ContextCompat.getDrawable(context, R.drawable.ic_launcher);
    //Wrap it with the compatibility library so it can be altered
    Drawable tintedDrawable = DrawableCompat.wrap(drawableRes);
    //Apply a coloured tint
    DrawableCompat.setTint(tintedDrawable, Color.WHITE);
    //At this point you may use the tintedDrawable just as you usually would 
    //(and drawableRes can be discarded)

    //NOTE: If your original drawableRes was in use somewhere (i.e. it was the result of 
    //a call to a `getBackground()` method then at this point you still need to replace 
    //the background. setTint does *not* alter the instance that drawableRes points to, 
    //but instead creates a new drawable instance

Please not that `int color` is **not** referring to a color Resource, however you are not limited to those colours defined in the 'Color' class. When you have a colour defined in your XML which you want to use you must just first get it's value.

_You can replace usages of `Color.WHITE` using the methods below_

When targetting older API's:

    getResources().getColor(R.color.your_color);

Or on newer targets:

    ContextCompat.getColor(context, R.color.your_color);



## Custom Drawable
Extend your class with Drawable and override these methods

    public class IconDrawable extends Drawable {
        /**
         * Paint for drawing the shape
         */
        private Paint paint;
        /**
         * Icon drawable to be drawn to the center of the shape
         */
        private Drawable icon;
        /**
         * Desired width and height of icon
         */
        private int desiredIconHeight, desiredIconWidth;
    
        /**
         * Public constructor for the Icon drawable
         *
         * @param icon            pass the drawable of the icon to be drawn at the center
         * @param backgroundColor background color of the shape
         */
        public IconDrawable(Drawable icon, int backgroundColor) {
            this.icon = icon;
            paint = new Paint(Paint.ANTI_ALIAS_FLAG);
            paint.setColor(backgroundColor);
            desiredIconWidth = 50;
            desiredIconHeight = 50;
        }
    
        @Override
        public void draw(Canvas canvas) {
            //if we are setting this drawable to a 80dpX80dp imageview 
            //getBounds will return that measurements,we can draw according to that width.
            Rect bounds = getBounds();
            //drawing the circle with center as origin and center distance as radius
            canvas.drawCircle(bounds.centerX(), bounds.centerY(), bounds.centerX(), paint);
            //set the icon drawable's bounds to the center of the shape
            icon.setBounds(bounds.centerX() - (desiredIconWidth / 2), bounds.centerY() - (desiredIconHeight / 2), (bounds.centerX() - (desiredIconWidth / 2)) + desiredIconWidth, (bounds.centerY() - (desiredIconHeight / 2)) + desiredIconHeight);
            //draw the icon to the bounds
            icon.draw(canvas);
    
        }
    
        @Override
        public void setAlpha(int alpha) {
            //sets alpha to your whole shape
            paint.setAlpha(alpha);
        }
    
        @Override
        public void setColorFilter(ColorFilter colorFilter) {
           //sets color filter to your whole shape
            paint.setColorFilter(colorFilter);
        }
    
        @Override
        public int getOpacity() {
            //give the desired opacity of the shape
            return PixelFormat.TRANSLUCENT;
        }
    }

Declare a ImageView in your layout

    <ImageView
       android:layout_width="80dp"
       android:id="@+id/imageView"
       android:layout_height="80dp" />

Set your custom drawable to the ImageView 

   

     IconDrawable iconDrawable=new IconDrawable(ContextCompat.getDrawable(this,android.R.drawable.ic_media_play),ContextCompat.getColor(this,R.color.pink_300));
     imageView.setImageDrawable(iconDrawable);

Screenshot

[![enter image description here][1]][1]  [![enter image description here][2]][2]  [![enter image description here][3]][3]


  [1]: https://i.stack.imgur.com/OG8Z7.png
  [2]: https://i.stack.imgur.com/mYxYf.png
  [3]: https://i.stack.imgur.com/yopxv.png

## Circular View
For a circular View (in this case `TextView`) create a drawble **round_view.xml** in **drawble** folder:

    <?xml version="1.0" encoding="utf-8"?>
    <shape
        xmlns:android="http://schemas.android.com/apk/res/android"
        android:shape="oval">
        <solid android:color="#FAA23C" />
        <stroke android:color="#FFF" android:width="2dp" />
    </shape>

Assign the drawable to the View:

        <TextView
            android:id="@+id/game_score"
            android:layout_width="60dp"
            android:layout_height="60dp"
            android:background="@drawable/round_score"
            android:padding="6dp"
            android:text="100"
            android:textColor="#fff"
            android:textSize="20sp"
            android:textStyle="bold"
            android:gravity="center" />

Now it should look like the orange circle:

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/9jbcT.png

## Make View with rounded corners
Create **drawable** file named with **custom_rectangle.xml** in **drawable** folder:
    
  

    <?xml version="1.0" encoding="utf-8"?>
    <shape xmlns:android="http://schemas.android.com/apk/res/android"
        android:shape="rectangle" >
    
        <solid android:color="@android:color/white" />
    
        <corners android:radius="10dip" />
    
        <stroke
            android:width="1dp"
            android:color="@android:color/white" />
    
    </shape>

Now apply **rectangle background** on **View**:

    mView.setBackGround(R.drawlable.custom_rectangle);

**Reference screenshot:**

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/8O2uU.png

