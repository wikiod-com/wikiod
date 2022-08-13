---
title: "Picasso"
slug: "picasso"
draft: false
images: []
weight: 9525
type: docs
toc: true
---

[Picasso][1] is an image library for Android. It's created and maintained by [Square][2]. It simplifies the process of displaying images from external locations. The library handles every stage of the process, from the initial HTTP request to the caching of the image. In many cases, only a few lines of code are required to implement this neat library.

[1]: http://square.github.io/picasso/
[2]: http://square.github.io/

Picasso is a powerful image downloading and caching library for Android.  
Follow [this example][1] to add the library to your project.

Websites:

 - [Source][2]
 - [Doc][3]
 - [Change Log][4] 


  [1]: https://www.wikiod.com/android/picasso#Adding Picasso Library to your Android Project
  [2]: https://github.com/square/picasso
  [3]: http://square.github.io/picasso/
  [4]: https://github.com/square/picasso/blob/master/CHANGELOG.md

## Circular Avatars with Picasso
Here is an example Picasso Circle Transform class based on [the original][1], with the addition of a thin border, and also includes functionality for an optional separator for stacking:

    import android.graphics.Bitmap;
    import android.graphics.BitmapShader;
    import android.graphics.Canvas;
    import android.graphics.Color;
    import android.graphics.Paint;
    import android.graphics.Paint.Style;
    
    import com.squareup.picasso.Transformation;
    
    public class CircleTransform implements Transformation {
    
        boolean mCircleSeparator = false;
    
        public CircleTransform(){
        }
    
        public CircleTransform(boolean circleSeparator){
            mCircleSeparator = circleSeparator;
        }
    
        @Override
        public Bitmap transform(Bitmap source) {
            int size = Math.min(source.getWidth(), source.getHeight());
    
            int x = (source.getWidth() - size) / 2;
            int y = (source.getHeight() - size) / 2;
    
            Bitmap squaredBitmap = Bitmap.createBitmap(source, x, y, size, size);
    
            if (squaredBitmap != source) {
                source.recycle();
            }
    
            Bitmap bitmap = Bitmap.createBitmap(size, size, source.getConfig());
    
            Canvas canvas = new Canvas(bitmap);
            BitmapShader shader = new BitmapShader(squaredBitmap, BitmapShader.TileMode.CLAMP, BitmapShader.TileMode.CLAMP);
            Paint paint = new Paint(Paint.ANTI_ALIAS_FLAG | Paint.DITHER_FLAG | Paint.FILTER_BITMAP_FLAG);
            paint.setShader(shader);
    
            float r = size/2f;
            canvas.drawCircle(r, r, r-1, paint);
    
            // Make the thin border:
            Paint paintBorder = new Paint();
            paintBorder.setStyle(Style.STROKE);
            paintBorder.setColor(Color.argb(84,0,0,0));
            paintBorder.setAntiAlias(true);
            paintBorder.setStrokeWidth(1);
            canvas.drawCircle(r, r, r-1, paintBorder);
    
            // Optional separator for stacking:
            if (mCircleSeparator) {
                Paint paintBorderSeparator = new Paint();
                paintBorderSeparator.setStyle(Style.STROKE);
                paintBorderSeparator.setColor(Color.parseColor("#ffffff"));
                paintBorderSeparator.setAntiAlias(true);
                paintBorderSeparator.setStrokeWidth(4);
                canvas.drawCircle(r, r, r+1, paintBorderSeparator);
            }
    
            squaredBitmap.recycle();
            return bitmap;
        }
    
        @Override
        public String key() {
            return "circle";
        }
    }


Here is how to use it when loading an image (assuming `this` is an Activity Context, and `url` is a String with the url of the image to load):

    ImageView ivAvatar = (ImageView) itemView.findViewById(R.id.avatar);
    Picasso.with(this).load(url)
        .fit()
        .transform(new CircleTransform())
        .into(ivAvatar);


Result:

[![enter image description here][2]][2]


For use with the separator, give `true` to the constructor for the top image:

    ImageView ivAvatar = (ImageView) itemView.findViewById(R.id.avatar);
    Picasso.with(this).load(url)
        .fit()
        .transform(new CircleTransform(true))
        .into(ivAvatar);


Result (two ImageViews in a FrameLayout):

[![enter image description here][3]][3]


  [1]: https://gist.github.com/julianshen/5829333
  [2]: http://i.stack.imgur.com/tIMMe.png
  [3]: http://i.stack.imgur.com/vXOMg.png

## Adding Picasso Library to your Android Project
From the [official documentation][1]:

# Gradle.

    dependencies {
     compile "com.squareup.picasso:picasso:2.5.2"
    }

# Maven:

    <dependency>
      <groupId>com.squareup.picasso</groupId>
      <artifactId>picasso</artifactId>
      <version>2.5.2</version>
    </dependency>


  [1]: http://square.github.io/picasso/

## Placeholder and Error Handling
Picasso supports both download and error placeholders as optional features. Its also provides callbacks for handling the download result.

    Picasso.with(context)
      .load("YOUR IMAGE URL HERE")
      .placeholder(Your Drawable Resource)   //this is optional the image to display while the url image is downloading
      .error(Your Drawable Resource)         //this is also optional if some error has occurred in downloading the image this image would be displayed
      .into(imageView, new Callback(){
         @Override
            public void onSuccess() {}

            @Override
            public void onError() {}
       });

A request will be retried three times before the error placeholder is shown.

## Re-sizing and Rotating
    Picasso.with(context)
     .load("YOUR IMAGE URL HERE")        
     .placeholder(DRAWABLE RESOURCE)   // optional        
     .error(DRAWABLE RESOURCE)         // optional        
     .resize(width, height)            // optional        
     .rotate(degree)                   // optional        
     .into(imageView);

## Disable cache in Picasso
    Picasso.with(context)
    .load(uri)
    .networkPolicy(NetworkPolicy.NO_CACHE)
    .memoryPolicy(MemoryPolicy.NO_CACHE)
    .placeholder(R.drawable.placeholder)
    .into(imageView);

## Using Picasso as ImageGetter for Html.fromHtml
Using Picasso as [ImageGetter][1] for [Html.fromHtml][2]

    public class PicassoImageGetter implements Html.ImageGetter {

    private TextView textView;

    private Picasso picasso;

    public PicassoImageGetter(@NonNull Picasso picasso, @NonNull TextView textView) {
        this.picasso = picasso;
        this.textView = textView;
    }

    @Override
    public Drawable getDrawable(String source) {
        Log.d(PicassoImageGetter.class.getName(), "Start loading url " + source);

        BitmapDrawablePlaceHolder drawable = new BitmapDrawablePlaceHolder();

        picasso
                .load(source)
                .error(R.drawable.connection_error)
                .into(drawable);

        return drawable;
    }

    private class BitmapDrawablePlaceHolder extends BitmapDrawable implements Target {

        protected Drawable drawable;

        @Override
        public void draw(final Canvas canvas) {
            if (drawable != null) {
                checkBounds();
                drawable.draw(canvas);
            }
        }

        public void setDrawable(@Nullable Drawable drawable) {
            if (drawable != null) {
                this.drawable = drawable;
                checkBounds();
            }
        }

        private void checkBounds() {
            float defaultProportion = (float) drawable.getIntrinsicWidth() / (float) drawable.getIntrinsicHeight();
            int width = Math.min(textView.getWidth(), drawable.getIntrinsicWidth());
            int height = (int) ((float) width / defaultProportion);

            if (getBounds().right != textView.getWidth() || getBounds().bottom != height) {

                setBounds(0, 0, textView.getWidth(), height); //set to full width

                int halfOfPlaceHolderWidth = (int) ((float) getBounds().right / 2f);
                int halfOfImageWidth = (int) ((float) width / 2f);

                drawable.setBounds(
                        halfOfPlaceHolderWidth - halfOfImageWidth, //centering an image
                        0,
                        halfOfPlaceHolderWidth + halfOfImageWidth,
                        height);

                textView.setText(textView.getText()); //refresh text
            }
        }

        //------------------------------------------------------------------//

        @Override
        public void onBitmapLoaded(Bitmap bitmap, Picasso.LoadedFrom from) {
            setDrawable(new BitmapDrawable(Application.getContext().getResources(), bitmap));
        }

        @Override
        public void onBitmapFailed(Drawable errorDrawable) {
            setDrawable(errorDrawable);
        }

        @Override
        public void onPrepareLoad(Drawable placeHolderDrawable) {
            setDrawable(placeHolderDrawable);
        }

        //------------------------------------------------------------------//

    }
    }

The usage is simple:

    Html.fromHtml(textToParse, new PicassoImageGetter(picasso, textViewTarget), null);


  [1]: https://developer.android.com/reference/android/text/Html.ImageGetter.html
  [2]: https://developer.android.com/reference/android/text/Html.html#fromHtml(java.lang.String,%20android.text.Html.ImageGetter,%20android.text.Html.TagHandler)

## Loading Image from external Storage
    String filename = "image.png";
    String imagePath = getExternalFilesDir() + "/" + filename;

    Picasso.with(context)
        .load(new File(imagePath))
        .into(imageView);

## Downloading image as Bitmap using Picasso
If you want to Download image as `Bitmap` using `Picasso` following code will help you:

    Picasso.with(mContext)
            .load(ImageUrl)
            .into(new Target() {
                @Override
                public void onBitmapLoaded(Bitmap bitmap, Picasso.LoadedFrom from) {
                   // Todo: Do something with your bitmap here
                }
    
                @Override
                public void onBitmapFailed(Drawable errorDrawable) {
                }
    
                @Override
                public void onPrepareLoad(Drawable placeHolderDrawable) {
                }
            });

## Cancelling Image Requests using Picasso
In certain cases we need to cancel an image download request in Picasso before the download has completed.

This could happen for various reasons, for example if the parent view transitioned to some other view before the image download could be completed. 

In this case, you can cancel the image download request using the `cancelRequest()` method:

    ImageView imageView; 

    //......

    Picasso.with(imageView.getContext()).cancelRequest(imageView);

## Try offline disk cache first, then go online and fetch the image
first add the OkHttp to the gradle build file of the app module 

    compile 'com.squareup.picasso:picasso:2.5.2'
    compile 'com.squareup.okhttp:okhttp:2.4.0'
    compile 'com.jakewharton.picasso:picasso2-okhttp3-downloader:1.0.2'

Then make a class extending Application

    import android.app.Application;
    
    import com.squareup.picasso.OkHttpDownloader;
    import com.squareup.picasso.Picasso;
    
    public class Global extends Application {
        @Override
        public void onCreate() {
            super.onCreate();
    
            Picasso.Builder builder = new Picasso.Builder(this);
            builder.downloader(new OkHttpDownloader(this,Integer.MAX_VALUE));
            Picasso built = builder.build();
            built.setIndicatorsEnabled(true);
            built.setLoggingEnabled(true);
            Picasso.setSingletonInstance(built);
    
        }
    }

add it to the Manifest file as follows :

    <application
            android:name=".Global"
            .. >
    
    </application>

<br>

**Normal Usage**


    Picasso.with(getActivity())
    .load(imageUrl)
    .networkPolicy(NetworkPolicy.OFFLINE)
    .into(imageView, new Callback() {
        @Override
        public void onSuccess() {
            //Offline Cache hit
        }
    
        @Override
        public void onError() {
            //Try again online if cache failed
            Picasso.with(getActivity())
                    .load(imageUrl)
                    .error(R.drawable.header)
                    .into(imageView, new Callback() {
                @Override
                public void onSuccess() {
                     //Online download
                }
    
                @Override
                public void onError() {
                    Log.v("Picasso","Could not fetch image");
                }
            });
        }
    });

[Link to original answer][1]


  [1]: http://stackoverflow.com/questions/23978828/how-do-i-use-disk-caching-in-picasso/30686992#30686992 "Original Answer"

