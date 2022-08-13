---
title: "Camera and Gallery"
slug: "camera-and-gallery"
draft: false
images: []
weight: 9948
type: docs
toc: true
---

## Taking full-sized photo from camera


## Take photo


## How to start camera or gallery and save camera result to storage
First of all you need `Uri` and temp Folders and request codes :

   
    public final int REQUEST_SELECT_PICTURE = 0x01;
    public final int REQUEST_CODE_TAKE_PICTURE = 0x2;
    public static String TEMP_PHOTO_FILE_NAME ="photo_";
    Uri mImageCaptureUri;
    File mFileTemp;

Then init mFileTemp :

    public void initTempFile(){
        String state = Environment.getExternalStorageState();
        if (Environment.MEDIA_MOUNTED.equals(state)) {

            mFileTemp = new File(Environment.getExternalStorageDirectory() + File.separator
                    + getResources().getString(R.string.app_foldername) + File.separator
                    + getResources().getString(R.string.pictures_folder)
                    , TEMP_PHOTO_FILE_NAME
                    + System.currentTimeMillis() + ".jpg");
            mFileTemp.getParentFile().mkdirs();
        } else {
            mFileTemp = new File(getFilesDir() + File.separator
                    + getResources().getString(R.string.app_foldername)
                    + File.separator + getResources().getString(R.string.pictures_folder)
                    , TEMP_PHOTO_FILE_NAME + System.currentTimeMillis() + ".jpg");
            mFileTemp.getParentFile().mkdirs();
        }
    }

Opening `Camera` and `Gallery` intents :

    public void openCamera(){
        Intent intent = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
        try {
            mImageCaptureUri = null;
            String state = Environment.getExternalStorageState();
            if (Environment.MEDIA_MOUNTED.equals(state)) {
                mImageCaptureUri = Uri.fromFile(mFileTemp);

            } else {

                mImageCaptureUri = InternalStorageContentProvider.CONTENT_URI;

            }
            intent.putExtra(MediaStore.EXTRA_OUTPUT, mImageCaptureUri);
            intent.putExtra("return-data", true);
            startActivityForResult(intent, REQUEST_CODE_TAKE_PICTURE);
        } catch (Exception e) {

            Log.d("error", "cannot take picture", e);
        }
    }

    public void openGallery(){
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN
                && ActivityCompat.checkSelfPermission(this, Manifest.permission.READ_EXTERNAL_STORAGE)
                != PackageManager.PERMISSION_GRANTED) {
            requestPermission(Manifest.permission.READ_EXTERNAL_STORAGE,
                    getString(R.string.permission_read_storage_rationale),
                    REQUEST_STORAGE_READ_ACCESS_PERMISSION);
        } else {
            Intent intent = new Intent();
            intent.setType("image/*");
            intent.setAction(Intent.ACTION_GET_CONTENT);
            intent.addCategory(Intent.CATEGORY_OPENABLE);
            startActivityForResult(Intent.createChooser(intent, getString(R.string.select_image)), REQUEST_SELECT_PICTURE);
        }

    }

Then in `onActivityResult` method :

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {

        if (resultCode != RESULT_OK) {
            return;
        }
        Bitmap bitmap;

        switch (requestCode) {

            case REQUEST_SELECT_PICTURE:
                try {
                    Uri uri = data.getData();
                    try {
                        bitmap = MediaStore.Images.Media.getBitmap(getContentResolver(), uri);
                        Bitmap bitmapScaled = Bitmap.createScaledBitmap(bitmap, 800, 800, true);
                        Drawable drawable=new BitmapDrawable(bitmapScaled);
                        mImage.setImageDrawable(drawable);
                        mImage.setVisibility(View.VISIBLE);
                    } catch (IOException e) {
                        Log.v("act result", "there is an error : "+e.getContent());
                    }
                } catch (Exception e) {
                    Log.v("act result", "there is an error : "+e.getContent());
                }
                break;
            case REQUEST_CODE_TAKE_PICTURE:
                try{
                   Bitmap bitmappicture = MediaStore.Images.Media.getBitmap(getContentResolver() , mImageCaptureUri);
                   mImage.setImageBitmap(bitmappicture);
                   mImage.setVisibility(View.VISIBLE);
               }catch (IOException e){
                  Log.v("error camera",e.getMessage());
               }
               break; 
        }
        super.onActivityResult(requestCode, resultCode, data);
    }

You need theese permissions in `AndroidManifest.xml` :

    <uses-permission android:name="android.permission.READ_EXTERNAL_STORAGE" />
    <uses-permission android:name="android.permission.WRITE_EXTERNAL_STORAGE" />
    <uses-permission android:name="android.permission.CAMERA" />

And you need to handle [runtime permissions][1] such as Read/Write external storage etc ...

I am checking `READ_EXTERNAL_STORAGE` permission in my `openGallery` method :

My `requestPermission` method :

    protected void requestPermission(final String permission, String rationale, final int requestCode) {
        if (ActivityCompat.shouldShowRequestPermissionRationale(this, permission)) {
            showAlertDialog(getString(R.string.permission_title_rationale), rationale,
                    new DialogInterface.OnClickListener() {
                        @Override
                        public void onClick(DialogInterface dialog, int which) {
                            ActivityCompat.requestPermissions(BasePermissionActivity.this,
                                    new String[]{permission}, requestCode);
                        }
                    }, getString(android.R.string.ok), null, getString(android.R.string.cancel));
        } else {
            ActivityCompat.requestPermissions(this, new String[]{permission}, requestCode);
        }
    }

Then Override  `onRequestPermissionsResult` method :

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        switch (requestCode) {
            case REQUEST_STORAGE_READ_ACCESS_PERMISSION:
                if (grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                    handleGallery();
                }
                break;
            default:
                super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        }
    }

`showAlertDialog` method :

    protected void showAlertDialog(@Nullable String title, @Nullable String message,
                                   @Nullable DialogInterface.OnClickListener onPositiveButtonClickListener,
                                   @NonNull String positiveText,
                                   @Nullable DialogInterface.OnClickListener onNegativeButtonClickListener,
                                   @NonNull String negativeText) {
        AlertDialog.Builder builder = new AlertDialog.Builder(this);
        builder.setTitle(title);
        builder.setMessage(message);
        builder.setPositiveButton(positiveText, onPositiveButtonClickListener);
        builder.setNegativeButton(negativeText, onNegativeButtonClickListener);
        mAlertDialog = builder.show();
    }


  [1]: https://developer.android.com/training/permissions/requesting.html

## Set camera resolution
Set High resolution programmatically. 


    Camera mCamera = Camera.open();
    Camera.Parameters params = mCamera.getParameters();
    
    // Check what resolutions are supported by your camera
    List<Size> sizes = params.getSupportedPictureSizes();
    
    // Iterate through all available resolutions and choose one.
    // The chosen resolution will be stored in mSize.
    Size mSize;
    for (Size size : sizes) {
        Log.i(TAG, "Available resolution: "+size.width+" "+size.height);
            mSize = size;
        }
    }
    
    Log.i(TAG, "Chosen resolution: "+mSize.width+" "+mSize.height);
    params.setPictureSize(mSize.width, mSize.height);
    mCamera.setParameters(params); 

## Decode bitmap correctly rotated from the uri fetched with the intent
    private static final String TAG = "IntentBitmapFetch";
    private static final String COLON_SEPARATOR = ":";
    private static final String IMAGE = "image";
    
    @Nullable
    public Bitmap getBitmap(@NonNull Uri bitmapUri, int maxDimen) {
        InputStream is = context.getContentResolver().openInputStream(bitmapUri);
        Bitmap bitmap = BitmapFactory.decodeStream(is, null, getBitmapOptions(bitmapUri, maxDimen));
        
        int imgRotation = getImageRotationDegrees(bitmapUri);

        int endRotation = (imgRotation < 0) ? -imgRotation : imgRotation;
        endRotation %= 360;
        endRotation = 90 * (endRotation / 90);
        if (endRotation > 0 && bitmap != null) {
            Matrix m = new Matrix();
            m.setRotate(endRotation);
            Bitmap tmp = Bitmap.createBitmap(bitmap, 0, 0, bitmap.getWidth(), bitmap.getHeight(), m, true);
            if (tmp != null) {
                bitmap.recycle();
                bitmap = tmp;
            }
        }

        return bitmap;
    }
    
    private BitmapFactory.Options getBitmapOptions(Uri uri, int imageMaxDimen){
        BitmapFactory.Options options = new BitmapFactory.Options();
        if (imageMaxDimen > 0) {
            options.inJustDecodeBounds = true;
            decodeImage(null, uri, options);
            options.inSampleSize = calculateScaleFactor(options, imageMaxDimen);
            options.inJustDecodeBounds = false;
            options.inPreferredConfig = Bitmap.Config.RGB_565;
            addInBitmapOptions(options);
        }
    }

    private int calculateScaleFactor(@NonNull BitmapFactory.Options bitmapOptionsMeasureOnly, int imageMaxDimen) {
        int inSampleSize = 1;
        if (bitmapOptionsMeasureOnly.outHeight > imageMaxDimen || bitmapOptionsMeasureOnly.outWidth > imageMaxDimen) {
            final int halfHeight = bitmapOptionsMeasureOnly.outHeight / 2;
            final int halfWidth = bitmapOptionsMeasureOnly.outWidth / 2;
            while ((halfHeight / inSampleSize) > imageMaxDimen && (halfWidth / inSampleSize) > imageMaxDimen) {
                inSampleSize *= 2;
            }
        }
        return inSampleSize;
    }
    
        public int getImageRotationDegrees(@NonNull Uri imgUri) {
        int photoRotation = ExifInterface.ORIENTATION_UNDEFINED;

        try {
            boolean hasRotation = false;
            //If image comes from the gallery and is not in the folder DCIM (Scheme: content://)
            String[] projection = {MediaStore.Images.ImageColumns.ORIENTATION};
            Cursor cursor = context.getContentResolver().query(imgUri, projection, null, null, null);
            if (cursor != null) {
                if (cursor.getColumnCount() > 0 && cursor.moveToFirst()) {
                    photoRotation = cursor.getInt(cursor.getColumnIndex(projection[0]));
                    hasRotation = photoRotation != 0;
                    Log.d("Cursor orientation: "+ photoRotation);
                }
                cursor.close();
            }

            //If image comes from the camera (Scheme: file://) or is from the folder DCIM (Scheme: content://)
            if (!hasRotation) {
                ExifInterface exif = new ExifInterface(getAbsolutePath(imgUri));
                int exifRotation = exif.getAttributeInt(ExifInterface.TAG_ORIENTATION,
                        ExifInterface.ORIENTATION_NORMAL);
                switch (exifRotation) {
                    case ExifInterface.ORIENTATION_ROTATE_90: {
                        photoRotation = 90;
                        break;
                    }
                    case ExifInterface.ORIENTATION_ROTATE_180: {
                        photoRotation = 180;
                        break;
                    }
                    case ExifInterface.ORIENTATION_ROTATE_270: {
                        photoRotation = 270;
                        break;
                    }
                }
                Log.d(TAG, "Exif orientation: "+ photoRotation);
            }
        } catch (IOException e) {
            Log.e(TAG, "Error determining rotation for image"+ imgUri, e);
        }
        return photoRotation;
    }

    @TargetApi(Build.VERSION_CODES.KITKAT)
    private String getAbsolutePath(Uri uri) {
        //Code snippet edited from: http://stackoverflow.com/a/20559418/2235133
        String filePath = uri.getPath();
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT && DocumentsContract.isDocumentUri(context, uri)) {
            // Will return "image:x*"
            String[] wholeID = TextUtils.split(DocumentsContract.getDocumentId(uri), COLON_SEPARATOR);
            // Split at colon, use second item in the array
            String type = wholeID[0];
            if (IMAGE.equalsIgnoreCase(type)) {//If it not type image, it means it comes from a remote location, like Google Photos
                String id = wholeID[1];
                String[] column = {MediaStore.Images.Media.DATA};
                // where id is equal to
                String sel = MediaStore.Images.Media._ID + "=?";
                Cursor cursor = context.getContentResolver().
                        query(MediaStore.Images.Media.EXTERNAL_CONTENT_URI,
                                column, sel, new String[]{id}, null);
                if (cursor != null) {
                    int columnIndex = cursor.getColumnIndex(column[0]);
                    if (cursor.moveToFirst()) {
                        filePath = cursor.getString(columnIndex);
                    }
                    cursor.close();
                }
                Log.d(TAG, "Fetched absolute path for uri" + uri);
            }
        }
        return filePath;
    }

