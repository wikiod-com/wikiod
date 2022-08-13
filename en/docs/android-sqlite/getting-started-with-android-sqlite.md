---
title: "Getting started with android-sqlite"
slug: "getting-started-with-android-sqlite"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Basic usage
To include a database in your app, you typically derive a class from [SQLiteOpenHelper](https://developer.android.com/reference/android/database/sqlite/SQLiteOpenHelper.html):

<!-- lanugage-all: lang-java -->

    public class HelloDBHelper extends SQLiteOpenHelper {
        private static final int DATABASE_VERSION = 1;
        private static final int DATABASE_NAME = "hello";

        HelloDBHelper(Context context) {
            super(context, DATABASE_NAME, null, DATABASE_VERSION);
        }

        @Override
        public void onCreate(SQLiteDatabase db) {
            db.execSQL("CREATE TABLE ...");
            ...
        }
    }

This helper class is responsible for opening (and creating/updating, if needed) the database.
Use it to get an [SQLiteDatabase](https://developer.android.com/reference/android/database/sqlite/SQLiteDatabase.html) object to access the data:

    SQLiteDatabase db = helper.getReadableDatabase();
    Cursor c = db.query(...);
    while (c.moveToNext()) {
        String name = c.getString(0);
        ...
    }
<!-- -->

    SQLiteDatabase db = helper.getWritableDatabase();
    ContentValues cv = new ContentValues();
    cv.put("column", value);
    ...
    db.insertOrThrow("table", null, cv);

