---
title: "SQLiteOpenHelper"
slug: "sqliteopenhelper"
draft: false
images: []
weight: 9956
type: docs
toc: true
---

## SQLiteOpenHelper with fully qualified DB path name/databse in public folder 
Normally Android-SQLiteOpenHelper does not allow fully qualified path names where the database should be stored. So public database files are not possible.

You can use the SQLiteOpenHelper with a custom path if you provide a custom ContextClass and if you have write access in the target directory.

    public class DatabaseHelper extends SQLiteOpenHelper {
        private static final int DATABASE_VERSION = 3;
        .....

        DatabaseHelper(final Context context, String databaseName) 
        {
           super(new DatabaseContext(context), databaseName, null, DATABASE_VERSION);
        }
    }

And here is the custom DatabaseContext class that does all the magic 

    class DatabaseContext extends ContextWrapper {

        private static final String DEBUG_CONTEXT = "DatabaseContext";

        public DatabaseContext(Context base) {
            super(base);
        }

        @Override
        public File getDatabasePath(String name) 
        {
            File sdcard = Environment.getExternalStorageDirectory();    
            String dbfile = sdcard.getAbsolutePath() + File.separator+ "databases" + File.separator + name;
            if (!dbfile.endsWith(".db"))
            {
                dbfile += ".db" ;
            }

            File result = new File(dbfile);

            if (!result.getParentFile().exists())
            {
                result.getParentFile().mkdirs();
            }

            if (Log.isLoggable(DEBUG_CONTEXT, Log.WARN))
            {
                Log.w(DEBUG_CONTEXT,
                        "getDatabasePath(" + name + ") = " + result.getAbsolutePath());
            }

            return result;
        }

        /* this version is called for android devices >= api-11. thank to @damccull for fixing this. */
        @Override
        public SQLiteDatabase openOrCreateDatabase(String name, int mode, SQLiteDatabase.CursorFactory factory, DatabaseErrorHandler errorHandler) {
            return openOrCreateDatabase(name,mode, factory);
        }

        /* this version is called for android devices < api-11 */
        @Override
        public SQLiteDatabase openOrCreateDatabase(String name, int mode, SQLiteDatabase.CursorFactory factory) 
        {
            SQLiteDatabase result = SQLiteDatabase.openOrCreateDatabase(getDatabasePath(name), null);
            // SQLiteDatabase result = super.openOrCreateDatabase(name, mode, factory);
            if (Log.isLoggable(DEBUG_CONTEXT, Log.WARN))
            {
                Log.w(DEBUG_CONTEXT,
                        "openOrCreateDatabase(" + name + ",,) = " + result.getPath());
            }
            return result;
        }
    }

This is a copy of my answer to http://stackoverflow.com/questions/5332328/sqliteopenhelper-problem-with-fully-qualified-db-path-name/9168969#9168969

