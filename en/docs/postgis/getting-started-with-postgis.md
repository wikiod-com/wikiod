---
title: "Getting started with postgis"
slug: "getting-started-with-postgis"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation from Source (with Postgres 9.1 or higher)
This guide is explicitly for PostgreSQL 9.1 or higher on linux machines. It uses the postgres extensions-feature that will greatly improve the import of extensions to an existing postgres-installation. If you have to work with an older version of postgres, please refer to the [official documentations][1].

**Resolve dependencies**

PostGIS is a complex project that has a number of dependencies. In order to proceed with the manual configuration and build procedure, you will have to resolve these dependencies and install the following packages either manually or via the package-managers. 

*Minimum Requirements*

 * [PostgreSQL][2] 9.1 or higher. It is important, that you install the database including server headers, usually found in the *dev*-packages of the package-manager of your repository. 
 * The GNU C Compiler [gcc][3].
 * GNU [make][4]. To complete the build-process.
 * [Proj4][5]. A projection library, for coordniate transformations.
 * [GEOS][6]. A geometry library, that implements feature descriptions and simple geometries. Version 3.5 or highter is recommended in order to use newer functions such as *ST_ClipByBox2D* and *ST_Subdivide*.
 * [GDAL][7], version 1.9 or higher. A library that implements abstract data-formats for raster- and vector-based geospatial data.
 * [LibXML2][8], version 2.5 or higher. A libraray to work with XML, XSLT and DTD.
 * [JSON-C][9], version 0.9 or higher. A library to create output in the JSON-format

*Optional Requirements*

 * [GTK][10] (requires GTK+2.0, 2.8+) to compile the shp2pgsql-gui.
 * [SFCGAL][11], version 1.1 (or higher) could be used to provide additional 2D and 3D advanced analysis functions to PostGIS.
 * [PCRE][12]. A library for regular expression pattern matching using the Perl 5 syntax. This library is needed if you want to work with the [Adress Standardizer][13].
 * [CUnit][14]. A unit-testing utility, needed for regression testing.
 * [DocBook][15] (xsltproc) is required for building the documentation.
 * [DBLatex][16] is required for building the documentation in PDF format. 
 * [ImageMagick][17] is required to generate the images used in the documentation.

**Get the Sources**

In order to get the source code, download the latest tarball:

    wget http://postgis.net/stuff/postgis-2.3.2dev.tar.gz
    tar -xvzf postgis-2.3.2dev.tar.gz

or use the official SVN-repository:

    svn checkout http://svn.osgeo.org/postgis/trunk/ postgis-2.3.2dev

**Configuration**

If you obtained the sources via SVN, you can prepare the config-script with:

    ./autogen.sh

In order to configure the build-process for your specific machine, run in the project folder:

    ./configure

There are several optional parameters for the configuration-step. Please refer to the [official documentation][18] for detailed instructions, this is usually optional and only for servers that use non-default installations.

**Build**

Once the configuration-step has finished successfully, a makefile will be created. To start the build-process run:

    make

The last output should be:

    "PostGIS was built successfully. Ready to install."

As of version 1.4.0, all the functions have comments generated from the documentation. If you wish to install these comments into your spatial databases later, run the command which requires docbook.

    make comments

**Installation**

Install all extensions with:

    make install

The PostGIS extensions are built and installed automatically if you are using PostgreSQL 9.1 or higher. You can install the necessary extensions manually, if you have different setup.

In the project folder:

    cd extensions
    cd postgis
    make clean
    make
    make install

    cd ..
    cd postgis_topology
    make clean
    make
    make install

    cd ..
    cd postgis_sfcgal
    make clean
    make
    make install

    cd ..
    cd address_standardizer
    make clean
    make
    make install
    make installcheck

    cd ..
    cd postgis_tiger_geocoder
    make clean
    make
    make install
    make installcheck

If you want to install the extensions manually on a different machine, copy the following files from the `extensions` folder into the `PostgreSQL/share/extension`-folder of the target. For each extension:

    scp extensions/[EXTENSION]/sql/*.sql user@target:[POSTGIS_PATH]/share/extension

where *[EXTENSION]* is the selected extension (postgis, postgis_topology, postgis_sfcgal, address_standardizer, postgis_tiger_geocoder) and *[POSTGIS_PATH]* is the PostGIS installation-path on your target-machine.

**Verifiying the Installation**

If you don't have a running postgres database service, [setup your postgres database first][19]. Connect to the database, using:

    su postgres -c psql

To verify that the extensions are accessible, run the following queries in a psql-session:

    SELECT name, default_version,installed_version FROM pg_available_extensions WHERE name LIKE 'postgis%' or name LIKE 'address%';

The output should look like this:

                 name             | default_version | installed_version
    ------------------------------+-----------------+-------------------
     address_standardizer         | 2.3.2dev        | 2.3.2dev
     address_standardizer_data_us | 2.3.2dev        | 2.3.2dev
     postgis                      | 2.3.2dev        | 2.3.2dev
     postgis_sfcgal               | 2.3.2dev        |
     postgis_tiger_geocoder       | 2.3.2dev        | 2.3.2dev
     postgis_topology             | 2.3.2dev        |

    (6 rows)

To perform an in-depth post-installation test, run the following command in your project-folder:

    make check

This will run through various checks and tests using the generated library against an actual PostgreSQL database.
 


  [1]: http://postgis.net/documentation/
  [2]: https://www.postgresql.org/
  [3]: https://www.wikiod.com/gcc/getting-started-with-gcc
  [4]: https://www.wikiod.com/makefile/getting-started-with-makefile#Basic Makefile
  [5]: http://trac.osgeo.org/proj/
  [6]: http://trac.osgeo.org/geos/
  [7]: http://trac.osgeo.org/gdal/wiki/DownloadSource
  [8]: http://xmlsoft.org/downloads.html
  [9]: https://github.com/json-c/json-c/releases
  [10]: https://www.gtk.org/
  [11]: https://github.com/Oslandia/SFCGAL
  [12]: http://www.pcre.org
  [13]: http://postgis.net/docs/manual-2.3/Address_Standardizer.html
  [14]: http://cunit.sourceforge.net/
  [15]: http://www.docbook.org/
  [16]: http://dblatex.sourceforge.net/
  [17]: http://www.imagemagick.org/
  [18]: http://postgis.net/docs/manual-2.3/postgis_installation.html
  [19]: https://wiki.postgresql.org/wiki/Detailed_installation_guides

## A geospatial "Hello World"
In this example we will set up a geospatial database, import data from 2 different sources, and view the results in an application called QGIS. This guide is explicitly written for linux-machines, if you operate on another platform, some commands or paths might not work as expected.

In order to view the imported data we will use an application called [QGIS][1]. If you don't have this application, [please install it first][2], if you like to use another viewer or geo-application (like ArcGIS) you can skip installing QGIS.

Our Sources will be the [New York City State Assembly Districts][3] and the [New York City LION Street Map Database][4]. Please Download the appropriate files frome the linked locations. You should also take a look at the Metadata-section of the material, since it gives you information on what coordinate reference system these files use.

To start, create a working folder "nycgis", copy the downloaded files to the location and unzip the archives.

    mkdir nycgis
    cd nycgis
    cp ~/Downloads/nyad_16d.zip .
    unzip ~/Downloads/nyad_16d.zip
    cp ~/Downloads/nylion_16d.zip .
    unzip  ~/Downloads/nylion_16d.zip

In the "nycgis" folder you should now have 2 folders: "nyad_16d", "lion" with several files. 

When working with geo data it is of vital importance, to know the coordinate reference system (CRS) of your source data, and of your final output data. In the Metadata-sections of the linked locations obove ([Metadata: Assembly Districts][5], [Metadata: LION Database][6]), you will find that the CRS for both files is EPSG:2263, a coordinate system used to reference the north eastern US.

Lets assume we want to use a different CRS in our database. This can have different reasons, we might want to work with a web-based geo application on the database for example. A common CRS for this kind of application is WGS:84 (EPSG:4326).

To convert the coordinate systems we use a tool called `ogr2ogr` wich is part of the GDAL package. In the working folder, we first create 2 folders representing the reprojected data, and then convert our data.

    mkdir nyad_16d_proj_4326
    ogr2ogr -f "ESRI Shapefile" ./nyad_16d_proj_4326/nyad_4326.shp ./nyad_16d/nyad_16d.shp -s_srs EPSG:2263 -t_srs EPSG:4326

    mkdir nylion_16d_proj_4326
    ogr2ogr -f "ESRI Shapefile" ./nylion_16d_proj_4326/ny_str_4326.shp ./nylion_16d/lion/lion.gdb/a0000000d.gdbtable -s_srs EPSG:2263 -t_srs EPSG:4326

Notice that we only use the file called: "a0000000d.gdbtable" of the LION-Database for our purposes. The syntax of the `ogr2ogr`-command is as follows:

    ogr2ogr -f [output-format] [output-file] [input-file] -s_srs [source crs] -t_srs [target crs]

We now have 2 shapefiles, projected in the correct CRS. In order to use the data in our database, we must convert the shapefiles to sql-statemens. For this we use a tool called `shp2pgsql`. In the working directory run the following commands:

    shp2pgsql ./nyad_16d_proj_4326/nyad_4326.shp > nyad_4326.sql
    shp2pgsql ./nylion_16d_proj_4326/ny_str_4326.shp > ny_streets_4326.sql

The files `nyad_4326.sql` and `ny_streets_4326.sql` are now ready to use in postgis. To proceed, and import the data create a spatially enabled database.

    sudo su - postgres
    createdb nycgis
    psql -d nycgis

In the psql-session, run:

    CREATE EXTENSION postgis;
    CREATE EXTENSION postgis_topology;

Quit the psql- and postgres-user-sessions with (CTRL+D). And import the files with: 

    psql -f nyad_4326.sql -d nycgis
    psql -f ny_streets_4326.sql -d nycgis

The database `nycgis` has now 2 tables in wich the reprojected sources were successfully imported.
 
To verify this: open QGIS 

 1. use *Layer* > *Add Layer* > *PostGIS-Layer*
 2. connect to your database
 3. select your tables
 4. (optional) set styling of the newly created layers.

[![QGIS-Screenshot][7]][7]

Et voilà: You now have a spatially enabled database, with imported, reprojected geodata.

  [1]: http://qgis.org/en/site/
  [2]: http://docs.qgis.org/2.14/en/docs/user_manual/introduction/getting_started.html#installation
  [3]: http://www1.nyc.gov/site/planning/data-maps/open-data/districts-download-metadata.page
  [4]: http://www1.nyc.gov/site/planning/data-maps/open-data.page#lion
  [5]: http://www1.nyc.gov/assets/planning/download/pdf/data-maps/open-data/nyad_metadata.pdf?ver=16da
  [6]: http://www1.nyc.gov/assets/planning/download/pdf/data-maps/open-data/lion_metadata.pdf?r=16d
  [7]: https://i.stack.imgur.com/ZAyht.jpg

## Installation via Package Manager
**Arch:**

An official pacman-package is available. Install the package as root, using:

    pacman -S postgis

**OpenSuse:**

In order to use the openSuse repositories for geospatial applications, enable the Geo-repository as root:

    zypper ar http://download.opensuse.org/repositories/Application:/Geo/openSUSE_[RELEASE]/ GEO 

where *[RELEASE]* is the official release number of your Suse-distribution. After this, you can install postgis with:

    zypper install postgis


## Setting up a geospatial database
To create a new empty database, run as postgres-user:

    createdb [yourdatabase]

Connect to the database with a psql-session:

    psql -d [yourdatabase]

In the psql-session run:

    CREATE EXTENSION postgis;
    CREATE EXTENSION postgis_topology;

to create the neccesary geospatial extensions. Once this is done, the database is a geospatially enabled database and it is ready to use.

