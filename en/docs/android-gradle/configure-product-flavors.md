---
title: "Configure Product Flavors"
slug: "configure-product-flavors"
draft: false
images: []
weight: 9925
type: docs
toc: true
---

The product flavors support the same properties as `defaultConfig` this is because defaultConfig actually belongs to the ProductFlavor class. This means you can provide the base configuration for all flavors in the `defaultConfig {}` block, and each flavor can override any of these default values, such as the a`pplicationId`.




## Flavor Constants and Resources in build.gradle
You can use gradle to have `BuildConfig` constants and `res` values on a per flavor basis. Just add the value to the flavor you want to support.

    android {
        defaultConfig {
            resValue "string", "app_name", "Full App"
            buildConfigField "boolean", "isDemo", "false"
        }
        productFlavors {
            demo {
                resValue "String", "app_name", "Demo App"
                buildConfigField "boolean", "isDemo", "true"
            }
            full {
                // use default values
            }
        }
    }

Gradle will do all the merging / overriding for you. The generated code will also allow you to see where the values come from, e.g.

    <!-- Values from default config. -->
    <string name="app_name" translatable="false">Default Name</string>

and

    public final class BuildConfig {
        public static final String VERSION_NAME = "1.0";
        // Fields from product flavor: demo
        public static final boolean isDemo = true;
    }

## How to configure the build.gradle file
    android {
        ...
        defaultConfig {...}
        buildTypes {...}
        productFlavors {
            demo {
                applicationId "com.example.myapp.demo"
                versionName "1.0-demo"
            }
            full {
                applicationId "com.example.myapp.full"
                versionName "1.0-full"
            }
        }
    }

## Add dependencies for flavors
You can add different dependencies  for a specific product flavor.

Just use the  `<flavorName>Compile 'group:name:x.y.z'` syntax:

    android {
        ...    
        productFlavors {
            flavor1 {
               //.....
            }
            flavor2 {
               //.....
            }
        }
    }
    
    ...
    dependencies {

        compile 'com.android.support:appcompat-v7:24.2.0'
       
        // Add a dependency only for flavor1
        flavor1Compile 'group:name:x.y.z'

        // Add a dependency only for flavor2
        flavor2Compile 'group:name:x.y.z'
    } 

## Using Flavor Dimension
When the app is based on more than one criteria, instead of creating a lot of flavors you can define flavor dimensions.

The flavor dimensions define the cartesian product that will be used to produce variants.

Example:

    flavorDimensions("dimA", "dimB")
    
    productFlavors {
    
        row1 {
            ...
            dimension = "dimA"
        }
        row2 {
            ...
            dimension = "dimA"
        }
        row3 {
             ...
            dimension = "dimA"
        }
    
        col1 {
            ...
            dimension = "dimB"
        }
        col2 {
            ...
            dimension = "dimB"
        }
        col3 {
             ...
            dimension = "dimB"
        }
    }


This config will produce 18 (3*3*2) variants (if you have the 2 standard build types : `debug` and `release`).
The following build variants will be created:

    row1-col1-debug 
    row1-col2-debug 
    row1-col3-debug 
    row1-col1-release
    row1-col2-release
    row1-col3-release
    
    row2-col1-debug 
    row2-col2-debug 
    row2-col3-debug 
    row2-col1-release
    row2-col2-release
    row2-col3-release
    
    row3-col1-debug 
    row3-col2-debug 
    row3-col3-debug 
    row3-col1-release
    row3-col2-release
    row3-col3-release

The **order of the dimension** is defined by `android.flavorDimensions` and
**drives which flavor override the other**, which is important for resources when a value in a flavor replaces a value defined in a lower priority flavor.

The flavor dimension is defined with higher priority first. So in this case:

    dimA > dimB > defaultConfig

There is also a "flavor combination" source folder available when more than one flavor dimension is used. For instance `src/flavor1Flavor2/`.  
- Note that this is for all combinations of all dimensions.
- Its priority is higher than single-flavor sourcesets, but lower than build-types.

## Develop and Production Product Flavors Example
    productFlavors {
            // Define separate dev and prod product flavors.
            dev {
                // dev utilizes minSDKVersion = 21 to allow the Android gradle plugin
                // to pre-dex each module and produce an APK that can be tested on
                // Android Lollipop without time consuming dex merging processes.
                minSdkVersion 21
            }
            prod {
                // The actual minSdkVersion for the application.
                minSdkVersion 15
            }
        }



