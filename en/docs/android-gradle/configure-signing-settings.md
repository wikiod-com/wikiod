---
title: "Configure Signing Settings"
slug: "configure-signing-settings"
draft: false
images: []
weight: 9932
type: docs
toc: true
---

## Define the signing configuration in an external file
You can define the signing configuration in an external file as a `signing.properties` in the root directory of your project.

For example you can define these keys (you can use your favorite names):

    STORE_FILE=myStoreFileLocation    
    STORE_PASSWORD=myStorePassword
    KEY_ALIAS=myKeyAlias
    KEY_PASSWORD=mykeyPassword
    
Then in your build.gradle file:


    android {
    
        signingConfigs {
            release
        }
        
         buildTypes {
            release {
                signingConfig signingConfigs.release
            }
         }
    }

Then you can introduce some checks to avoid gradle issues in the build process.

    //------------------------------------------------------------------------------
    // Signing
    //------------------------------------------------------------------------------
    def Properties props = new Properties()
    def propFile = file('../signing.properties')
    if (propFile.canRead()) {
    
        if (props != null && props.containsKey('STORE_FILE') && props.containsKey('STORE_PASSWORD') &&
                props.containsKey('KEY_ALIAS') && props.containsKey('KEY_PASSWORD')) {
    
            android.signingConfigs.release.storeFile = file(props['STORE_FILE'])
            android.signingConfigs.release.storePassword = props['STORE_PASSWORD']
            android.signingConfigs.release.keyAlias = props['KEY_ALIAS']
            android.signingConfigs.release.keyPassword = props['KEY_PASSWORD']
        } else {
            android.buildTypes.release.signingConfig = null
        }
    } else {
        android.buildTypes.release.signingConfig = null
    }



## Define the signing configuration setting environment variables
You can store the signing information setting environment variables.  
These values can be accessed with `System.getenv("<VAR-NAME>")`

In your `build.gradle` you can define:

    signingConfigs {
        release {
            storeFile file(System.getenv("KEYSTORE"))
            storePassword System.getenv("KEYSTORE_PASSWORD")
            keyAlias System.getenv("KEY_ALIAS")
            keyPassword System.getenv("KEY_PASSWORD")
        }
    }

## Define signing configuration in a separate gradle file
The simplest and cleanest way to add an external configuration is through a separate Gradle file

**build.gradle**

    apply from: './keystore.gradle'
    android{
        signingConfigs {
            release {
                storeFile file(keystore.storeFile)
                storePassword keystore.storePassword
                keyAlias keystore.keyAlias
                keyPassword keystore.keyPassword
            }
        }
    }

**keystore.gradle**

    ext.keystore = [
        storeFile    : "/path/to/your/file",
        storePassword: 'password of the store',
        keyAlias     : 'alias_of_the_key',
        keyPassword  : 'password_of_the_key'
    ]

The keystore.gradle file can exist anywhere in your file system, you can specify its location inside the `apply from: ''` at the top of your gradle file or at the end of your main project build.gradle file.

Typically its a good idea to ignore this file from version control system such as git if its located inside your repo.

It is also a good idea to provide a sample `keystore.gradle.sample` which developers entering the project would rename and populate on their development machine. This file would always be contained inside the repo at the correct location.

## Configure the build.gradle with signing configuration
You can define the  signing configuration to sign the apk in the `build.gradle` file.

You can define:

- `storeFile` : the keystore file
- `storePassword`: the keystore password
- `keyAlias`: a key alias name
- `keyPassword`: A key alias password

You have to **define** the `signingConfigs` block to create a signing configuration:

    android {
        signingConfigs {
    
            myConfig {
                storeFile file("myFile.keystore")
                storePassword "myPasswork"
                keyAlias "aKeyAlias"
                keyPassword "myAliasPassword"
            }
        }
        //....
    }

Then you can **assign** it to one or more build types.

    android {
    
        buildTypes {
            release {
                signingConfig signingConfigs.myConfig
            }
        }
    }

