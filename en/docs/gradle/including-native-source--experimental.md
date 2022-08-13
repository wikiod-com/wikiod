---
title: "Including Native Source - Experimental"
slug: "including-native-source---experimental"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Parameters
| Parameters | Details |
| ------ | ------ |
| model.android.ndk.toolchain   | native toolchain found in the ndk-bundle folder   |

## Basic JNI Gradle Config
root: build.gradle

    buildscript {
        repositories {
            jcenter()
        }
        dependencies {
            classpath 'com.android.tools.build:gradle-experimental:0.8.0-alpha4'
        }
    }
    
    allprojects {
        repositories {
            jcenter()
        }
    }

app: build.gradle

    apply plugin: 'com.android.model.application'
    
    dependencies {
        compile "com.android.support:support-v4:23.3.0"
        compile fileTree(dir: 'libs', include: '*.jar')
    }
    
    model {
        android {
            compileSdkVersion = 23
            buildToolsVersion = '23.0.3'
    
            defaultConfig {
                applicationId = 'com.example.hello'
                minSdkVersion.apiLevel = 9
                targetSdkVersion.apiLevel = 23
    
                buildConfigFields {
                    create() {
                        type "int"
                        name "VALUE"
                        value "1"
                    }
                }
            }
    
            ndk {
                platformVersion = 9
                moduleName "hello"
    
                toolchain "clang"
    
                stl "gnustl_static"
                CFlags.add("-DANDROID_NDK")
                cppFlags.add("-std=c++11")
    
                ldLibs.add("android")
                ldLibs.add("dl")
                ldLibs.add("log")
            }
    
            sources {
                main {
                    jni {
                        exportedHeaders {
                            srcDirs "../../common/headers"
                        }
                        source {
                            srcDirs "../../common/src"
                        }
                    }
                }
            }
        }
    }

## Using prebuilt libraries and OpenGL ES 2.0
root: build.gradle

    buildscript {
        repositories {
            jcenter()
        }
        dependencies {
            classpath 'com.android.tools.build:gradle-experimental:0.8.0-alpha4'
        }
    }
    
    allprojects {
        repositories {
            jcenter()
        }
    }

app: build.gradle

    apply plugin: 'com.android.model.application'
    
    dependencies {
        compile "com.android.support:support-v4:23.3.0"
        compile fileTree(dir: 'libs', include: '*.jar')
    }
    
    model {
        android {
            compileSdkVersion = 23
            buildToolsVersion = '23.0.3'
    
            defaultConfig {
                applicationId = 'com.example.glworld'
                minSdkVersion.apiLevel = 9
                targetSdkVersion.apiLevel = 23
    
                buildConfigFields {
                    create() {
                        type "int"
                        name "VALUE"
                        value "1"
                    }
                }
            }
    
            buildTypes {
                release {
                    minifyEnabled = false
                    proguardFiles.add(file('proguard-rules.txt'))
                }
            }
    
            ndk {
                platformVersion = 9
                moduleName "glworld"
    
                toolchain "clang"
    
                stl "gnustl_static"
                CFlags.add("-DANDROID_NDK")
                CFlags.add("-DDISABLE_IMPORTGL")
                CFlags.add("-DFT2_BUILD_LIBRARY=1")
                cppFlags.add("-std=c++11")
    
                ldLibs.add("EGL")
                ldLibs.add("android")
                ldLibs.add("GLESv2")
                ldLibs.add("dl")
                ldLibs.add("log")
            }
    
            sources {
                main {
                    jni {
                        dependencies {
                            library "freetype2" linkage "shared"
                        }
                        exportedHeaders {
                            srcDirs "../../common/headers"
                        }
                        source {
                            srcDirs "../../common/src"
                        }
                    }
                }
            }
        }
    
        repositories {
            prebuilt(PrebuiltLibraries) {
                freetype2 {
                    headers.srcDir "../../common/freetype2-android/include"
                    binaries.withType(SharedLibraryBinary) {
                        def localLib = "../../common/freetype2-android/Android/libs"
                        sharedLibraryFile =
                                file("$localLib/${targetPlatform.getName()}/libfreetype2.so")
                    }
                }
            }
        }
    }
    
    // The next tasks compile a freetype library using a make file.
    // These `.so`'s are then used as the shared libraries compiled above.
    tasks.withType(JavaCompile) {
        compileTask -> compileTask.dependsOn buildNative
    }
    
    // Call regular ndk-build (.cmd) script from the app directory
    task buildNative(type: Exec) {
        def ndkDir = "/Development/android-sdk-macosx/ndk-bundle"
        commandLine "$ndkDir/ndk-build",
                '-C',
                file('../../common/freetype2-android/Android/jni').absolutePath
    }
    
    task cleanNative(type: Exec) {
        def ndkDir = "/Development/android-sdk-macosx/ndk-bundle"
        commandLine "$ndkDir/ndk-build",
                '-C',
                file('../../common/freetype2-android/Android/jni').absolutePath,
                "clean"
    }
    
    clean.dependsOn cleanNative

