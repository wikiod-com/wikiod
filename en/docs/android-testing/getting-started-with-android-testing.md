---
title: "Getting started with android-testing"
slug: "getting-started-with-android-testing"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Test Types
Android tests are based on JUnit, and you can run them either as local unit tests on the JVM or as instrumented tests on an Android device. This page provides an introduction to the concepts and tools for building Android tests

 - Local unit tests (Located at module-name/src/test/java/)
 - Instrumented tests (Located at module-name/src/androidTest/java/)

**Local unit tests JUnit**

    @RunWith(AndroidJUnit4.class)
    @LargeTest
    public class MainActivityInstrumentationTest {
    
        @Rule
        public ActivityTestRule mActivityRule = new ActivityTestRule<>(
                MainActivity.class);
    
        @Test
        public void sayHello(){
            onView(withText("Say hello!")).perform(click());
    
            onView(withId(R.id.textView)).check(matches(withText("Hello, World!")));
        }
    }

**Instrumented tests Example** 
dependencies {
    androidTestCompile 'com.android.support:support-annotations:24.0.0'
    androidTestCompile 'com.android.support.test:runner:0.5'
    androidTestCompile 'com.android.support.test:rules:0.5'
    // Optional -- Hamcrest library
    androidTestCompile 'org.hamcrest:hamcrest-library:1.3'
    // Optional -- UI testing with Espresso
    androidTestCompile 'com.android.support.test.espresso:espresso-core:2.2.2'
    // Optional -- UI testing with UI Automator
    androidTestCompile 'com.android.support.test.uiautomator:uiautomator-v18:2.1.2'
}

android {
    defaultConfig {
        testInstrumentationRunner "android.support.test.runner.AndroidJUnitRunner"
    }
}

**Instrumented Unit Test Class**

    import android.os.Parcel;
    import android.support.test.runner.AndroidJUnit4;
    import android.util.Pair;
    import org.junit.Test;
    import org.junit.runner.RunWith;
    import java.util.List;
    import static org.hamcrest.Matchers.is;
    import static org.junit.Assert.assertThat;
    
    @RunWith(AndroidJUnit4.class)
    @SmallTest
    public class LogHistoryAndroidUnitTest {
    
        public static final String TEST_STRING = "This is a string";
        public static final long TEST_LONG = 12345678L;
        private LogHistory mLogHistory;
    
        @Before
        public void createLogHistory() {
            mLogHistory = new LogHistory();
        }
    
        @Test
        public void logHistory_ParcelableWriteRead() {
            // Set up the Parcelable object to send and receive.
            mLogHistory.addEntry(TEST_STRING, TEST_LONG);
    
            // Write the data.
            Parcel parcel = Parcel.obtain();
            mLogHistory.writeToParcel(parcel, mLogHistory.describeContents());
    
            // After you're done with writing, you need to reset the parcel for reading.
            parcel.setDataPosition(0);
    
            // Read the data.
            LogHistory createdFromParcel = LogHistory.CREATOR.createFromParcel(parcel);
            List<Pair<String, Long>> createdFromParcelData = createdFromParcel.getData();
    
            // Verify that the received data is correct.
            assertThat(createdFromParcelData.size(), is(1));
            assertThat(createdFromParcelData.get(0).first, is(TEST_STRING));
            assertThat(createdFromParcelData.get(0).second, is(TEST_LONG));
        }
    }

