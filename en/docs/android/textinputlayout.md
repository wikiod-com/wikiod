---
title: "TextInputLayout"
slug: "textinputlayout"
draft: false
images: []
weight: 9657
type: docs
toc: true
---

TextInputLayout was introduced to display the floating label on EditText. The EditText has to be wrapped by TextInputLayout in order to display the floating label. 

[`TextInputLayout`][1] is a layout which wraps an `EditText` (or descendant) to show a floating label when the hint is hidden due to the user inputting text. Additonally the `TextInputLayout` enables you to display an error message below the `EditText`.

Make sure the following dependency is added to your app's `build.gradle` file under dependencies: 

    compile 'com.android.support:design:25.3.1'


  [1]: https://developer.android.com/reference/android/support/design/widget/TextInputLayout.html

## Password Visibility Toggles
With an input password type, you can also [enable an icon that can show or hide][1] the entire text using the [`passwordToggleEnabled`][2] attribute. 

You can also customize same default using these attributes:
- [`passwordToggleDrawable`][3]: to change the default eye icon  
- [`passwordToggleTint`][4]: to apply a tint to the password visibility toggle drawable.
- [`passwordToggleTintMode`][5]: to specify the blending mode used to apply the background tint. 

Example:

    <android.support.design.widget.TextInputLayout
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            app:passwordToggleContentDescription="@string/description"
            app:passwordToggleDrawable="@drawable/another_toggle_drawable"
            app:passwordToggleEnabled="true">
    
                <EditText/>
    
    </android.support.design.widget.TextInputLayout>


  [1]: https://material.google.com/components/text-fields.html#text-fields-password-input
  [2]: https://developer.android.com/reference/android/support/design/widget/TextInputLayout.html#setPasswordVisibilityToggleEnabled(boolean)
  [3]: https://developer.android.com/reference/android/support/design/widget/TextInputLayout.html#setPasswordVisibilityToggleDrawable(android.graphics.drawable.Drawable)
  [4]: https://developer.android.com/reference/android/support/design/widget/TextInputLayout.html#setPasswordVisibilityToggleTintList(android.content.res.ColorStateList)
  [5]: https://developer.android.com/reference/android/support/design/widget/TextInputLayout.html#setPasswordVisibilityToggleTintMode(android.graphics.PorterDuff.Mode)

## Adding Character Counting
The TextInputLayout has a [character counter][1] for an EditText defined within it.  
The counter will be rendered below the EditText.

Just use the [`setCounterEnabled()`][2] and [`setCounterMaxLength`][3] methods:

    TextInputLayout til = (TextInputLayout) findViewById(R.id.username);
    til.setCounterEnabled(true);
    til.setCounterMaxLength(15);


or the [`app:counterEnabled`][4] and [`app:counterMaxLength`][5] attributes in the xml.

    <android.support.design.widget.TextInputLayout
        app:counterEnabled="true"
        app:counterMaxLength="15">
    
        <EditText/>
    
    </android.support.design.widget.TextInputLayout>


  [1]: https://material.google.com/components/text-fields.html#text-fields-character-counter
  [2]: https://developer.android.com/reference/android/support/design/widget/TextInputLayout.html#setCounterEnabled(boolean)
  [3]: https://developer.android.com/reference/android/support/design/widget/TextInputLayout.html#setCounterMaxLength(int)
  [4]: https://developer.android.com/reference/android/support/design/widget/TextInputLayout.html#attr_android.support.design:counterEnabled
  [5]: https://developer.android.com/reference/android/support/design/widget/TextInputLayout.html#attr_android.support.design:counterMaxLength

## Handling Errors
You can use the `TextInputLayout` to display error messages according to the [material design guidelines][1] using the [`setError`][2] and [`setErrorEnabled`][3]methods.

In order to show the error below the EditText use:

    TextInputLayout til = (TextInputLayout) findViewById(R.id.username);
    til.setErrorEnabled(true);
    til.setError("You need to enter a name");

To enable error in the `TextInputLayout` you can eithr use  `app:errorEnabled="true"` in xml or `til.setErrorEnabled(true);` as shown above.

You will obtain:

[![enter image description here][4]][4]


  [1]: https://material.google.com/patterns/errors.html#errors-user-input-errors
  [2]: https://developer.android.com/reference/android/support/design/widget/TextInputLayout.html#setError(java.lang.CharSequence)
  [3]: https://developer.android.com/reference/android/support/design/widget/TextInputLayout.html#setErrorEnabled(boolean)
  [4]: http://i.stack.imgur.com/kbuWP.png

## Basic usage
It is the basic usage of the `TextInputLayout`.  
Make sure to add the dependency in the `build.gradle` file as described in the remarks section.

Example:

     <android.support.design.widget.TextInputLayout
             android:layout_width="match_parent"
             android:layout_height="wrap_content">
    
         <EditText
                 android:layout_width="match_parent"
                 android:layout_height="wrap_content"
                 android:hint="@string/username"/>
     
     </android.support.design.widget.TextInputLayout>

## TextInputEditText
The [`TextInputEditText`][1] is an `EditText` with an extra fix to display a hint in the IME when in ['extract' mode][2].

The **Extract mode** is the mode that the keyboard editor switches to when you click on an EditText when the space is too small (for example landscape on a smartphone).  
In this case, using an `EditText` while you are editing the text you can see that the IME doesn't give you a hint of what you're editing

The `TextInputEditText` fixes this issue providing hint text while the user’s device’s IME is in Extract mode.

Example:

    <android.support.design.widget.TextInputLayout
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        android:hint="Description"
        >
        <android.support.design.widget.TextInputEditText
            android:id="@+id/description"
            android:layout_width="match_parent"
            android:layout_height="wrap_content"/>

    </android.support.design.widget.TextInputLayout>

  [1]: https://developer.android.com/reference/android/support/design/widget/TextInputEditText.html
  [2]: http://developer.android.com/reference/android/inputmethodservice/InputMethodService.html#FullscreenMode

## Customizing the appearance of the TextInputLayout
You can customize the appearance of the `TextInputLayout` and its embedded `EditText`by defining custom styles in your `styles.xml`. The defined styles can either be added as styles or themes to your `TextInputLayout`.

Example for customizing the hint appearance:

`styles.xml`:

    <!--Floating label text style-->  
    <style name="MyHintStyle" parent="TextAppearance.AppCompat.Small">  
        <item name="android:textColor">@color/black</item>
    </style>
    
    <!--Input field style-->  
    <style name="MyEditText" parent="Theme.AppCompat.Light">  
        <item name="colorControlNormal">@color/indigo</item>
        <item name="colorControlActivated">@color/pink</item>
    </style>  

To Apply Style update your TextInputLayout And EditText as follows

    <android.support.design.widget.TextInputLayout  
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        app:hintTextAppearance="@style/MyHintStyle">
    
        <EditText
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:hint="@string/Title"
            android:theme="@style/MyEditText" />
    
    </android.support.design.widget.TextInputLayout>  

Example to customize the accent color of the `TextInputLayout`. The accent color affects the color of the baseline of the `EditText` and the text color for the floating hint text: 

`styles.xml`:

    <style name="TextInputLayoutWithPrimaryColor" parent="Widget.Design.TextInputLayout">
        <item name="colorAccent">@color/primary</item>
    </style> 

layout file:

    <android.support.design.widget.TextInputLayout
                android:id="@+id/textInputLayout_password"
                android:layout_width="match_parent"
                android:layout_height="wrap_content"
                android:theme="@style/TextInputLayoutWithPrimaryColor">

                <android.support.design.widget.TextInputEditText
                    android:id="@+id/textInputEditText_password"
                    android:layout_width="match_parent"
                    android:layout_height="wrap_content"
                    android:hint="@string/login_hint_password"
                    android:inputType="textPassword" />

    </android.support.design.widget.TextInputLayout>
 

