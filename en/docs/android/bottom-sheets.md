---
title: "Bottom Sheets"
slug: "bottom-sheets"
draft: false
images: []
weight: 9708
type: docs
toc: true
---

A bottom sheet is a sheet that slides up from the bottom edge of the screen.

[Bottom sheets][1] slide up from the bottom of the screen to reveal more content.  
They were added to the Android Support Library in v23.2.0 version.


  [1]: https://material.google.com/components/bottom-sheets.html

## BottomSheetBehavior like Google maps
<!-- if version [gte 2.1.x] -->

This example depends on Support Library 23.4.0.+.<br><br>

BottomSheetBehavior is characterized by :

 1. Two toolbars with animations that respond to the bottom sheet movements.
 2. A FAB that hides when it is near to the "modal toolbar" (the one that appears when you are sliding up).
 3. A backdrop image behind bottom sheet with some kind of parallax effect.
 4. A Title (TextView) in Toolbar that appears when bottom sheet reach it.
 5. The notification satus bar can turn its background to transparent or full color.
 6. A custom bottom sheet behavior with an "anchor" state.<br><br>


Now let's check them one by one:<br><br>

**ToolBars**<br>
When you open that view in Google Maps, you can see a toolbar in where you can search, it's the only one that I'm not doing exactly like Google Maps, because I wanted to do it more generic. Anyway that `ToolBar` is inside an `AppBarLayout` and it got hidden when you start dragging the BottomSheet and it appears again when the BottomSheet reach the `COLLAPSED` state.<br>
To achieve it you need to:<br>

 - create a `Behavior` and extend it from `AppBarLayout.ScrollingViewBehavior`
 - override `layoutDependsOn` and `onDependentViewChanged` methods. Doing it you will listen for bottomSheet movements.
 - create some methods to hide and unhide the AppBarLayout/ToolBar with animations.<br><br>

This is how I did it for first toolbar or ActionBar:<br>

    @Override
    public boolean layoutDependsOn(CoordinatorLayout parent, View child, View dependency) {
        return dependency instanceof NestedScrollView;
    }

    @Override
    public boolean onDependentViewChanged(CoordinatorLayout parent, View child,
                                          View dependency) {

        if (mChild == null) {
            initValues(child, dependency);
            return false;
        }

        float dVerticalScroll = dependency.getY() - mPreviousY;
        mPreviousY = dependency.getY();

        //going up
        if (dVerticalScroll <= 0 && !hidden) {
            dismissAppBar(child);
            return true;
        }

        return false;
    }

    private void initValues(final View child, View dependency) {

        mChild = child;
        mInitialY = child.getY();

        BottomSheetBehaviorGoogleMapsLike bottomSheetBehavior = BottomSheetBehaviorGoogleMapsLike.from(dependency);
        bottomSheetBehavior.addBottomSheetCallback(new BottomSheetBehaviorGoogleMapsLike.BottomSheetCallback() {
            @Override
            public void onStateChanged(@NonNull View bottomSheet, @BottomSheetBehaviorGoogleMapsLike.State int newState) {
                if (newState == BottomSheetBehaviorGoogleMapsLike.STATE_COLLAPSED ||
                        newState == BottomSheetBehaviorGoogleMapsLike.STATE_HIDDEN)
                    showAppBar(child);
            }

            @Override
            public void onSlide(@NonNull View bottomSheet, float slideOffset) {

            }
        });
    }

    private void dismissAppBar(View child){
        hidden = true;
        AppBarLayout appBarLayout = (AppBarLayout)child;
        mToolbarAnimation = appBarLayout.animate().setDuration(mContext.getResources().getInteger(android.R.integer.config_shortAnimTime));
        mToolbarAnimation.y(-(mChild.getHeight()+25)).start();
    }

    private void showAppBar(View child) {
        hidden = false;
        AppBarLayout appBarLayout = (AppBarLayout)child;
        mToolbarAnimation = appBarLayout.animate().setDuration(mContext.getResources().getInteger(android.R.integer.config_mediumAnimTime));
        mToolbarAnimation.y(mInitialY).start();
    }
[Here is the complete file if you need it][1]
<br><br>

The second Toolbar or "Modal" toolbar:<br>
You have to override the same methods, but in this one you have to take care of more behaviors:<br>

 - show/hide the ToolBar with animations
 - change status bar color/background
 - show/hide the BottomSheet title in the ToolBar
 - close the bottomSheet or send it to collapsed state


The code for this one is a little extensive, so I will let [the link][2]
<br><br>


**The FAB**<br><br>
This is a Custom Behavior too, but extends from `FloatingActionButton.Behavior`. In `onDependentViewChanged` you have to look when it reach the "offSet" or point in where you want to hide it. In my case I want to hide it when it's near to the second toolbar, so I dig into FAB parent (a CoordinatorLayout) looking for the AppBarLayout that contains the ToolBar, then I use the ToolBar position like `OffSet`:<br>

    @Override
    public boolean onDependentViewChanged(CoordinatorLayout parent, FloatingActionButton child, View dependency) {

        if (offset == 0)
            setOffsetValue(parent);

        if (dependency.getY() <=0)
            return false;

        if (child.getY() <= (offset + child.getHeight()) && child.getVisibility() == View.VISIBLE)
            child.hide();
        else if (child.getY() > offset && child.getVisibility() != View.VISIBLE)
            child.show();

        return false;
    }

[Complete Custom FAB Behavior link][3]<br><br>



**The image behind the BottomSheet with parallax effect**:<br>
Like the others, it's a custom behavior, the only "complicated" thing in this one is the little algorithm that keeps the image anchored to the BottomSheet and avoid the image collapse like default parallax effect:<br>

    @Override
    public boolean onDependentViewChanged(CoordinatorLayout parent, View child,
                                          View dependency) {

        if (mYmultiplier == 0) {
            initValues(child, dependency);
            return true;
        }

        float dVerticalScroll = dependency.getY() - mPreviousY;
        mPreviousY = dependency.getY();

        //going up
        if (dVerticalScroll <= 0 && child.getY() <= 0) {
            child.setY(0);
            return true;
        }

        //going down
        if (dVerticalScroll >= 0 && dependency.getY() <= mImageHeight)
            return false;

        child.setY( (int)(child.getY() + (dVerticalScroll * mYmultiplier) ) );

        return true;
    }
<br>[The complete file for backdrop image with parallax effect][4]
<br><br>

Now for the end: **The Custom BottomSheet Behavior**<br>
To achieve the 3 steps, first you need to understand that default BottomSheetBehavior has 5 states: `STATE_DRAGGING, STATE_SETTLING, STATE_EXPANDED, STATE_COLLAPSED, STATE_HIDDEN` and for the Google Maps behavior you need to add a middle state between collapsed and expanded: `STATE_ANCHOR_POINT`.<br>
I tried to extend the default bottomSheetBehavior with no success, so I just copy pasted all the code and modified what I need.<br>
To achieve what I'm talking about follow the next steps:<br><br>

 1. Create a Java class and extend it from `CoordinatorLayout.Behavior<V>`
 2. Copy paste code from default `BottomSheetBehavior ` file to your new one.
 3. Modify the method `clampViewPositionVertical` with the following code:<br>

        @Override
        public int clampViewPositionVertical(View child, int top, int dy) {
            return constrain(top, mMinOffset, mHideable ? mParentHeight : mMaxOffset);
        }
        int constrain(int amount, int low, int high) {
            return amount < low ? low : (amount > high ? high : amount);
        }

 4. Add a new state

    public static final int STATE_ANCHOR_POINT = X;

 5. Modify the next methods: `onLayoutChild`, `onStopNestedScroll`, `BottomSheetBehavior<V> from(V view)` and `setState` (optional)

<br><br>

    public boolean onLayoutChild(CoordinatorLayout parent, V child, int layoutDirection) {
        // First let the parent lay it out
        if (mState != STATE_DRAGGING && mState != STATE_SETTLING) {
            if (ViewCompat.getFitsSystemWindows(parent) &&
                    !ViewCompat.getFitsSystemWindows(child)) {
                ViewCompat.setFitsSystemWindows(child, true);
            }
            parent.onLayoutChild(child, layoutDirection);
        }
        // Offset the bottom sheet
        mParentHeight = parent.getHeight();
        mMinOffset = Math.max(0, mParentHeight - child.getHeight());
        mMaxOffset = Math.max(mParentHeight - mPeekHeight, mMinOffset);

        //if (mState == STATE_EXPANDED) {
        //    ViewCompat.offsetTopAndBottom(child, mMinOffset);
        //} else if (mHideable && mState == STATE_HIDDEN...
        if (mState == STATE_ANCHOR_POINT) {
            ViewCompat.offsetTopAndBottom(child, mAnchorPoint);
        } else if (mState == STATE_EXPANDED) {
            ViewCompat.offsetTopAndBottom(child, mMinOffset);
        } else if (mHideable && mState == STATE_HIDDEN) {
            ViewCompat.offsetTopAndBottom(child, mParentHeight);
        } else if (mState == STATE_COLLAPSED) {
            ViewCompat.offsetTopAndBottom(child, mMaxOffset);
        }
        if (mViewDragHelper == null) {
            mViewDragHelper = ViewDragHelper.create(parent, mDragCallback);
        }
        mViewRef = new WeakReference<>(child);
        mNestedScrollingChildRef = new WeakReference<>(findScrollingChild(child));
        return true;
    }


    public void onStopNestedScroll(CoordinatorLayout coordinatorLayout, V child, View target) {
        if (child.getTop() == mMinOffset) {
            setStateInternal(STATE_EXPANDED);
            return;
        }
        if (target != mNestedScrollingChildRef.get() || !mNestedScrolled) {
            return;
        }
        int top;
        int targetState;
        if (mLastNestedScrollDy > 0) {
            //top = mMinOffset;
            //targetState = STATE_EXPANDED;
            int currentTop = child.getTop();
            if (currentTop > mAnchorPoint) {
                top = mAnchorPoint;
                targetState = STATE_ANCHOR_POINT;
            }
            else {
                top = mMinOffset;
                targetState = STATE_EXPANDED;
            }
        } else if (mHideable && shouldHide(child, getYVelocity())) {
            top = mParentHeight;
            targetState = STATE_HIDDEN;
        } else if (mLastNestedScrollDy == 0) {
            int currentTop = child.getTop();
            if (Math.abs(currentTop - mMinOffset) < Math.abs(currentTop - mMaxOffset)) {
                top = mMinOffset;
                targetState = STATE_EXPANDED;
            } else {
                top = mMaxOffset;
                targetState = STATE_COLLAPSED;
            }
        } else {
            //top = mMaxOffset;
            //targetState = STATE_COLLAPSED;
            int currentTop = child.getTop();
            if (currentTop > mAnchorPoint) {
                top = mMaxOffset;
                targetState = STATE_COLLAPSED;
            }
            else {
                top = mAnchorPoint;
                targetState = STATE_ANCHOR_POINT;
            }
        }
        if (mViewDragHelper.smoothSlideViewTo(child, child.getLeft(), top)) {
            setStateInternal(STATE_SETTLING);
            ViewCompat.postOnAnimation(child, new SettleRunnable(child, targetState));
        } else {
            setStateInternal(targetState);
        }
        mNestedScrolled = false;
    }

    public final void setState(@State int state) {
        if (state == mState) {
            return;
        }
        if (mViewRef == null) {
            // The view is not laid out yet; modify mState and let onLayoutChild handle it later
            /**
             * New behavior (added: state == STATE_ANCHOR_POINT ||)
             */
            if (state == STATE_COLLAPSED || state == STATE_EXPANDED ||
                    state == STATE_ANCHOR_POINT ||
                    (mHideable && state == STATE_HIDDEN)) {
                mState = state;
            }
            return;
        }
        V child = mViewRef.get();
        if (child == null) {
            return;
        }
        int top;
        if (state == STATE_COLLAPSED) {
            top = mMaxOffset;
        } else if (state == STATE_ANCHOR_POINT) {
            top = mAnchorPoint;
        } else if (state == STATE_EXPANDED) {
            top = mMinOffset;
        } else if (mHideable && state == STATE_HIDDEN) {
            top = mParentHeight;
        } else {
            throw new IllegalArgumentException("Illegal state argument: " + state);
        }
        setStateInternal(STATE_SETTLING);
        if (mViewDragHelper.smoothSlideViewTo(child, child.getLeft(), top)) {
            ViewCompat.postOnAnimation(child, new SettleRunnable(child, state));
        }
    }


    public static <V extends View> BottomSheetBehaviorGoogleMapsLike<V> from(V view) {
        ViewGroup.LayoutParams params = view.getLayoutParams();
        if (!(params instanceof CoordinatorLayout.LayoutParams)) {
            throw new IllegalArgumentException("The view is not a child of CoordinatorLayout");
        }
        CoordinatorLayout.Behavior behavior = ((CoordinatorLayout.LayoutParams) params)
                .getBehavior();
        if (!(behavior instanceof BottomSheetBehaviorGoogleMapsLike)) {
            throw new IllegalArgumentException(
                    "The view is not associated with BottomSheetBehaviorGoogleMapsLike");
        }
        return (BottomSheetBehaviorGoogleMapsLike<V>) behavior;
    }

<br><br>

[Link to the whole project][5] where you can see all the Custom Behaviors<br><br>


**And here it is how it looks like:**<br>
[![CustomBottomSheetBehavior](https://raw.githubusercontent.com/miguelhincapie/CustomBottomSheetBehavior/master/CustomBottomSheetBehaviorLikeGoogleMaps3states.gif)]

<!-- end version if -->


  [1]: https://github.com/miguelhincapie/CustomBottomSheetBehavior/blob/master/app/src/main/java/co/com/parsoniisolutions/custombottomsheetbehavior/lib/ScrollingAppBarLayoutBehavior.java
  [2]: https://github.com/miguelhincapie/CustomBottomSheetBehavior/blob/master/app/src/main/java/co/com/parsoniisolutions/custombottomsheetbehavior/lib/MergedAppBarLayoutBehavior.java
  [3]: https://github.com/miguelhincapie/CustomBottomSheetBehavior/blob/master/app/src/main/java/co/com/parsoniisolutions/custombottomsheetbehavior/lib/ScrollAwareFABBehavior.java
  [4]: https://github.com/miguelhincapie/CustomBottomSheetBehavior/blob/master/app/src/main/java/co/com/parsoniisolutions/custombottomsheetbehavior/lib/BackdropBottomSheetBehavior.java
  [5]: https://github.com/miguelhincapie/CustomBottomSheetBehavior

## Modal bottom sheets with BottomSheetDialog
The [`BottomSheetDialog`][1] is a dialog styled as a bottom sheet

Just use:
    
    //Create a new BottomSheetDialog
    BottomSheetDialog dialog = new BottomSheetDialog(context);
    //Inflate the layout R.layout.my_dialog_layout
    dialog.setContentView(R.layout.my_dialog_layout);
    //Show the dialog
    dialog.show();

In this case you don't need to attach a BottomSheet behavior.

  [1]: https://developer.android.com/reference/android/support/design/widget/BottomSheetDialog.html


## Quick Setup
Make sure the following dependency is added to your app's build.gradle file under dependencies:

    compile 'com.android.support:design:25.3.1'

Then you can use the Bottom sheet using these options:

- [`BottomSheetBehavior`][1] to be used with `CoordinatorLayout`
- [`BottomSheetDialog`][2] which is a dialog with a bottom sheet behavior
- [`BottomSheetDialogFragment`][3] which is an extension of `DialogFragment`, that creates a `BottomSheetDialog` instead of a standard dialog.


  [1]: https://www.wikiod.com/android/bottom-sheets#Persistent Bottom Sheets
  [2]: https://www.wikiod.com/android/bottom-sheets#Modal bottom sheets with BottomSheetDialog
  [3]: https://www.wikiod.com/android/bottom-sheets#Modal bottom sheets with BottomSheetDialogFragment

## Modal bottom sheets with BottomSheetDialogFragment
You can realize a [modal bottom sheets][1] using a [`BottomSheetDialogFragment`][2].

The [`BottomSheetDialogFragment`][2] is a modal bottom sheet.  
This is a version of `DialogFragment` that shows a bottom sheet using `BottomSheetDialog` instead of a floating dialog.

Just define the fragment:

    public class MyBottomSheetDialogFragment extends BottomSheetDialogFragment {
    
        @Override
        public View onCreateView(LayoutInflater inflater, ViewGroup container,
                Bundle savedInstanceState) {
            return inflater.inflate(R.layout.my_fragment_bottom_sheet, container);
        }
    }

Then use this code to show the fragment:

    MyBottomSheetDialogFragment mySheetDialog = new MyBottomSheetDialogFragment();
    FragmentManager fm = getSupportFragmentManager();
    mySheetDialog.show(fm, "modalSheetDialog");

This Fragment will create a [`BottomSheetDialog`][3].


  [1]: https://material.google.com/components/bottom-sheets.html#bottom-sheets-modal-bottom-sheets
  [2]: https://developer.android.com/reference/android/support/design/widget/BottomSheetDialogFragment.html
  [3]: https://www.wikiod.com/android/bottom-sheets#Modal bottom sheets with BottomSheetDialog

## Persistent Bottom Sheets
You can achieve a [Persistent Bottom Sheet][2] attaching a [`BottomSheetBehavior`][3] to a child View of a [`CoordinatorLayout`][4]:

    <android.support.design.widget.CoordinatorLayout >

        <!-- .....   -->

        <LinearLayout
           android:id="@+id/bottom_sheet"
           android:elevation="4dp"
           android:minHeight="120dp"
           app:behavior_peekHeight="120dp"
           ...
           app:layout_behavior="android.support.design.widget.BottomSheetBehavior">

               <!-- .....   -->

           </LinearLayout>
    
    </android.support.design.widget.CoordinatorLayout>

Then in your code you can create a reference using:

     // The View with the BottomSheetBehavior  
     View bottomSheet = coordinatorLayout.findViewById(R.id.bottom_sheet);  
     BottomSheetBehavior mBottomSheetBehavior = BottomSheetBehavior.from(bottomSheet);  

You can set the state of your BottomSheetBehavior using the [setState()][5] method:

    mBottomSheetBehavior.setState(BottomSheetBehavior.STATE_EXPANDED);

You can use one of these states:

- [`STATE_COLLAPSED`][6]: this collapsed state is the default and shows just a portion of the layout along the bottom. The height can be controlled with the `app:behavior_peekHeight` attribute (defaults to 0)

- [`STATE_EXPANDED`][7]: the fully expanded state of the bottom sheet, where either the whole bottom sheet is visible (if its height is less than the containing `CoordinatorLayout`) or the entire `CoordinatorLayout` is filled

- [`STATE_HIDDEN`][8]: disabled by default (and enabled with the `app:behavior_hideable` attribute), enabling this allows users to swipe down on the bottom sheet to completely hide the bottom sheet

If you’d like to receive callbacks of state changes, you can add a `BottomSheetCallback`:

    mBottomSheetBehavior.setBottomSheetCallback(new BottomSheetCallback() {  
        @Override  
        public void onStateChanged(@NonNull View bottomSheet, int newState) {  
          // React to state change  
        }  
          @Override  
          public void onSlide(@NonNull View bottomSheet, float slideOffset) {  
           // React to dragging events  
       }  
     });  


 
  [2]: https://material.google.com/components/bottom-sheets.html#bottom-sheets-persistent-bottom-sheets
  [3]: https://developer.android.com/reference/android/support/design/widget/BottomSheetBehavior.html
  [4]: https://developer.android.com/reference/android/support/design/widget/CoordinatorLayout.html
  [5]: https://developer.android.com/reference/android/support/design/widget/BottomSheetBehavior.html#setState(int)
  [6]: https://developer.android.com/reference/android/support/design/widget/BottomSheetBehavior.html#STATE_COLLAPSED
  [7]: https://developer.android.com/reference/android/support/design/widget/BottomSheetBehavior.html#STATE_EXPANDED
  [8]: https://developer.android.com/reference/android/support/design/widget/BottomSheetBehavior.html#STATE_HIDDEN

## Open BottomSheet DialogFragment in Expanded mode by default.
BottomSheet DialogFragment opens up in `STATE_COLLAPSED` by default. Which can be forced to open to `STATE_EXPANDED`and take up the full device screen with help of the following code template.

@NonNull @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {

        BottomSheetDialog dialog = (BottomSheetDialog) super.onCreateDialog(savedInstanceState);

        dialog.setOnShowListener(new DialogInterface.OnShowListener() {
            @Override
            public void onShow(DialogInterface dialog) {
                BottomSheetDialog d = (BottomSheetDialog) dialog;

                FrameLayout bottomSheet = (FrameLayout) d.findViewById(android.support.design.R.id.design_bottom_sheet);
                BottomSheetBehavior.from(bottomSheet).setState(BottomSheetBehavior.STATE_EXPANDED);
            }
        });

        // Do something with your dialog like setContentView() or whatever
        return dialog;
    }

Although dialog animation is slightly noticeable but does the task of opening the DialogFragment in full screen very well.

