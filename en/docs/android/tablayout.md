---
title: "TabLayout"
slug: "tablayout"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Using a TabLayout without a ViewPager
Most of the time a `TabLayout` is used together with a [ViewPager][1], in order to get the swipe functionality that comes with it.

It is possible to use a `TabLayout` without a `ViewPager` by using a `TabLayout.OnTabSelectedListener`.

First, add a `TabLayout` to your activity's XML file:

    <android.support.design.widget.TabLayout
        android:layout_height="wrap_content"
        android:layout_width="match_parent"
        android:id="@+id/tabLayout" />

For navigation within an `Activity`, manually populate the UI based on the tab selected.

    TabLayout tabLayout = (TabLayout) findViewById(R.id.tabLayout);
    tabLayout.addOnTabSelectedListener(new TabLayout.OnTabSelectedListener() {
        @Override
        public void onTabSelected(TabLayout.Tab tab) {
            int position = tab.getPosition();
            switch (tab.getPosition()) {
                case 1:
                    getSupportFragmentManager().beginTransaction()
                        .replace(R.id.fragment_container, new ChildFragment()).commit();
                    break;
                // Continue for each tab in TabLayout
        }
    
        @Override
        public void onTabUnselected(TabLayout.Tab tab) {
    
        }
    
        @Override
        public void onTabReselected(TabLayout.Tab tab) {
    
        }
    });


  [1]: https://www.wikiod.com/android/viewpager

