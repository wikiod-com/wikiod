---
title: "Adding UIRefreshControl to a table view"
slug: "adding-uirefreshcontrol-to-a-table-view"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Addind a simple UIRefreshControl to a UIScrollView
We assume a fully working `UIScrollview` named `_scrollView`;

Note that `UITableView`, `UICollectionView` are also scrollviews, hence the following examples would work on those UI elements.

First, creation & allocation

    UIRefreshControl refreshControl = new UIRefreshControl();

Second, connecting the refresh event to a method. There are different ways to do that.

<h1>Style 1:</h1>

    refreshControl.ValueChanged += (object sender, EventArgs e) => MyMethodCall();

<h1>Style 2:</h1>

    refreshControl.ValueChanged += (object sender, EventArgs e) =>
    {
        //Write code here
    };

<h1>Style 3:</h1>
      
    refreshControl.ValueChanged += HandleRefreshValueChanged;

    void HandleRefreshValueChanged(object sender, EventArgs e)
    {
        //Write code here
    }

Third and last, adding the refresh control itself to our scrollview.

    _scrollView.AddSubview(refreshControl);

