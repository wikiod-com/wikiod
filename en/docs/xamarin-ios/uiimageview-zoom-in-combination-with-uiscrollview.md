---
title: "UIImageView zoom in combination with UIScrollView"
slug: "uiimageview-zoom-in-combination-with-uiscrollview"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

The UIImageView has to be within a scrollview in order for this to work.

The DoubleTap method will toggle between the minScale and the doubleTapScale.

## Double Tap
<!-- language-all: c# -->

    private float minScale = 1f;
    private float doubleTapScale = 2f;
    private float maxScale = 4f;

    private void SetUpDoubleTapZoom()
    {
        imageViewToZoom.ContentMode = UIViewContentMode.ScaleAspectFit;
        scrollView.MaximumZoomScale = maxScale;
        scrollView.MinimumZoomScale = minScale;
    
        var doubleTap = new UITapGestureRecognizer(OnDoubleTap)
        {
            NumberOfTapsRequired = 2
        };
    
        scrollView.AddGestureRecognizer(doubleTap);
    }

    private void OnDoubleTap(UIGestureRecognizer gesture)
    {
        scrollView.ZoomScale = (scrollView.ZoomScale.Equals(minScale)) ? doubleTapScale : minScale;
    }



## Pinch gesture zoom
<!-- language-all: c# -->

    private float minScale = 1f;
    private float maxScale = 4f;

    private void SetUpPinchGestureZoom()
    {
        imageViewToZoom.ContentMode = UIViewContentMode.ScaleAspectFit;

        scrollView.MaximumZoomScale = maxScale;
        scrollView.MinimumZoomScale = minScale;

        scrollView.ViewForZoomingInScrollView += (UIScrollView sv) => { return imageViewToZoom; };
    }



