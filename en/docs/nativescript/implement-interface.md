---
title: "implement Interface"
slug: "implement-interface"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## implement View.OnLayoutChangeListener in Nativescript
----------

    let playerLayoutChangeListener = new android.view.View.OnLayoutChangeListener( {
          onLayoutChange : function ( v:View, left:number, top:number, right:number, bottom:number, oldLeft:number, oldTop:number, oldRight:number, oldBottom:number):any {
              if (left != oldLeft || top != oldTop || right != oldRight || bottom != oldBottom) {
                  console.log("OnLayoutChangeListener");
                  __this.changeSurfaceLayout();
              }
          }
        });

   create a surfaceView https://www.wikiod.com/proposed

Add Listener:

        surfaceView.addOnLayoutChangeListener(playerLayoutChangeListener);
 remove Listener:

        surfaceView.removeOnLayoutChangeListener(playerLayoutChangeListener);



  [1]: https://www.wikiod.com/proposed

