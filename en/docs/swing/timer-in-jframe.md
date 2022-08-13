---
title: "timer in JFrame"
slug: "timer-in-jframe"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Timer In JFrame
Suppose you have a button in your Java program that counts down a time. Here is the code for 10 minutes timer.

    private final static long REFRESH_LIST_PERIOD=10 * 60 * 1000; //10 minutes
    
    Timer timer = new Timer(1000, new ActionListener() {
    
       @Override
       public void actionPerformed(ActionEvent e) {
        if (cnt > 0) {
         cnt = cnt - 1000;
         btnCounter.setText("Remained (" + format.format(new Date(cnt)) + ")");
        } else {
         cnt = REFRESH_LIST_PERIOD;
         //TODO
        }
    
       }
      });
    
      timer.start();

