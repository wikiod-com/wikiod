---
title: "Performance Profiling"
slug: "performance-profiling"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## All About Profiling
**What is Profiling?**

By definition [Profiling][1] is a form of dynamic program analysis that measures, for example, the space (memory) or time complexity of a program, the usage of particular instructions, or the frequency and duration of function calls.

**Why is it necessary?**

Profiling is important because you can’t optimise effectively until you know what your program is spending most of its time doing. Without measuring your program execution time (profiling), you won’t know if you’ve actually improved it.

**Tools and Techniques :**

1. Chrome's in-built dev tools

    This includes a comprehensive set of tools to be used for profiling.You can go deep to find out bottlenecks in your javascript file, css files,         animations, cpu consumption, memory leaks, network, security etc.
     
    Make a Timeline [recording][2] and look for suspiciously long Evaluate Script events. If you find any, you can enable the [JS Profiler][3] and re-do your recording to get more detailed information about exactly which JS functions were called and how long each took. [Read more...][4] 


2. [FireBug][5] (use with Firefox)


3. [Dynatrace][6] (use with IE)


4. [Batarang][7] (use with Chrome)

    It's an outdated add-on for chrome browser though it's stable and can be used to monitor models, performance, dependencies for an angular application. It works fine for small scale application and can give you an insight of what does scope variable holds at various levels. It tells you about active watchers, watch expressions, watch collections in the app.


5. [Watcher][8] (use with Chrome)

    Nice and simplistic UI to count the number of watchers in a Angular app. 


6. Use the following code to manually find out the number of watchers in your angular app (credit to [@Words Like Jared Number of watchers][9])


    (function() {
        var root = angular.element(document.getElementsByTagName('body')),
            watchers = [],
            f = function(element) {
            angular.forEach(['$scope', '$isolateScope'], function(scopeProperty) {
                if(element.data() && element.data().hasOwnProperty(scopeProperty)) {
                    angular.forEach(element.data()[scopeProperty].$$watchers, function(watcher) {
                    watchers.push(watcher);
                    });
                }
            });
    
            angular.forEach(element.children(), function(childElement) {
                f(angular.element(childElement));
            });
        };
     
        f(root);
     
        // Remove duplicate watchers
        var watchersWithoutDuplicates = [];
        angular.forEach(watchers, function(item) {
            if(watchersWithoutDuplicates.indexOf(item) < 0) {
                watchersWithoutDuplicates.push(item);
            }
        });
        console.log(watchersWithoutDuplicates.length);
    })();

7. There are several online tools/websites available which facilitates wide range of functionalities to create a profile of your application. 

    One such site is : https://www.webpagetest.org/ 
    
    With this you can run a free website speed test from multiple locations around the globe using real browsers (IE and Chrome) and at real consumer connection speeds. You can run simple tests or perform advanced testing including multi-step transactions, video capture, content blocking and much more.


   **Next Steps:**

 
   Done with Profiling. It only brings you half way down the road. The very next task is to actually turn your findings into action items to optimise your application. [See this documentation][10] on how you can improve the performance of your angular app with simple tricks.

Happy Coding :)


  [1]: https://en.wikipedia.org/wiki/Profiling_(computer_programming)
  [2]: https://developers.google.com/web/tools/chrome-devtools/profile/evaluate-performance/timeline-tool#make-a-recording
  [3]: https://developers.google.com/web/tools/chrome-devtools/profile/evaluate-performance/timeline-tool#profile-js
  [4]: https://developers.google.com/web/tools/chrome-devtools/?hl=en
  [5]: https://addons.mozilla.org/en-US/firefox/addon/firebug/
  [6]: https://help.dynatrace.com/get-started/
  [7]: https://chrome.google.com/webstore/detail/unofficial-angularjs-bata/niopocochgahfkiccpjmmpchncjoapek/reviews
  [8]: https://chrome.google.com/webstore/detail/angular-watchers/nlmjblobloedpmkmmckeehnbfalnjnjk?utm_source=chrome-ntp-icon
  [9]: http://stackoverflow.com/questions/18499909/how-to-count-total-number-of-watches-on-a-page
  [10]: https://www.wikiod.com/angularjs/profiling-and-performance

