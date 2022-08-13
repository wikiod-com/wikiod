---
title: "lein-cljsbuild"
slug: "lein-cljsbuild"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

More details on parameters for lein-clsjbuild can be found in their [example project][1].


  [1]: https://github.com/emezeske/lein-cljsbuild/blob/1.1.4/sample.project.clj

## ClojureScript dev and production build
<!-- This whole thing greatly confused me, so I thought I'd add it here. -->

Add a `:cljsbuild` node like the following to your project.clj file.

    :cljsbuild {
              :builds {
                       ;;Different target goals should have different names.
                       ;;We have the dev build here
                       :dev {
                            ;;The ClojureScript code should reside in these directories
                             :source-paths ["src-cljs"]
                             :compiler {
                                        ;;This is the target output file
                                        ;;This will include none of the goog code.                                  
                                        :output-to "resources/public/js/main.js"
                                        ;;This stores the compilation files, including goog files.
                                        ;;When using this dev profile, you should add "<script src="/js/goog/base.js>" to your html files.
                                        ;;That will load the goog dependency.
                                        :output-dir "resources/public/js/out"
                                        ;;Having optimizations off is recommended for development
                                        ;;However this means you have to add the goog dependencies yourself (see above)
                                        :optimizations :none
                                        ;;With no optimizations, marking this true, means it adds a source map so you can debug in browser
                                        :source-map true
                                        }
                             }
                       :prod {
                                    :source-paths ["src-cljs"]
                                    :compiler {
                                               ;;Outputs the javascript file to this path.
                                               :output-to "resources/public/js/main.js"
                                               ;;Advanced optimizations make your code smaller.
                                               ;;They include the goog code in your main file.
                                               ;;No need to add an extra script tag.
                                               :optimizations :advanced
                                               :jar true
                                               }
                                    }
                       }
              }

To run these build commands, use `lein cljsbuild once`. You can specify which profile to use by adding the profile name as the last argument, for example `lein cljsbuild once dev` to compile with dev parameters.

Using `lein cljsbuild auto` will cause cljsbuild to automatically update any changed files.


