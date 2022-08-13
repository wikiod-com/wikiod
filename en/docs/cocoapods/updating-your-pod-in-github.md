---
title: "Updating Your Pod In Github"
slug: "updating-your-pod-in-github"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Pod Update Checklist
1. Update your project with `git pull origin master`
2. Update `s.version` inside `MyRepo.podspec`
3. Check local errors with `pod lib lint MyRepo.podspec`
4. `git add .` & `git commit -m "Update pods version"`
5. `git push origin master`
6. Make a new [release](`https://github.com/<username>/MyRepo/releases)
7. `pod trunk push MyRepo.podspec`

[Source][1]


  [1]: http://guides.cocoapods.org/making/making-a-cocoapod.html

