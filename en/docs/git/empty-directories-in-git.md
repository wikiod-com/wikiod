---
title: "Empty directories in Git"
slug: "empty-directories-in-git"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Git doesn't track directories
Assume you've initialized a project with the following directory structure:

<pre>
/build
app.js
</pre>

Then you add everything so you've created so far and commit:

<pre>
git init
git add .
git commit -m "Initial commit"
</pre>

Git will only track the file app.js.

Assume you added a build step to your application and rely on the "build" directory to be there as the output directory (and you don't want to make it a setup instruction every developer has to follow), a *convention* is to include a ".gitkeep" file inside the directory and let Git track that file.

<pre>
/build
  .gitkeep
app.js
</pre>

Then add this new file:

<pre>
git add build/.gitkeep
git commit -m "Keep the build directory around"
</pre>

Git will now track the file build/.gitkeep file and therefore the build folder will be made available on checkout.

Again, this is just a convention and not a Git feature.

