---
title: "GitHub Pages"
slug: "github-pages"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Using Git to create pages from scratch
 1. Create a new repository or clone an existing one.
 2. Create a new branch called `gh-pages` without any history


    $ git checkout --orphan gh-pages
    
    # ensure you are in the correct directory then,
    # remove all files from the old working tree
    $ git rm -rf
    

3. Add an index.html file to the root of the repository.
    

    $ echo "Hello World" > index.html
    $ git add index.html
    $ git commit -a -m "First pages commit"

4. Push to Github.
    

    $ git push origin gh-pages

You can now load your new Github Pages site at 
`http(s)://<username>.github.io/<projectname>`

## Using the automatic page generator for a repository
1. Go to the GitHub website
2. Open your repository
3. Click Settings
4. Under GitHub Pages, click "Launch Automatic Page Generator"
5. Follow the instructions

## Creating a custom URL for your GitHub page
You will need a domain name from a [registrar](https://en.wikipedia.org/wiki/Domain_name_registrar). 

In the `gh-pages` branch of your project repository, or the main branch of your `username.github.io` repository, create a CNAME file with the contents `www.yourdomain.com` - the [*canonical* domain](https://www.mattcutts.com/blog/seo-advice-url-canonicalization/). 

At your registrar's domain configuration page, point your domain to your GitHub website. 
Set up two CNAME records (one for the root apex (@) and one for www). Both should point to `username.github.io` or `username.github.io/repository`. 
If your DNS provider does NOT support ALIAS records on the root apex (@), simply create A records that point to 192.30.252.153 and 192.30.252.154. 

* * *

## Resources

[GitHub instructions for a custom domain](https://help.github.com/articles/using-a-custom-domain-with-github-pages/)

[Stack Overflow Q&A: "Custom domain for GitHub project pages"](http://stackoverflow.com/a/9123911/3193542)

[Audrey Watters - Using GitHub to Power A Web Project: How and Why](http://audreywatters.com/2013/07/07/how-to-run-your-site-on-github)

[Alex Cican - How I moved my websites to Dropbox and GitHub](https://alexcican.com/post/guide-hosting-website-dropbox-github/)

[Treehouse - Using GitHub Pages To Host Your Website](http://blog.teamtreehouse.com/using-github-pages-to-host-your-website)

