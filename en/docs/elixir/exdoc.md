---
title: "ExDoc"
slug: "exdoc"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Introduction
To generate documentation in `HTML` format from `@doc` and `@moduledoc` attributes in your source code, add `ex_doc` and a markdown processor, right now ExDoc supports [Earmark][], [Pandoc][], [Hoedown][] and [Cmark][], as dependencies into your `mix.exs` file:

    # config/mix.exs
    
    def deps do
      [{:ex_doc, "~> 0.11", only: :dev},
       {:earmark, "~> 0.1", only: :dev}]
    end

If you want to use another Markdown processor, you can find more information in the [Changing the Markdown tool][change markdown tool] section.

You can use Markdown within Elixir `@doc` and `@moduledoc` attributes.

Then, run `mix docs`.

One thing to keep in mind is that ExDoc allows configuration parameters, such as:

      def project do
        [app: :my_app,
         version: "0.1.0-dev",
         name: "My App",
         source_url: "https://github.com/USER/APP",
         homepage_url: "http://YOUR_PROJECT_HOMEPAGE",
         deps: deps(),
         docs: [logo: "path/to/logo.png",
                output: "docs",
                main: "README",
                extra_section: "GUIDES",
                extras: ["README.md", "CONTRIBUTING.md"]]]
      end

You can see more information about this configuration options with `mix help docs`

[Earmark]: http://github.com/pragdave/earmark
[Pandoc]: http://johnmacfarlane.net/pandoc/
[Hoedown]: https://github.com/hoedown/hoedown
[Cmark]: https://github.com/jgm/cmark
[change markdown tool]: https://github.com/elixir-lang/ex_doc#changing-the-markdown-tool

