---
title: "Better debugging with IO.inspect and labels"
slug: "better-debugging-with-ioinspect-and-labels"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

`IO.inspect` is very useful when you try to debug your chains of method calling. It can get messy though if you use it too often.

Since Elixir 1.4.0 the `label` option of `IO.inspect` can help

Only works with Elixir 1.4+, but I can't tag that yet.

## Without labels
    url
      |> IO.inspect
      |> HTTPoison.get!
      |> IO.inspect
      |> Map.get(:body)
      |> IO.inspect
      |> Poison.decode!
      |> IO.inspect

This will result in a lot of output with no context:

    "https://jsonplaceholder.typicode.com/posts/1"
    %HTTPoison.Response{body: "{\n  \"userId\": 1,\n  \"id\": 1,\n  \"title\": \"sunt aut facere repellat provident occaecati excepturi optio reprehenderit\",\n  \"body\": \"quia et suscipit\\nsuscipit recusandae consequuntur expedita et cum\\nreprehenderit molestiae ut ut quas totam\\nnostrum rerum est autem sunt rem eveniet architecto\"\n}",
     headers: [{"Date", "Thu, 05 Jan 2017 14:29:59 GMT"},
      {"Content-Type", "application/json; charset=utf-8"},
      {"Content-Length", "292"}, {"Connection", "keep-alive"},
      {"Set-Cookie",
       "__cfduid=d56d1be0a544fcbdbb262fee9477600c51483626599; expires=Fri, 05-Jan-18 14:29:59 GMT; path=/; domain=.typicode.com; HttpOnly"},
      {"X-Powered-By", "Express"}, {"Vary", "Origin, Accept-Encoding"},
      {"Access-Control-Allow-Credentials", "true"},
      {"Cache-Control", "public, max-age=14400"}, {"Pragma", "no-cache"},
      {"Expires", "Thu, 05 Jan 2017 18:29:59 GMT"},
      {"X-Content-Type-Options", "nosniff"},
      {"Etag", "W/\"124-yv65LoT2uMHrpn06wNpAcQ\""}, {"Via", "1.1 vegur"},
      {"CF-Cache-Status", "HIT"}, {"Server", "cloudflare-nginx"},
      {"CF-RAY", "31c7a025e94e2d41-TXL"}], status_code: 200}
    "{\n  \"userId\": 1,\n  \"id\": 1,\n  \"title\": \"sunt aut facere repellat provident occaecati excepturi optio reprehenderit\",\n  \"body\": \"quia et suscipit\\nsuscipit recusandae consequuntur expedita et cum\\nreprehenderit molestiae ut ut quas totam\\nnostrum rerum est autem sunt rem eveniet architecto\"\n}"
    %{"body" => "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit molestiae ut ut quas totam\nnostrum rerum est autem sunt rem eveniet architecto",
      "id" => 1,
      "title" => "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
      "userId" => 1}

## With labels
using the `label` option to add context can help a lot:

    url
      |> IO.inspect(label: "url")
      |> HTTPoison.get!
      |> IO.inspect(label: "raw http resonse")
      |> Map.get(:body)
      |> IO.inspect(label: "raw body")
      |> Poison.decode!
      |> IO.inspect(label: "parsed body")

    url: "https://jsonplaceholder.typicode.com/posts/1"
    raw http resonse: %HTTPoison.Response{body: "{\n  \"userId\": 1,\n  \"id\": 1,\n  \"title\": \"sunt aut facere repellat provident occaecati excepturi optio reprehenderit\",\n  \"body\": \"quia et suscipit\\nsuscipit recusandae consequuntur expedita et cum\\nreprehenderit molestiae ut ut quas totam\\nnostrum rerum est autem sunt rem eveniet architecto\"\n}",
     headers: [{"Date", "Thu, 05 Jan 2017 14:33:06 GMT"},
      {"Content-Type", "application/json; charset=utf-8"},
      {"Content-Length", "292"}, {"Connection", "keep-alive"},
      {"Set-Cookie",
       "__cfduid=d22d817e48828169296605d27270af7e81483626786; expires=Fri, 05-Jan-18 14:33:06 GMT; path=/; domain=.typicode.com; HttpOnly"},
      {"X-Powered-By", "Express"}, {"Vary", "Origin, Accept-Encoding"},
      {"Access-Control-Allow-Credentials", "true"},
      {"Cache-Control", "public, max-age=14400"}, {"Pragma", "no-cache"},
      {"Expires", "Thu, 05 Jan 2017 18:33:06 GMT"},
      {"X-Content-Type-Options", "nosniff"},
      {"Etag", "W/\"124-yv65LoT2uMHrpn06wNpAcQ\""}, {"Via", "1.1 vegur"},
      {"CF-Cache-Status", "HIT"}, {"Server", "cloudflare-nginx"},
      {"CF-RAY", "31c7a4b8ae042d77-TXL"}], status_code: 200}
    raw body: "{\n  \"userId\": 1,\n  \"id\": 1,\n  \"title\": \"sunt aut facere repellat provident occaecati excepturi optio reprehenderit\",\n  \"body\": \"quia et suscipit\\nsuscipit recusandae consequuntur expedita et cum\\nreprehenderit molestiae ut ut quas totam\\nnostrum rerum est autem sunt rem eveniet architecto\"\n}"
    parsed body: %{"body" => "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit molestiae ut ut quas totam\nnostrum rerum est autem sunt rem eveniet architecto",
      "id" => 1,
      "title" => "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
      "userId" => 1}

