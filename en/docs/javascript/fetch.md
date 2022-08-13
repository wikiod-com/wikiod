---
title: "Fetch"
slug: "fetch"
draft: false
images: []
weight: 9858
type: docs
toc: true
---

## Syntax
 - promise = fetch(url).then(function(response) {})
 - promise = fetch(url, options)
 - promise = fetch(request)

## Parameters
| Options       | Details |
| ------------- | ----------- |
| `method`      | The HTTP method to use for the request. ex: `GET`, `POST`, `PUT`, `DELETE`, `HEAD`. Defaults to `GET`. |
| `headers`     | A `Headers` object containing additional HTTP headers to include in the request. |
| `body`        | The request payload, can be a `string` or a `FormData` object. Defaults to `undefined` |
| `cache`       | The caching mode. `default`, `reload`, `no-cache`
| `referrer`    | The referrer of the request. |
| `mode`        | `cors`, `no-cors`, `same-origin`. Defaults to `no-cors`. |
| `credentials` | `omit`, `same-origin`, `include`. Defaults to `omit`. |
| `redirect`    | `follow`, `error`, `manual`. Defaults to `follow`. |
| `integrity`   | Associated integrity metadata. Defaults to empty string. |

[The Fetch standard][1] defines requests, responses, and the process that binds them: fetching.

Among other interfaces, the standard defines `Request` and `Response` Objects, designed to be used for all operations involving network requests.

A useful application of these interfaces is `GlobalFetch`, which can be used to load remote resources.

For browsers that do not yet support the Fetch standard, GitHub has a [polyfill][2] available. In addition, there is also a [Node.js implementation][3] that is useful for server/client consistency. 

In the absence of cancelable Promises you can't abort the fetch request ([github issue][4]). But there is a [proposal][5] by the T39 in stage 1 for cancelable promises.


  [1]: https://fetch.spec.whatwg.org
  [2]: https://github.com/github/fetch
  [3]: https://github.com/bitinn/node-fetch
  [4]: https://github.com/whatwg/fetch/issues/27
  [5]: https://github.com/domenic/cancelable-promise

## Getting JSON data
```
// get some data from stackoverflow
fetch("https://api.stackexchange.com/2.2/questions/featured?order=desc&sort=activity&site=stackoverflow")
  .then(resp => resp.json())
  .then(json => console.log(json))
  .catch(err => console.log(err));
```

## Set Request Headers
```
fetch('/example.json', {
    headers: new Headers({
        'Accept': 'text/plain',
        'X-Your-Custom-Header': 'example value'
    })
});
```

## POST Data
Posting form data

```
fetch(`/example/submit`, {
    method: 'POST',
    body: new FormData(document.getElementById('example-form'))
});
```

Posting JSON data

```
fetch(`/example/submit.json`, {
    method: 'POST',
    body: JSON.stringify({
        email: document.getElementById('example-email').value,
        comment: document.getElementById('example-comment').value
    })
});
```

## GlobalFetch
The [GlobalFetch][1] interface exposes the `fetch` function, which can be used to request resources.

    fetch('/path/to/resource.json')
        .then(response => {
            if (!response.ok()) {
                throw new Error("Request failed!");
            }
                
            return response.json();
        })
        .then(json => { 
            console.log(json);
        }); 

The resolved value is a [Response][2] Object. This Object contains the body of the response, as well as it's status and headers.



[1]: https://fetch.spec.whatwg.org/#globalfetch
[2]: https://fetch.spec.whatwg.org/#response-class

## Send cookies
The fetch function does not send cookies by default. There are two possible ways to send cookies:

1. Only send cookies if the URL is on the same origin as the calling script.
```
fetch('/login', {
    credentials: 'same-origin'
})
```

2. Always send cookies, even for cross-origin calls.

```
fetch('https://otherdomain.com/login', {
    credentials: 'include'
})
```

## Using Fetch to Display Questions from the Stack Overflow API
    const url =
          'http://api.stackexchange.com/2.2/questions?site=stackoverflow&tagged=javascript';
    
    const questionList = document.createElement('ul');
    document.body.appendChild(questionList);
    
    const responseData = fetch(url).then(response => response.json());
    responseData.then(({items, has_more, quota_max, quota_remaining}) => {
      for (const {title, score, owner, link, answer_count} of items) {
        const listItem = document.createElement('li');
        questionList.appendChild(listItem);
        const a = document.createElement('a');
        listItem.appendChild(a);
        a.href = link;
        a.textContent = `[${score}] ${title} (by ${owner.display_name || 'somebody'})`
      }
    });



