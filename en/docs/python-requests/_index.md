---
title : python-requests Tutorial
slug : python-requests-tutorial
weight : 9929
draft : false
images : []
type : docs
---

# HTTP for Humans  

[Requests][1] is the only Non-GMO HTTP library for Python, safe for human consumption.

Requests allows you to send organic, grass-fed `HTTP/1.1` requests, without the need for manual labor. There's no need to manually add query strings to your URLs, or to form-encode your POST data. Keep-alive and HTTP connection pooling are 100% automatic, powered by `urllib3`, which is embedded within Requests.

The power of Requests:

```python
>>> r = requests.get('https://api.github.com/user', auth=('user', 'pass'))
>>> r.status_code
200
>>> r.headers['content-type']
'application/json; charset=utf8'
>>> r.encoding
'utf-8'
>>> r.text
u'{"type":"User"...'
>>> r.json()
{u'private_gists': 419, u'total_private_repos': 77, ...}
```


  [1]: http://docs.python-requests.org/en/master/

