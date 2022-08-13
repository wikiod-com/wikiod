---
title : mod-rewrite Tutorial
slug : mod-rewrite-tutorial
weight : 9996
draft : false
images : []
type : docs
---

[mod_rewrite][1] is a module for Apache. This module is used for internal rewrites (external requests that should load a different resource) and external redirects (external requests that should make the client request a different url).

mod_rewrite provides a finer control over internal rewrites than [mod_alias][2], as the latter can only map requests to filenames. mod_rewrite provides some means of [access control][3], but this is usually better done with [mod_authz_core][4] and [mod_authz_host][5]. mod_rewrite provides some integration with [mod_proxy][6], but for performance reasons this integration should not be used and instead `ProxyPass` and `ProxyPassMatch` of the latter module should be used.

mod_rewrite can be set up in a way that allows for directives to be placed in the dynamic (.htaccess) configuration files. For performance reasons, one should always use the static (httpd.conf) configuration file whenever possible.


  [1]: https://httpd.apache.org/docs/current/mod/mod_rewrite.html
  [2]: https://httpd.apache.org/docs/current/mod/mod_alias.html
  [3]: https://httpd.apache.org/docs/2.4/howto/access.html
  [4]: https://httpd.apache.org/docs/current/mod/mod_authz_core.html
  [5]: https://httpd.apache.org/docs/current/mod/mod_authz_host.html
  [6]: https://httpd.apache.org/docs/current/mod/mod_proxy.html

