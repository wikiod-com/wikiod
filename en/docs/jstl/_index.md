---
title : jstl Tutorial
slug : jstl-tutorial
weight : 9975
draft : false
images : []
type : docs
---

[JSTL][1] (JSP Standard Tag Library) is a JSP based standard tag library which offers `<c:xxx>` tags to [control the flow][2] in the JSP page, `<fmt:xxx>` tags for [date/number formatting and internationalization][3] facilities and several `${fn:xxx()}` [utility EL functions][4]. 

Note that JSTL also offers [SQL][5] and [XML][6] taglibs which enable a declarative manner of executing SQL queries and parsing XML inside a JSP page. This is however discouraged for other purposes than [quick prototyping][7]. In the real world both tasks need to be done by real Java classes which are (in)directly controlled/delegated by a [Servlet][8].

  [1]: http://docs.oracle.com/javaee/5/jstl/1.1/docs/tlddocs/
  [2]: http://docs.oracle.com/javaee/5/jstl/1.1/docs/tlddocs/c/tld-summary.html
  [3]: http://docs.oracle.com/javaee/5/jstl/1.1/docs/tlddocs/fmt/tld-summary.html
  [4]: http://docs.oracle.com/javaee/5/jstl/1.1/docs/tlddocs/fn/tld-summary.html
  [5]: http://docs.oracle.com/javaee/5/jstl/1.1/docs/tlddocs/sql/tld-summary.html
  [6]: http://docs.oracle.com/javaee/5/jstl/1.1/docs/tlddocs/x/tld-summary.html
  [7]: http://docs.oracle.com/javaee/5/tutorial/doc/bnald.html
  [8]: http://stackoverflow.com/tags/servlets/info


