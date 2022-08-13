---
title : rspec Tutorial
slug : rspec-tutorial
weight : 9971
draft : false
images : []
type : docs
---

RSpec is a BDD tool used to specify and test Ruby programs. It is used primarily to specify and test classes and methods, i.e. for unit testing.

[The rspec gem][1] is just a meta-gem which brings in the three parts of RSpec. Those three parts are also a way to structure this documentation.

- [rspec-core][2] provides RSpec's way of structuring and running tests: the `rspec` command-line executable, the `describe`, `context` and `it` methods, shared examples, etc. It is documented in [the RSpec Core topic][3].
- [rspec-expectations][4] provides RSpec's support for expecting test results: the `expect`/`to` expectation syntax and RSpec's built-in matchers. (It also provides the deprecated `should` expectation syntax.) It is documented in [the RSpec Expectations topic][5].
- [rspec-mocks][6] provides RSpec's support for test doubles: `double`, `allow`, `expect`, `receive`, `have_received`, etc. It is documented in [the RSpec Mocks topic][7].

There is also the rspec-rails gem, which extends RSpec with support for testing the types of classes used in Rails applications, and with support for writing feature specs (acceptance tests) which test the application from the user's point of view.

Official documentation for RSpec and rspec-rails is here: https://www.relishapp.com/rspec


  [1]: https://github.com/rspec/rspec
  [2]: https://github.com/rspec/rspec-core
  [3]: https://www.wikiod.com/rspec/rspec-core
  [4]: https://github.com/rspec/rspec-expectations/
  [5]: https://www.wikiod.com/rspec/rspec-expectations
  [6]: https://github.com/rspec/rspec-mocks/
  [7]: https://www.wikiod.com/rspec/rspec-mocks

