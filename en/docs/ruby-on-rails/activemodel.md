---
title: "ActiveModel"
slug: "activemodel"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

ActiveModel was created to extract the model behavior of ActiveRecord into a separate concern. This allows us to use ActiveModel behavior in any object, not just ActiveRecord models.

ActiveRecord objects include all of this behavior by default.

## Using ActiveModel::Validations
You can validate any object, even plain ruby.

```ruby
class User
  include ActiveModel::Validations

  attr_reader :name, :age

  def initialize(name, age)
    @name = name
    @age  = age
  end

  validates :name, presence: true
  validates :age, numericality: { only_integer: true, greater_than: 12 }
end
```

```ruby
User.new('John Smith', 28).valid? #=> true
User.new('Jane Smith', 11).valid? #=> false
User.new(nil, 30).valid?          #=> false
```

