---
title: "Polymorphism in Elixir"
slug: "polymorphism-in-elixir"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

Polymorphism is the provision of a single interface to entities of different types. Basically, it allows different data types respond to the same function. So, the same function shapes for different data types to accomplish the same behavior. Elixir language has `protocols` to implement polymorphism with a clean way.


If you want to cover all data types you can define an implementation for `Any` data type. Lastly, if you have time, check the source code of [Enum][1] and [String.Char][2], which are good examples of polymorphism in core Elixir.


  [1]: https://github.com/elixir-lang/elixir/blob/master/lib/elixir/lib/enum.ex#L3096
  [2]: https://github.com/elixir-lang/elixir/blob/master/lib/elixir/lib/string/chars.ex#L3

## Polymorphism with Protocols
Let's implement a basic protocol that converts Kelvin and Fahrenheit temperatures to Celsius.


```elixir
defmodule Kelvin do
  defstruct name: "Kelvin", symbol: "K", degree: 0 
end

defmodule Fahrenheit do
  defstruct name: "Fahrenheit", symbol: "°F", degree: 0
end

defmodule Celsius do
  defstruct name: "Celsius", symbol: "°C", degree: 0
end

defprotocol Temperature do
  @doc """
  Convert Kelvin and Fahrenheit to Celsius degree
  """
  def to_celsius(degree)
end

defimpl Temperature, for: Kelvin do
  @doc """
  Deduct 273.15
  """
  def to_celsius(kelvin) do
    celsius_degree = kelvin.degree - 273.15
    %Celsius{degree: celsius_degree}
  end
end

defimpl Temperature, for: Fahrenheit do
  @doc """
  Deduct 32, then multiply by 5, then divide by 9
  """
  def to_celsius(fahrenheit) do
    celsius_degree = (fahrenheit.degree - 32) * 5 / 9
    %Celsius{degree: celsius_degree}
  end
end
```

Now, we implemented our converters for the Kelvin and Fahrenheit types. Let's make some conversions:

```elixir
iex> fahrenheit = %Fahrenheit{degree: 45}
%Fahrenheit{degree: 45, name: "Fahrenheit", symbol: "°F"}
iex> celsius = Temperature.to_celsius(fahrenheit)
%Celsius{degree: 7.22, name: "Celsius", symbol: "°C"}
iex> kelvin = %Kelvin{degree: 300}
%Kelvin{degree: 300, name: "Kelvin", symbol: "K"}
iex> celsius = Temperature.to_celsius(kelvin)
%Celsius{degree: 26.85, name: "Celsius", symbol: "°C"}
```

Let's try to convert any other data type which has no implementation for `to_celsius` function:

```elixir
iex> Temperature.to_celsius(%{degree: 12})
** (Protocol.UndefinedError) protocol Temperature not implemented for %{degree: 12}
    iex:11: Temperature.impl_for!/1
    iex:15: Temperature.to_celsius/1
```

