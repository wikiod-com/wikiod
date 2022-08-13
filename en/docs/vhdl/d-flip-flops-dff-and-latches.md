---
title: "D-Flip-Flops (DFF) and latches"
slug: "d-flip-flops-dff-and-latches"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

D-Flip-Flops (DFF) and latches are memory elements. A DFF samples its input on one or the other edge of its clock (not both) while a latch is transparent on one level of its enable and memorizing on the other. The following figure illustrates the difference:

[![DFF vs. latches behavior][1]][1]

Modelling DFFs or latches in VHDL is easy but there are a few important aspects that must be taken into account:

- The differences between VHDL models of DFFs and latches.
- How to describe the edges of a signal.
- How to describe synchronous or asynchronous set or resets.

  [1]: http://i.stack.imgur.com/nkCvj.png

## D-Flip-Flops (DFF)
In all examples:
- `clk` is the clock,
- `d` is the input,
- `q` is the output,
- `srst` is an active high synchronous reset,
- `srstn` is an active low synchronous reset,
- `arst` is an active high asynchronous reset,
- `arstn` is an active low asynchronous reset,
- `sset` is an active high synchronous set,
- `ssetn` is an active low synchronous set,
- `aset` is an active high asynchronous set,
- `asetn` is an active low asynchronous set

All signals are of type `ieee.std_logic_1164.std_ulogic`. The syntax used is the one that leads to correct synthesis results with all logic synthesizers. Please see the _Clock edge detection_ example for a discussion about alternate syntax.

# Rising edge clock

    process(clk)
    begin
      if rising_edge(clk) then
        q <= d;
      end if;
    end process;

# Falling edge clock

    process(clk)
    begin
      if falling_edge(clk) then
        q <= d;
      end if;
    end process;

# Rising edge clock, synchronous active high reset

    process(clk)
    begin
      if rising_edge(clk) then
        if srst = '1' then
          q <= '0';
        else
          q <= d;
        end if;
      end if;
    end process;

# Rising edge clock, asynchronous active high reset

    process(clk, arst)
    begin
      if arst = '1' then
        q <= '0';
      elsif rising_edge(clk) then
        q <= d;
      end if;
    end process;

# Falling edge clock, asynchronous active low reset, synchronous active high set

    process(clk, arstn)
    begin
      if arstn = '0' then
        q <= '0';
      elsif falling_edge(clk) then
        if sset = '1' then
          q <= '1';
        else
          q <= d;
        end if;
      end if;
    end process;

# Rising edge clock, asynchronous active high reset, asynchronous active low set

> Note: set has higher priority than reset

    process(clk, arst, asetn)
    begin
      if asetn = '0' then
        q <= '1';
      elsif arst = '1' then
        q <= '0';
      elsif rising_edge(clk) then
        q <= d;
      end if;
    end process;

## Latches
In all examples:
- `en` is the enable signal,
- `d` is the input,
- `q` is the output,
- `srst` is an active high synchronous reset,
- `srstn` is an active low synchronous reset,
- `arst` is an active high asynchronous reset,
- `arstn` is an active low asynchronous reset,
- `sset` is an active high synchronous set,
- `ssetn` is an active low synchronous set,
- `aset` is an active high asynchronous set,
- `asetn` is an active low asynchronous set

All signals are of type `ieee.std_logic_1164.std_ulogic`. The syntax used is the one that leads to correct synthesis results with all logic synthesizers. Please see the _Clock edge detection_ example for a discussion about alternate syntax.

# Active high enable

    process(en, d)
    begin
      if en = '1' then
        q <= d;
      end if;
    end process;

# Active low enable

    process(en, d)
    begin
      if en = '0' then
        q <= d;
      end if;
    end process;

# Active high enable, synchronous active high reset

    process(en, d)
    begin
      if en = '1' then
        if srst = '1' then
          q <= '0';
        else
          q <= d;
        end if;
      end if;
    end process;

# Active high enable, asynchronous active high reset

    process(en, d, arst)
    begin
      if arst = '1' then
        q <= '0';
      elsif en = '1' then
        q <= d;
      end if;
    end process;

# Active low enable, asynchronous active low reset, synchronous active high set

    process(en, d, arstn)
    begin
      if arstn = '0' then
        q <= '0';
      elsif en = '0' then
        if sset = '1' then
          q <= '1';
        else
          q <= d;
        end if;
      end if;
    end process;

# Active high enable, asynchronous active high reset, asynchronous active low set

> Note: set has higher priority than reset

    process(en, d, arst, asetn)
    begin
      if asetn = '0' then
        q <= '1';
      elsif arst = '1' then
        q <= '0';
      elsif en = '1' then
        q <= d;
      end if;
    end process;

## Clock edge detection
# The short story

Whith VHDL 2008 and if the type of the clock is `bit`, `boolean`, `ieee.std_logic_1164.std_ulogic` or `ieee.std_logic_1164.std_logic`, a clock edge detection can be coded for rising edge
- `if rising_edge(clock) then`
- `if clock'event and clock = '1' then -- type bit, std_ulogic or std_logic`
- `if clock'event and clock then -- type boolean`

and for falling edge
- `if falling_edge(clock) then`
- `if clock'event and clock = '0' then -- type bit, std_ulogic or std_logic`
- `if clock'event and not clock then -- type boolean`

This will behave as expected, both for simulation and synthesis.

> Note: the definition of a rising edge on a signal of type `std_ulogic` is a bit more complex than the simple `if clock'event and clock = '1' then`. The standard `rising_edge` function, for instance, has a different definition. Even if it will probably make no difference for synthesis, it could make one for simulation.

The use of the `rising_edge` and `falling_edge` standard functions is strongly encouraged. With previous versions of VHDL the use of these functions may require to explicitly declare the use of standard packages (e.g. `ieee.numeric_bit` for type `bit`) or even to define them in a custom package.

> Note: do not use the `rising_edge` and `falling_edge` standard functions to detect edges of non-clock signals. Some synthesizers could conclude that the signal is a clock. Hint: detecting an edge on a non-clock signal can frequently be done by sampling the signal in a shift register and comparing the sampled values at different stages of the shift register.

# The long story

Properly describing the detection of the edges of a clock signal is essential when modelling D-Flip-Flops (DFF). An edge is, by definition, a transition from one particular value to another. For instance, we can defined the rising edge of a signal of type `bit` (the standard VHDL enumerated type that takes two values: `'0'` and `'1'`) as the transition from `'0'` to `'1'`. For type `boolean` we can define it as a transition from `false` to `true`.

Frequently, more complex types are used. The `ieee.std_logic_1164.std_ulogic` type, for instance, is also an enumerated type, just like `bit` or `boolean`, but it has 9 values instead of 2:

| Value | Meaning            |
| :---- | :----------------- |
| `'U'` | Uninitialized      |
| `'X'` | Forcing unknown    |
| `'0'` | Forcing low level  |
| `'1'` | Forcing high level |
| `'Z'` | High impedance     |
| `'W'` | Weak unknown       |
| `'L'` | Weak low level     |
| `'H'` | Weak high level    |
| `'-'` | Don't care         |

Defining a rising edge on such a type is a bit more complex than for `bit` or `boolean`. We can, for instance, decide that it is a transition from `'0'` to `'1'`. But we can also decide that it is a transition from `'0'` or `'L'` to `'1'` or `'H'`.

> Note: it is this second definition that the standard uses for the `rising_edge(signal s: std_ulogic)` function defined in `ieee.std_logic_1164`.

When discussing the various ways to detect edges, it is thus important to consider the type of the signal. It is also important to take the modeling goal into account: simulation only or logic synthesis? Let us illustrate this on a few examples:

## Rising edge DFF with type bit

    signal clock, d, q: bit;
    ...
    P1: process(clock)
    begin
      if clock = '1' then
        q <= d;
      end if;
    end process P1;

Technically, on a pure simulation semantics point of view, process `P1` models a rising edge triggered DFF. Indeed, the `q <= d` assignment is executed if and only if:
- `clock` changed (this is what the sensitivity list expresses) __and__
- the current value of `clock` is `'1'`.

As `clock` is of type bit and type bit has only values `'0'` and `'1'`, this is exactly what we defined as a rising edge of a signal of type bit. Any simulator will handle this model as we expect.

> Note: For logic synthesizers, things are a bit more complex, as we will see later.

## Rising edge DFF with asynchronous active high reset and type bit

In order to add an asynchronous active high reset to our DFF, one could try something like:

    signal clock, reset, d, q: bit;
    ...
    P2_BOGUS: process(clock, reset)
    begin
      if reset = '1' then
        q <= '0';
      elsif clock = '1' then
        q <= d;
      end if;
    end process P2_BOGUS;

But __this does not work__. The condition for the `q <= d` assignment to be executed should be: _a rising edge of `clock` while `reset = '0'`_. But what we modeled is:
- `clock` or `reset` or both changed __and__
- `reset = '0'` __and__
- `clock = '1'`

Which is not the same: if `reset` changes from `'1'` to `'0'` while `clock = '1'` the assignment will be executed while it is __not__ a rising edge of `clock`.

In fact, there is no way to model this in VHDL without the help of a signal attribute:

    P2_OK: process(clock, reset)
    begin
      if reset = '1' then
        q <= '0';
      elsif clock = '1' and clock'event then
        q <= d;
      end if;
    end process P2_OK;

The `clock'event` is the signal attribute `event` applied to signal `clock`. It evaluates as a `boolean` and it is `true` if and only if signal `clock` changed during the signal update phase that just preceded the current execution phase. Thanks to this, process `P2_OK` now perfectly models what we want in simulation (and synthesis).

## Synthesis semantics

Many logic synthesizers identify signal edge detections based on syntactic patterns, not on the semantics of the VHDL model. In other words, they consider what the VHDL code looks like, not what behavior it models. One of the patterns they all recognize is:

    if clock = '1' and clock'event then

So, even in the example of process `P1` we should use it if we want our model to be synthesizable by all logic synthesizers:

    signal clock, d, q: bit;
    ...
    P1_OK: process(clock)
    begin
      if clock = '1' and clock'event then
        q <= d;
      end if;
    end process P1_OK;

The `and clock'event` part of the condition is completely redundant with the sensitivity list but as some synthesizers need it...

## Rising edge DFF with asynchronous active high reset and type `std_ulogic`

In this case, expressing the rising edge of the clock and the reset condition can become complicated. If we retain the definition of a rising edge that we proposed above and if we consider that the reset is active if it is `'1'` or `'H'`, the model becomes:

    library ieee;
    use ieee.std_logic_1164.all;
    ...
    signal clock, reset, d, q: std_ulogic;
    ...
    P4: process(clock, reset)
    begin
      if reset = '1' or reset = 'H' then
        q <= '0';
      elsif clock'event and
            (clock'last_value = '0' or clock'last_value = 'L') and
            (clock = '1' or clock = 'H') then
        q <= d;
      end if;
    end process P4;

> Note: `'last_value` is another signal attribute that returns the value the signal had before the last value change.

## Helper functions

The VHDL 2008 standard offers several helper functions to simplify the detection of signal edges, especially with multi-valued enumerated types like `std_ulogic`. The `std.standard` package defines the `rising_edge` and `falling_edge` functions on types `bit` and `boolean` and the `ieee.std_logic_1164` package defines them on types `std_ulogic` and `std_logic`.

> Note: with previous versions of VHDL the use of these functions may require to explicitly declare the use of standard packages (e.g. ieee.numeric_bit for type bit) or even to define them in a user package.

Let us revisit the previous examples and use the helper functions:

    signal clock, d, q: bit;
    ...
    P1_OK_NEW: process(clock)
    begin
      if rising_edge(clock) then
        q <= d;
      end if;
    end process P1_OK_NEW;

<!-- -->

    signal clock, d, q: bit;
    ...
    P2_OK_NEW: process(clock, reset)
    begin
      if reset = '1' then
        q <= '0';
      elsif rising_edge(clock) then
        q <= d;
      end if;
    end process P2_OK_NEW;

<!-- -->

    library ieee;
    use ieee.std_logic_1164.all;
    ...
    signal clock, reset, d, q: std_ulogic;
    ...
    P4_NEW: process(clock, reset)
    begin
      if reset = '1' then
        q <= '0';
      elsif rising_edge(clock) then
        q <= d;
      end if;
    end process P4_NEW;

> Note: in this last example we also simplified the test on the reset. Floating, high impedance, resets are quite rare and, in most cases, this simplified version works for simulation and synthesis.

