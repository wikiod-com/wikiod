---
title: "Wait"
slug: "wait"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

## Syntax
- wait [on SIGNAL1[, SIGNAL2[...]]] [until CONDITION] [for TIMEOUT];
- wait; -- Eternal wait
- wait on s1, s2; -- Wait until signals s1 or s2 (or both) change
- wait until s1 = 15; -- Wait until signal s1 changes and its new value is 15
- wait until s1 = 15 for 10 ns; -- Wait until signal s1 changes and its new value is 15 for at most 10 ns

## Wait until condition
<!-- vim: set textwidth=0: -->
<!-- Example: Wait until condition -->
It is possible to omit the `on <sensitivity_list>` and the `for <timeout>` clauses, like in:
```
    wait until CONDITION;
```

which is equivalent to:
```
    wait on LIST until CONDITION;
```

where `LIST` is the list of all __signals__ that appear in `CONDITION`. It is also equivalent to:
```
    loop
      wait on LIST;
      exit when CONDITION;
    end loop;
```

An important consequence is that if the `CONDITION` contains no signals, then:
```
    wait until CONDITION;
```

is equivalent to:
```
    wait;
```

A classical example of this is the famous:
```
    wait until now = 1 sec;
```

that does not do what one could think: as `now` is a function, not a signal, executing this statement suspends the process forever.

## Eternal wait
The simplest form of `wait` statement is simply:

    wait;

Whenever a process executes this it is suspended forever. The simulation scheduler will never resume it again. Example:

    signal end_of_simulation: boolean := false;
    ...
    process
    begin
      clock <= '0';
      wait for 500 ps;
      clock <= '1';
      wait for 500 ps;
      if end_of_simulation then
        wait;
      end if;
    end process;

## Sensitivity lists and wait statements
<!-- vim: set textwidth=0: -->
<!-- Example: Sensitivity lists and wait statements -->
A process with a sensitivity list cannot also contain wait statements. It is equivalent to the same process, without a sensitivity list and with one more last statement which is:
```
wait on <sensitivity_list>;
```

Example:
```
  process(clock, reset)
  begin
    if reset = '1' then
      q <= '0';
    elsif rising_edge(clock) then
      q <= d;
    end if;
  end process;
```

is equivalent to:

```
  process
  begin
    if reset = '1' then
      q <= '0';
    elsif rising_edge(clock) then
      q <= d;
    end if;
    wait on clock, reset;
  end process;
```

VHDL2008 introduced the `all` keyword in sensitivity lists. It is equivalent to *all signals that are read somewhere in the process*. It is especially handy to avoid incomplete sensitivity lists when designing combinatorial processes for synthesis. Example of incomplete sensitivity list:
```
  process(a, b)
  begin
    if ci = '0' then
      s  <= a xor b;
      co <= a and b;
    else
      s  <= a xnor b;
      co <= a or b;
    end if;
  end process;
```

the `ci` signal is not part of the sensitivity list and this is very likely a coding error that will lead to simulation mismatches before and after synthesis. The correct code is:
```
  process(a, b, ci)
  begin
    if ci = '0' then
      s  <= a xor b;
      co <= a and b;
    else
      s  <= a xnor b;
      co <= a or b;
    end if;
  end process;
```

In VHDL2008 the `all` keyword simplifies this and reduces the risk:
```
  process(all)
  begin
    if ci = '0' then
      s  <= a xor b;
      co <= a and b;
    else
      s  <= a xnor b;
      co <= a or b;
    end if;
  end process;
```

## Wait for a specific duration
using only the `for <timeout>` clause, it is possible to get an unconditional wait that lasts for a specific duration. This is not synthesizable (no real hardware can perform this behaviour so simply), but is frequently used for scheduling events and generating clocks within a testbench.

This example generates a 100 MHz, 50% duty cycle clock in the simulation testbench for driving the unit under test:

    constant period : time := 10 ns;
    ...
    process
    begin
       loop
          clk <= '0';
          wait for period/2;
          clk <= '1';
          wait for period/2;
       end loop;
    end process;

This example demonstrates how one might use a literal duration wait to sequence the testbench stimulus/analysis process:

    process
    begin
       rst <= '1';
       wait for 50 ns;
       wait until rising_edge(clk); --deassert reset synchronously
       rst <= '0';
       uut_input <= test_constant;
       wait for 100 us; --allow time for the uut to process the input
       if uut_output /= expected_output_constant then
          assert false report "failed test" severity error;
       else
          assert false report "passed first stage" severity note;
          uut_process_stage_2 <= '1';
       end if;
       ...
       wait;
    end process;

