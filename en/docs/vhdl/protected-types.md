---
title: "Protected types"
slug: "protected-types"
draft: false
images: []
weight: 9921
type: docs
toc: true
---

Prior VHDL 1993, two concurrent processes could communicate only with signals. Thanks to the simulation semantics of the language that updates signals only between simulation steps, the result of a simulation was deterministic: it did not depend on the order chosen by the simulation scheduler to execute the processes.

[In fact, this is not 100% true. Processes could also communicate using file input/output. But if a designer was compromising the determinism by using files, it could not really be a mistake.]

The language was thus safe. Except on purpose, it was almost impossible to design non-deterministic VHDL models.

VHDL 1993 introduced shared variables and designing non-deterministic VHDL models became very easy.

VHDL 2000 introduced protected types and the constraint that shared variables must be of protected type.

In VHDL, protected types are what resemble most the concept of objects in Object Oriented (OO) languages. They implement the encapsulation of data structures and their methods. They also guarantee the exclusive and atomic access to their data members. This does not completely prevent non-determinism but, at least, adds exclusiveness and atomicity to the shared variables.

Protected types are very useful when designing high level VHDL models intended for simulation only. They have several very good properties of OO languages. Using them frequently makes the code more readable, maintainable and reusable.

Notes:
- Some simulation tool chains, by default, only issue warnings when a shared variable is not of a protected type.
- Some synthesis tools do not support protected types.
- Some synthesis tools have a limited support of shared variables.
- One could think that shared variables are not usable to model hardware and shall be reserved for code instrumentation without side effects. But the VHDL patterns advised by several EDA vendors to model the memory plane of multi-ports Random Access Memories (RAM) use shared variables. So, yes, shared variables can be synthesizable in certain circumstances.

## A pseudo-random generator
Peudo-random generators are frequently useful when designing simulation environments. The following VHDL package shows how to use protected types to design a pseudo-random generator of `boolean`, `bit` and `bit_vector`. It can easily be extended to also generate random `std_ulogic_vector`, `signed`, `unsigned`. Extending it to generate random integers with arbitrary bounds and a uniform distribution is a bit more tricky but doable.

# The package declaration

A protected type has a declaration where all public subprogram accessors are declared. For our random generator we will make public one seed initialization procedure and three impure functions returning a random `boolean`, `bit` or `bit_vector`. Note that the functions cannot be pure as different calls of any of them, with the same parameters, can return different values.

    -- file rnd_pkg.vhd
    package rnd_pkg is
        type rnd_generator is protected
            procedure init(seed: bit_vector);
            impure function get_boolean return boolean;
            impure function get_bit return bit;
            impure function get_bit_vector(size: positive) return bit_vector;
        end protected rnd_generator;
    end package rnd_pkg;

# The package body

The protected type body defines the inner data structures (members) and the subprogram bodies. Our random generator is based on a 128-bits Linear Feedback Shift Register (LFSR) with four taps. The `state` variable stores the current state of the LFSR. A private `throw` procedure shifts the LFSR every time the generator is used.

    -- file rnd_pkg.vhd
    package body rnd_pkg is
        type rnd_generator is protected body
            constant len: positive := 128;
            constant default_seed: bit_vector(1 to len) := X"8bf052e898d987c7c31fc71c1fc063bc";
            type tap_array is array(natural range <>) of positive range 1 to len;
            constant taps: tap_array(0 to 3) := (128, 126, 101, 99);
    
            variable state: bit_vector(1 to len) := default_seed;
    
            procedure throw(n: positive := 1) is
                variable tmp: bit;
            begin
                for i in 1 to n loop
                    tmp := '1';
                    for j in taps'range loop
                        tmp := tmp xnor state(taps(j));
                    end loop;
                    state := tmp & state(1 to len - 1);
                end loop;
            end procedure throw;
    
            procedure init(seed: bit_vector) is
                constant n:   natural            := seed'length;
                constant tmp: bit_vector(1 to n) := seed;
                constant m:   natural            := minimum(n, len);
            begin
                state         := (others => '0');
                state(1 to m) := tmp(1 to m);
            end procedure init;
    
            impure function get_boolean return boolean is
                constant res: boolean := state(len) = '1';
            begin
                throw;
                return res;
            end function get_boolean;
    
            impure function get_bit return bit is
                constant res: bit := state(len);
            begin
                throw;
                return res;
            end function get_bit;
    
            impure function get_bit_vector(size: positive) return bit_vector is
                variable res: bit_vector(1 to size);
            begin
                if size <= len then
                    res := state(len + 1 - size to len);
                    throw(size);
                else
                    res(1 to len) := state;
                    throw(len);
                    res(len + 1 to size) := get_bit_vector(size - len);
                end if;
                return res;
            end function get_bit_vector;
        end protected body rnd_generator;
    end package body rnd_pkg;

The random generator can then be used in a OO style as in:

    -- file rnd_sim.vhd
    use std.env.all;
    use std.textio.all;
    use work.rnd_pkg.all;
    
    entity rnd_sim is
    end entity rnd_sim;
    
    architecture sim of rnd_sim is
        shared variable rnd: rnd_generator;
    begin
        process
            variable l: line;
        begin
            rnd.init(X"fe39_3d9f_24bb_5bdc_a7d0_2572_cbff_0117");
            for i in 1 to 10 loop
                write(l, rnd.get_boolean);
                write(l, HT);
                write(l, rnd.get_bit);
                write(l, HT);
                write(l, rnd.get_bit_vector(10));
                writeline(output, l);
            end loop;
            finish;
        end process;
    end architecture sim;

<!-- -->

    $ mkdir gh_work
    $ ghdl -a --std=08 --workdir=gh_work rnd_pkg.vhd rnd_sim.vhd
    $ ghdl -r --std=08 --workdir=gh_work rnd_sim
    TRUE    1    0001000101
    FALSE   0    1111111100
    TRUE    1    0010110010
    TRUE    1    0010010101
    FALSE   0    0111110100
    FALSE   1    1101110010
    TRUE    1    1011010110
    TRUE    1    0010010010
    TRUE    1    1101100111
    TRUE    1    0011100100
    simulation finished @0ms

