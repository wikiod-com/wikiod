---
title: "Static Timing Analysis - what does it mean when a design fails timing?"
slug: "static-timing-analysis---what-does-it-mean-when-a-design-fails-timing"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## What is timing?
The concept of timing is related more to the physics of flip flops than VHDL, but is an important concept that any designer using VHDL to create hardware should know.

When designing digital hardware, we are typically creating **synchronous logic**. This means our data travels from flip-flop to flip-flop, possibly with some combinatorial logic between them. The most basic diagram of synchronous logic that incorporates a combinatorial function is shown below:[![enter image description here][1]][1]

One Important design goal is **deterministic operation**. In this case, that means if flop A's Q output was presenting logic `1` when the clock edge occurred, we expect flop B's Q output to start presenting logic `0` every time without exception.

With **ideal** flip-flops, as typically described with VHDL (ex. `B <= not A when rising_edge(clk);`) deterministic operation is assumed. Behavioral VHDL simulations usually assume ideal flip-flops that always act deterministically. With real flip-flops, this is not so simple and we must obey **setup** and **hold** requirements pertaining to when the D input of a flop changes in order to guarantee reliable operation.

The **setup** time specifies how long the D input must remain unchanged *before* the arrival of the clock edge.
The **hold** time specifies how long the D input must remain unchanged *after* the arrival of the clock edge.

The numerical values are based on the underlying physics of a flip flop and vary significantly with **process** (imperfections in the silicon from the creation of the hardware), **voltage** (levels of logic '0' and '1'), and **temperature**. Typically the values used for calculations are the worst case (longest requirement) so we can guarantee functionality in any chip and environment. Chips are manufactured with permissible ranges for temperature power supply in part to limit the worst case that needs to be considered.

**Violating** setup and hold times can result in a variety of non-deterministic behavior, including the wrong logic value appearing at Q, an intermediate voltage appearing at Q (may be interpreted as a `0` or `1` by the next logic element), and having the Q output oscillate. Because all the numbers used are worst case values, moderate violations will typically result in the normal, deterministic result on a specific piece of hardware, but an implementation that has any timing failure is not safe to distribute on multiple devices because a case where the actual values approach the worst case values will eventually occur.

Typical requirements for flip-flops in a modern FPGA are 60 pico-seconds setup time, with a matching 60 ps hold requirement. Although the specifics of implementation are given in an **FPGA** context, almost all of this material applies to **ASIC** design as well.

There are several other delays and time values that need to be considered to determine whether timing was met. These include:

- **Routing Delay** - the time it takes for electrical signals to travel along the wires between logic elements
- **Logic Delay** - the time it takes for the input to the intermediate combinational logic to affect the output. Also commonly referred to as gate delay.
- **Clock-to-out Delay** - another physical property of the flip-flop, this is the time it takes for the Q output to change after the clock edge occurs.
- **Clock Period** - the ideal time between two edges of the clock. A typical period for a modern FPGA that meets timing easily is 5 nano-seconds, but the actual period used is chosen by the designer and can be moderately shorter or drastically longer.
- **Clock Skew** - the difference in routing delays of clock source to flop A and the clock source to flop B
- **Clock Jitter/Uncertainty** - a function of electrical noise and imperfect oscillators. This is the maximum deviation the clock period can have from the ideal, incorporating both frequency error (ex. oscillator runs 1% too fast causing the 5ns ideal period to become 4.95ns with 50ps uncertainty) and peak-to-peak (ex. the average period is 5ns but 1/1000 cycles has a period of 4.9ns with 100ps of jitter)

Checking whether a circuit implementation meets timing is calculated in two steps with two sets of values for the delays since the worst case delays for the hold requirement are the best case delays for setup requirement.

The **hold check** is verifying that the new value of A's Q output on clock cycle x doesn't arrive so early that it disrupts B's Q output on clock cycle x, and thus is not a function of clock period as we are looking at the same clock edge at both flops.
When a hold check fails, it is relatively easy to fix because the solution is to add delay. Implementation tools can increase the delay as simply as adding more wire length in the route.

In order to meet the hold requirement, the *shortest* possible clock-to-out, logic, and routing delays must cumulatively be longer than the hold requirement where the hold requirement is modified by the clock skew.

The **setup check** is verifying that the new value of A's Q output on clock cycle x arrives in time for B's Q output to consider it on clock cycle x+1, and is thus a function of the period. A failure of the setup check requires delay to be removed or the requirement (clock period) to be increased. Implementation tools cannot change the clock period (that is up to the designer), and there is only so much delay that can be removed without changing any functionality, so tools are not always able to change the placement and routing of circuit elements in order to pass the setup check.

In order to meet the setup requirement, the *longest* possible clock-to-out, logic, and routing delays must be cumulatively be shorter than the clock period (modified by clock skew and jitter/uncertainty) less the setup requirement.

Because the period of the clock (typically provided from off-chip via the clock input pins) must be known to calculate whether the setup check was met, all implementation tools will need at least one **timing constraint** provided by the designer indicating the period of the clock. Jitter/uncertainty is assumed to be 0 or a small default value, and the other values are always internally known by the tools for the target FPGA. If a clock period is not provided, most FPGA tools will verify the hold check then find the fastest clock that still allows all paths to meet setup, although it will spend minimal time optimizing slow routes to improve that fastest allowable clock since the actual speed needed is unknown.


----------


If the design has the required period constraints and non-synchronous logic is properly excluded from timing analysis (not covered in this document), but the design **still fails timing** there are a few options:

- The simplest option that doesn't affect functionality at all is to **adjust the directives** given to the tool in hopes that trying different optimization strategies will produce a result that meets timing. This is not reliably successful, but can often find a solution for borderline cases.

- The designer can always **reduce clock frequency** (increase the period) to meet  setup checks, but that has its own functional trade offs, namely that your system has reduced data throughput proportional to the clock speed reduction. 

- Designs can sometimes be **refactored** to do the same thing with simpler logic, or to do a different thing with an equally acceptable end result to reduce combinatorial delays, making setup checks easier.

- It is also common practice to change the described design (in the VHDL) to the same logical operations with the same throughput but more latency by using more flip-flops and splitting the combinatorial logic across multiple clock cycles. This is known as **pipelining** and leads to reduced combinatorial delays (and removes the routing delay between what was previously multiple layers of combinatorial logic). Some designs lend themselves well to pipelining, although it can be non-obvious if a long logic path is a monolithic operation, while other designs (such as those that incorporate a great deal of **feedback**) will not function at all with the additional latency that pipelining entails.


  [1]: http://i.stack.imgur.com/EzbBH.png

