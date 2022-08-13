---
title: "Procedural Blocks"
slug: "procedural-blocks"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Syntax
- always @ (posedge clk) begin /* statements */ end
- always @ (negedge clk) begin /* statements */ end
- always @ (posedge clk or posedge reset) // may synthesize less efficiently than synchronous reset


## Simple counter
A counter using an FPGA style flip-flop initialisation:

    module counter(
        input clk,
        output reg[7:0] count
    )
    initial count = 0;
    always @ (posedge clk) begin
        count <= count + 1'b1;
    end

A counter implemented using asynchronous resets suitable for ASIC synthesis:

    module counter(
      input            clk,  
      input            rst_n, // Active-low reset
      output reg [7:0] count
    )
    always @ (posedge clk or negedge rst_n) begin
      if (~rst_n) begin
        count <= 'b0;
      end
      else begin
        count <= count + 1'b1;
      end
    end

The procedural blocks in these examples increment `count` at every rising clock edge.

## Non-blocking assignments
A non-blocking assignment (`<=`) is used for assignment inside edge-sensitive `always` blocks. Within a block, the new values are not visible until the entire block has been processed. For example:

    module flip(
        input clk,
        input reset
    )
    reg f1;
    reg f2;

    always @ (posedge clk) begin
      if (reset) begin // synchronous reset
        f1 <= 0;
        f2 <= 1;
      end
      else begin
        f1 <= f2;
        f2 <= f1;
      end
    end
    endmodule


Notice the use of non-blocking (`<=`) assignments here. Since the first assignment doesn't actually take effect until after the procedural block, the second assignment does what is intended and actually swaps the two variables -- unlike in a blocking assignment (`=`) or assignments in other languages; `f1` still has its original value on the right-hand-side of the second assignment in the block.

