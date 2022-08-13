---
title: "Paging - Virtual Addressing and Memory"
slug: "paging---virtual-addressing-and-memory"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Introduction
# History
## The first computers
Early computers had a block of memory that the programmer put code and data into, and the CPU executed within this environment. Given that the computers then were very expensive, it was unfortunate that it would do one job, stop and wait for the next job to be loaded into it, and then process that one.

## Multi-user, multi-processing
So computers quickly became more sophisticated and supported multiple users and/or programs simultaneously - but that's when problems started to arise with the simple "one block of memory" idea. If a computer was running two programs simultaneously, or running the same program for multiple users - whch of course would have required separate data for each user - then the management of that memory became critical.

## Example
For example: if a program was written to work at memory address 1000, but another program was already loaded there, then the new program couldn't be loaded. One way of solving this would be to make programs work with "relative addressing" - it didn't matter where the program was loaded, it just did everything relative to the memory address that it was loaded in. But that required hardware support.

## Sophistication
As computer hardware became more sophisticated, it was able to support larger blocks of memory, allowing for more simultaneous programs, and it became trickier to write programs that didn't interfere with what was already loaded. One stray memory reference could bring down not only the current program, but any other program in memory - including the Operating System itself!

# Solutions
What was needed was a mechanism that allowed blocks of memory to have _dynamic_ addresses. That way a program could be written to work with its blocks of memories at addresses that it recognised - and not be able to access other blocks for other programs (unless some cooperation allowed it to).

## Segmentation
One mechanism that implemented this was Segmentation. That allowed blocks of memory to be defined of all different sizes, and the program would need to define which Segment it wanted to access all the time.

### Problems
This technique was powerful - but its very flexibility was a problem. Since Segments essentially subdivided the available memory into different sized chunks, then the memory management for those Segments was an issue: allocation, deallocation, growing, shrinking, fragmentation - all required sophisticated routines and sometimes mass copying to implement.

## Paging
A different technique divided all of the memory into equal-sized blocks, called "Pages", which made the allocation and deallocation routines very simple, and did away with growing, shrinking and fragmentation (except for internal fragmentation, which is merely a problem of wastage).

### Virtual addressing
By dividing the memory into these blocks, they could be allocated to different programs as needed with whatever address the program needed it at. This "mapping" between the memory's physical address and the program's desired address is very powerful, and is the basis for every major processor's (Intel, ARM, MIPS, Power et. al.) memory management today.

### Hardware and OS support
The hardware performed the remapping automatically and continually, but required memory to define the tables of what to do. Of course, the housekeeping associated with this remapping had to be controlled by something. The Operating System would have to dole out the memory as required, and manage the tables of data required by the hardware to support what the programs required.

# Paging features
Once the hardware could do this remapping, what did it allow? The main driver was multiprocessing - the ability to run multiple programs, each with their "own" memory, protected from each other. But two other options included "sparse data", and "virtual memory".

## Multiprocessing
Each program was given their own, virtual "Address Space" - a range of addresses that they could have physical memory mapped into, at whatever addresses were desired. As long as there was enough physical memory to go around (although see "Virtual Memory" below), numerous programs could be supported simultaneously.

What's more, those programs _**couldn't**_ access memory that wasn't mapped into their virtual address space - protection between programs was automatic. If programs needed to communicate, they could ask the OS to arrange for a shared block of memory - a block of physical memory that was mapped into two different programs' address spaces simultaneously.

## Sparse Data
Allowing a huge virtual address space (4 GB is typical, to correspond with the 32-bit registers these processors typically had) does not in and of itself waste memory, if large areas of that address space go unmapped. This allows for the creation of huge data structures where only certain parts are mapped at any one time. Imagine a 3-dimensional array of 1,000 bytes in each direction: that would normally take a billion bytes! But a program could reserve a block of its virtual address space to "hold" this data, but only map small sections as they were populated. This makes for efficient programming, while not wasting memory for data that isn't needed yet.

## Virtual Memory
Above I used the term "Virtual Addressing" to describe the virtual-to-physical addressing performed by the hardware. This is often called "Virtual Memory" - but that term more correctly corresponds to the technique of using Virtual Addressing to support providing an illusion of more memory than is actually available.

It works like this:
* As programs are loaded and request more memory, the OS provides the memory from what it has available. As well as keeping track of what memory has been mapped, the OS also keeps track of when the memory is actually used - the hardware supports marking used pages.
* When the OS runs out of physical memory, it looks at all the memory that it has already handed out for whichever Page was used the least, or hadn't been used the longest. It saves that particular Page's contents to the hard disk, remembers where that was, marks it as "Not Present" to the hardware for the original owner, and then zeroes the Page and gives it to the new owner.
* If the original owner attempts to access that Page again, the hardware notifies the OS. The OS then allocates a new Page (perhaps having to do the previous step again!), loads up the old Page's contents, then hands the new Page to the original program.
  > The important point to notice is that since any Page can be mapped to any address, and each Page is the same size, then one Page is as good as any other - as long as the contents remain the same!
* If a program accesses an unmapped memory location, the hardware notifies the OS as before. This time, the OS notes that it wasn't a Page that had been saved away, so recognises it as a bug in the program, and terminates it!
  > This is actually what happens when your app mysteriously vanishes on you - perhaps with a MessageBox from the OS. It's also what (often) happens to cause an infamous Blue Screen or Sad Mac - the buggy program was in fact an OS driver that accessed memory that it shouldn't!

# Paging decisions
The hardware architects needed to make some big decisions about Paging, since the design would directly affect the design of the CPU! A very flexible system would have a high overhead, requiring large amounts of memory just to manage the Paging infrastructure itself.

## How big should a Page be?
In hardware, the easiest implementation of Paging would be to take an Address and divide it into two parts. The upper part would be an indicator of which Page to access, while the lower part would be the index into the Page for the required byte:

    +-----------------+------------+
    | Page index      | Byte index |
    +-----------------+------------+

It quickly became obvious though that small pages would require vast indexes for each program: even memory that wasn't mapped would need an entry in the table indicating this.

So instead a multi-tiered index is used. The address is broken into multiple parts (three are indicated in the below example), and the top part (commonly called a "Directory") indexes into the next part and so on until the final byte index into the final page is decoded:

    +-----------+------------+------------+
    | Dir index | Page index | Byte index |
    +-----------+------------+------------+

That means that a Directory index can indicate "not mapped" for a vast chunk of the address space, without requiring numerous Page indexes.

## How to optimise the usage of the Page Tables?
Every address access that the CPU will make will have to be mapped - the virtual-to-physical process must therefore be as efficient as possible. If the three-tier system described above were to be implemented, that would mean that every memory access would actually be three accesses: one into the Directory; one into the Page Table; and then finally the desired data itself. And if the CPU needed to perform housekeeping as well, such as indicating that this Page had now been accessed or written to, then that would require yet more accesses to update the fields.

Memory may be fast, but this would impose a triple-slowdown on all memory accesses during Paging! Luckily, most programs have a "locality of scope" - that is, if they access one location in memory, then future accesses will probably be nearby. And since Pages aren't too small, that mapping conversion would only need to be performed when a new Page was accessed: not for absolutely every access.

But even better would be to implement a cache of recently-accessed Pages, not just the most current one. The problem would be keeping up with what Pages had been accessed and what hadn't - the hardware would have to scan through the cache on every access to find the cached value. So the cache is implemented as a content-addressable cache: instead of being accessed by address, it is accessed by content - if the data requested is present, it is offered up, otherwise an empty location is flagged for filling in instead. The cache manages all of that.

This content-addressable cache is often called a Translation Lookaside Buffer (TLB), and is required to be managed by the OS as part of the Virtual Addressing subsystem. When the Directories or Page Tables are modified by the OS, it needs to notify the TLB to update its entries - or to simply invalidate them.


## 80386 Paging
# High Level Design
The 80386 is a 32-bit processor, with a 32-bit addressable memory space. The designers of the Paging subsystem noted that a 4K page design mapped into those 32 bits in quite a neat way - 10 bits, 10 bits and 12 bits:

    +-----------+------------+------------+
    | Dir index | Page index | Byte index |
    +-----------+------------+------------+
     3         2 2          1 1          0  Bit
     1         2 1          2 1          0  number

That meant that the Byte index was 12 bits wide, which would index into a 4K Page. The Directory and Page indexes were 10 bits, which would each map into a 1,024-entry table - and if those table entries were each 4 bytes, that would be 4K per table: also a Page!

So that's what they did:
* Each program would have its own Directory, a Page with 1,024 Page Entries that each defined where the next level Page Table was - if there was one.
* If there was, that Page Table would have 1,024 Page Entries that each defined where the last level Page was - if there was one.
* If there was, then that Page could have its Byte directly read out.

# Page Entry
Both the top-level Directory and the next-level Page Table is comprised of 1,024 Page Entries. The most important part of these entries is the address of what it is indexing: a Page Table or an actual Page. Note that this address doesn't need the full 32 bits - since everything is a Page, only the top 20 bits are significant. Thus the other 12 bits in the Page Entry can be used for other things: whether the next level is even present; housekeeping as to whether the page has been accessed or written to; and even whether writes should even be allowed!

    +--------------+----+------+-----+---+---+
    | Page Address | OS | Used | Sup | W | P |
    +--------------+----+------+-----+---+---+
    Page Address = Top 20 bits of Page Table or Page address
    OS           = Available for OS use
    Used         = Whether this page has been accessed or written to
    Sup          = Whether this page is Supervisory - only accessible by the OS
    W            = Whether this page is allowed to be Written
    P            = Whether this page is even Present

Note that if the `P` bit is 0, then the rest of the Entry is allowed to have anything that the OS wants to put in there - such as where the Page's contents mught be on the hard disk!

# Page Directory Base Register (`PDBR`)
If each program has its own Directory, how does the hardware know where to start mapping? Since the CPU is only running one program at a time, it has a single Control Register to hold the address of the current program's Directory. This is the Page Directory Base Register (`CR3`). As the OS swaps between different programs, it updates the `PDBR` with the relevant Page Directory for the program.

# Page Faults
Every time the CPU accesses memory, it has to map the indicated virtual address into the appropriate physical address. This is a three-step process:

1. Index the top 10 bits of the address into the Page indicated by the `PDBR` to get the address of the appropriate Page Table;
2. Index the next 10 bits of the address into the Page indicated by the Directory to get the address of the appropriate Page;
3. Index the last 12 bits of the address to get the data out of that Page.

Because both steps 1. and 2. above use Page Entries, each Entry could indicate a problem:
* The next level may be marked "Not Present";
* The next level may be marked as "Read Only" - and the operation is a Write;
* The next level may be marked as "Supervisor" - and it's the program accessing the memory, not the OS.

When such a problem is noted by the hardware, instead of completing the access it raises a Fault: Interrupt #14, the Page Fault. It also fills in some specific Control Registers with the information of why the Fault occurred: the address referenced; whether it was a Supervisor access; and whether it was a Write attempt.

The OS is expected to trap that Fault, decode the Control Registers, and decide what to do. If it's an invalid access, it can terminate the faulting program. If it's a Virtual Memory access though, the OS should allocate a new Page (which may need to vacate a Page that is already in use!), fill it with the required contents (either all zeroes, or the previous contents loaded back from disk), map the new Page into the appropriate Page Table, mark it as present, then resume the faulting instruction. This time the access will progress successfully, and the program will proceed with no knowledge that anything special happened (unless it takes a look at the clock!)


## 80486 Paging
The 80486 Paging Subsystem was very similar to the 80386 one. It was backward compatible, and the only new features were to allow for memory cache control on a Page-by-Page basis - the OS designers could mark specific pages as not to be cached, or to use different write-through or write-back caching techniques.

In all other respects, the "80386 Paging" example is applicable.


## Pentium Paging
When the Pentium was being developed, memory sizes, and the programs that ran in them, were getting larger. The OS had to do more and more work to maintain the Paging Subsystem just in the sheer number of Page Indexes that needed to be updated when large programs or data sets were being used.

So the Pentium designers added a simple trick: they put an extra bit in the Entries of the Page Directory that indicated whether the next level was a Page Table (as before) - or went directly to a 4 MB Page! By having the concept of 4 MB Pages, the OS wouldn't have to create a Page Table and fill it with 1,024 Entries that were basically indexing addresses 4K higher than the previous one.

## Address layout
    +-----------+----------------------+
    | Dir Index | 4MB Byte Index       |
    +-----------+----------------------+
     3         2 2                    0   Bit
     1         2 1                    0   number

## Directory Entry layout
    +-----------+----+---+------+-----+---+---+
    | Page Addr | OS | S | Used | Sup | W | P |
    +-----------+----+---+------+-----+---+---+
    Page Addr = Top 20 bits of Page Table or Page address
    OS        = Available for OS use
    S         = Size of Next Level: 0 = Page Table, 1 = 4 MB Page
    Used      = Whether this page has been accessed or written to
    Sup       = Whether this page is Supervisory - onlly accessible by the OS
    W         = Whether this page is allowed to be Written
    P         = Whether this page is even Present

Of course, that had some ramifications:
* The 4 MB Page had to start on a 4 MB address boundary, just like the 4K Pages had to start on a 4K address boundary.
* All 4 MB had to belong to a single Program - or be shared by multiple ones.

This was perfect for use for large-memory peripherals, such as graphics adapters, that had large address space windows that needed to be mapped for the OS to use.


## Physical Address Extension (PAE)
# Introduction
As memory prices dropped, Intel-based PCs were able to have more and more RAM affordably, alleviating many users' problems with running many of the ever-larger applications that were being produced simultaneously. While virtual memory allowed memory to be virtually "created" - swapping existing "old" Page contents to the hard disk to allow "new" data to be stored - this slowed down the running of the programs as Page "thrashing" kept continually swapping data on and off the hard disk.

## More RAM
What was needed was the ability to access more physical RAM - but it was already a 32-bit address bus, so any increase would require larger address registers. Or would it? When developing the Pentium Pro (and even the Pentium M), as a stop-gap until 64-bit processors could be produced, to add more Physical Address bits (allowing more Physical memory) _without_ changing the number of register bits. This could be achieved since Virtual Addresses were mapped to Physical Addresses anyway - all that needed to change was the mapping system.

# Design
The existing system could access a maximum of 32 bits of Physical Addresses. Increasing this required a complete change of the Page Entry structure, from 32 to 64 bits. It was decided to keep the minimum granularity at 4K Pages, so the 64-bit Entry would have 52 bits of Address and 12 bits of Control (like the previous Entry had 20 bits of Address and 12 bits of Control).

Having a 64-bit Entry, but a Page size of (still) 4K, meant that there would only be 512 Entries per Page Table or Directory, instead of the previous 1,024. That meant that the 32-bit Virtual Address would be divided differently than before:

    +-----+-----------+------------+------------+
    | DPI | Dir Index | Page Index | Byte Index |
    +-----+-----------+------------+------------+
     3   3 2         2 2          1 1          0   Bit
     1   0 9         1 0          2 1          0   number

     DPI        = 2-bit index into Directory Pointer Table
     Dir Index  = 9-bit index into Directory
     Page Index = 9-bit index into Page Table
     Byte Index = 12-bit index into Page (as before)

Chopping one bit from both the Directory Index and Page Index gave two bits for a third tier of mapping: they called this the Page Directory Pointer Table (PDPT), a table of exactly four 64-bit Entries that addressed four Directories instead of the previous one. The PDBR (`CR3`) now pointed to the PDPT instead - which, since `CR3` was only 32 bits, needed to be stored in the first 4 GB of RAM for accessibility. Note that since the low bits of `CR3` are used for Control, the PDPT has to start on a 32-byte boundary.

# Page Size Extension (PSE)
And, since the previous 4MB Pages were such a good idea, they wanted to be able to support large Pages again. This time though, removing the last layer of the tier system didn't produce 10+12 bit 4MB Pages, but 9+12 bit 2MB Pages instead.


## PSE-32 (and PSE-40)
Since the Physical Address Extension (PAE) mode that was introduced in the Pentium Pro (and Pentum M) was such a change to the Operating System memory management subsystem, when Intel designed the Pentium II they decided to enhance the "normal" Page mode to support the new Physical Address bits of the processor within the previously-defined 32-bit Entries.

They realised that when a 4MB Page was used, the Directory Entry looked like this:

    +-----------+------------+---------+
    | Dir Index |  Unused    | Control |
    +-----------+------------+---------+
The Dir Index and Control areas of the Entry were the same, but the block of unused bits between them - which would be used by the Page Index if it existed - were wasted. So they decided to use that area _to define the upper Physical Address bits above 31_!

    +-----------+------+-----+---------+
    | Dir Index |Unused|Upper| Control |
    +-----------+------+-----+---------+
This allowed RAM above 4 GB to be accessible to OSes that didn't adopt the PAE mode - with a little extra logic, they could provide large amounts of extra RAM to the system, albeit no more than the normal 4GB to each program. At first only 4 bits were added, allowing for 36-bit Physical Addressing, so this mode was called Page Size Extension 36 (PSE-36). It didn't actually change the Page size, only the Addressing however.

The limitation of this though was that only 4MB Pages above 4GB were definable - 4K Pages weren't allowed. Adoption of this mode [wasn't wide][1] - it was reportedly slower than using PAE, and Linux didn't end up ever using it.

Nevertheless, in later processors that had even more Physical Address bits, both AMD and Intel widened the PSE area to 8 bits, which some people dubbed "PSE-40"

  [1]: https://en.wikipedia.org/wiki/PSE-36#Usage

