---
title: "Bit Syntax Defaults"
slug: "bit-syntax-defaults"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

Blah blah blah.

## Rewrite of the docs.
> 4.4 Defaults  
>
> [Beginning omitted: <<3.14>> isn't even legal syntax.]  
>      
> The default Size depends on the type. For integer it is 8. For float it is 64. For binary it is the actual size of the
> specified binary:
> 
>     1> Bin = << 17/integer, 3.2/float, <<97, 98, 99>>/binary >>. 
>     <<17,64,9,153,153,153,153,153,154,97,98,99>>
>       ^ |<-------------------------->|<------>|
      |             float=64          binary=24
    integer=8

>     2> size(Bin). % Returns the number of bytes:
>     12            % 8 bits + 64 bits + 3*8 bits = 96 bits => 96/8 = 12 bytes
> 
> In matching, a binary segment without a Size is only allowed at the end of the pattern, and the default Size is the rest of the
> binary on the right hand side of the match:  
> 
>     25> Bin = <<97, 98, 99>>.
>     <<"abc">>
>     
>     26> << X/integer, Rest/binary >> = Bin.
>     <<"abc">>
>     
>     27> X.
>     97
>     
>     28> Rest.
>     <<"bc">>
> 
> All other segments with type binary in a pattern must specify a Size:
> 
>     12> Bin = <<97, 98, 99, 100>>.         
>     <<"abcd">>
>     
>     13> << B:1/binary, X/integer, Rest/binary >> = Bin. %'unit' defaults to 8 for  
>     <<"abcd">>                    %binary type, total segment size is Size * unit  
>     
>     14> B.
>     <<"a">>
>     
>     15> X.
>     98
>     
>     16> Rest.
>     <<"cd">>
> 
>     17> << B2/binary, X2/integer, Rest2/binary >> = Bin. 
>     * 1: a binary field without size is only allowed at the end of a binary pattern


