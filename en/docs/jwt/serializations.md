---
title: "Serializations"
slug: "serializations"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## JWS Compact Serialization
The Compact Serialization is the most common serialization format and is designed to be used in a web context.

JWS are represented into a string that contains Base64 Url Safe encoded information seperated by an dot ".".

This mode does not support unprotected headers.

*Line breaks added for readability*

```
BASE64URL(UTF8(JWS Protected Header)) || '.' ||
BASE64URL(JWS Payload) || '.' ||
BASE64URL(JWS Signature)
```

Example
-------

```
eyJhbGciOiJQUzM4NCIsImtpZCI6ImJpbGJvLmJhZ2dpbnNAaG9iYml0b24uZX
hhbXBsZSJ9
.
SXTigJlzIGEgZGFuZ2Vyb3VzIGJ1c2luZXNzLCBGcm9kbywgZ29pbmcgb3V0IH
lvdXIgZG9vci4gWW91IHN0ZXAgb250byB0aGUgcm9hZCwgYW5kIGlmIHlvdSBk
b24ndCBrZWVwIHlvdXIgZmVldCwgdGhlcmXigJlzIG5vIGtub3dpbmcgd2hlcm
UgeW91IG1pZ2h0IGJlIHN3ZXB0IG9mZiB0by4
.
cu22eBqkYDKgIlTpzDXGvaFfz6WGoz7fUDcfT0kkOy42miAh2qyBzk1xEsnk2I
pN6-tPid6VrklHkqsGqDqHCdP6O8TTB5dDDItllVo6_1OLPpcbUrhiUSMxbbXU
vdvWXzg-UD8biiReQFlfz28zGWVsdiNAUf8ZnyPEgVFn442ZdNqiVJRmBqrYRX
e8P_ijQ7p8Vdz0TTrxUeT3lm8d9shnr2lfJT8ImUjvAA2Xez2Mlp8cBE5awDzT
0qI0n6uiP1aCN_2_jLAeQTlqRHtfa64QQSUmFAAjVKPbByi7xho0uTOcbH510a
6GYmJUAfmWjwZ6oD4ifKo8DYM-X72Eaw
```


## JWE Compact Serialization
The Compact Serialization is the most common serialization format and is designed to be used in a web context.

JWE are represented into a string that contains Base64 Url Safe encoded information seperated by an dot ".".

This mode does not support unprotected headers or AAD.

*Line breaks added for readability*

```
BASE64URL(UTF8(JWE Protected Header)) || '.' ||
BASE64URL(JWE Encrypted Key) || '.' ||
BASE64URL(JWE Initialization Vector) || '.' ||
BASE64URL(JWE Ciphertext) || '.' ||
BASE64URL(JWE Authentication Tag)
```

Example
-------

```
eyJhbGciOiJSU0EtT0FFUCIsImtpZCI6InNhbXdpc2UuZ2FtZ2VlQGhvYmJpdG
9uLmV4YW1wbGUiLCJlbmMiOiJBMjU2R0NNIn0
.
rT99rwrBTbTI7IJM8fU3Eli7226HEB7IchCxNuh7lCiud48LxeolRdtFF4nzQi
beYOl5S_PJsAXZwSXtDePz9hk-BbtsTBqC2UsPOdwjC9NhNupNNu9uHIVftDyu
cvI6hvALeZ6OGnhNV4v1zx2k7O1D89mAzfw-_kT3tkuorpDU-CpBENfIHX1Q58
-Aad3FzMuo3Fn9buEP2yXakLXYa15BUXQsupM4A1GD4_H4Bd7V3u9h8Gkg8Bpx
KdUV9ScfJQTcYm6eJEBz3aSwIaK4T3-dwWpuBOhROQXBosJzS1asnuHtVMt2pK
IIfux5BC6huIvmY7kzV7W7aIUrpYm_3H4zYvyMeq5pGqFmW2k8zpO878TRlZx7
pZfPYDSXZyS0CfKKkMozT_qiCwZTSz4duYnt8hS4Z9sGthXn9uDqd6wycMagnQ
fOTs_lycTWmY-aqWVDKhjYNRf03NiwRtb5BE-tOdFwCASQj3uuAgPGrO2AWBe3
8UjQb0lvXn1SpyvYZ3WFc7WOJYaTa7A8DRn6MC6T-xDmMuxC0G7S2rscw5lQQU
06MvZTlFOt0UvfuKBa03cxA_nIBIhLMjY2kOTxQMmpDPTr6Cbo8aKaOnx6ASE5
Jx9paBpnNmOOKH35j_QlrQhDWUN6A2Gg8iFayJ69xDEdHAVCGRzN3woEI2ozDR
s
.
-nBoKLH0YkLZPSI9
.
o4k2cnGN8rSSw3IDo1YuySkqeS_t2m1GXklSgqBdpACm6UJuJowOHC5ytjqYgR
L-I-soPlwqMUf4UgRWWeaOGNw6vGW-xyM01lTYxrXfVzIIaRdhYtEMRBvBWbEw
P7ua1DRfvaOjgZv6Ifa3brcAM64d8p5lhhNcizPersuhw5f-pGYzseva-TUaL8
iWnctc-sSwy7SQmRkfhDjwbz0fz6kFovEgj64X1I5s7E6GLp5fnbYGLa1QUiML
7Cc2GxgvI7zqWo0YIEc7aCflLG1-8BboVWFdZKLK9vNoycrYHumwzKluLWEbSV
maPpOslY2n525DxDfWaVFUfKQxMF56vn4B9QMpWAbnypNimbM8zVOw
.
UCGiqJxhBI3IFVdPalHHvA
```


## General JWS JSON Serialization Syntax
The JWS JSON Serialization represents digitally signed or MACed content as a JSON object.  This representation is neither optimized for compactness nor URL-safe.

This syntax is optimized for more than one digital signature and/or MAC operation.

*Line breaks added for readability*

```
     {
      "payload":"<payload contents>",
      "signatures":[
       {"protected":"<integrity-protected header 1 contents>",
        "header":<non-integrity-protected header 1 contents>,
        "signature":"<signature 1 contents>"},
       ...
       {"protected":"<integrity-protected header N contents>",
        "header":<non-integrity-protected header N contents>,
        "signature":"<signature N contents>"}]
     }
```

Example
-------

```
   {
     "payload": "SXTigJlzIGEgZGFuZ2Vyb3VzIGJ1c2luZXNzLCBGcm9kbywg
         Z29pbmcgb3V0IHlvdXIgZG9vci4gWW91IHN0ZXAgb250byB0aGUgcm9h
         ZCwgYW5kIGlmIHlvdSBkb24ndCBrZWVwIHlvdXIgZmVldCwgdGhlcmXi
         gJlzIG5vIGtub3dpbmcgd2hlcmUgeW91IG1pZ2h0IGJlIHN3ZXB0IG9m
         ZiB0by4",
     "signatures": [
       {
         "protected": "eyJhbGciOiJSUzI1NiJ9",
         "header": {
           "kid": "bilbo.baggins@hobbiton.example"
         },
         "signature": "MIsjqtVlOpa71KE-Mss8_Nq2YH4FGhiocsqrgi5Nvy
             G53uoimic1tcMdSg-qptrzZc7CG6Svw2Y13TDIqHzTUrL_lR2ZFc
             ryNFiHkSw129EghGpwkpxaTn_THJTCglNbADko1MZBCdwzJxwqZc
             -1RlpO2HibUYyXSwO97BSe0_evZKdjvvKSgsIqjytKSeAMbhMBdM
             ma622_BG5t4sdbuCHtFjp9iJmkio47AIwqkZV1aIZsv33uPUqBBC
             XbYoQJwt7mxPftHmNlGoOSMxR_3thmXTCm4US-xiNOyhbm8afKK6
             4jU6_TPtQHiJeQJxz9G3Tx-083B745_AfYOnlC9w"
       },
       {
         "header": {
           "alg": "ES512",
           "kid": "bilbo.baggins@hobbiton.example"
         },
         "signature": "ARcVLnaJJaUWG8fG-8t5BREVAuTY8n8YHjwDO1muhc
             dCoFZFFjfISu0Cdkn9Ybdlmi54ho0x924DUz8sK7ZXkhc7AFM8Ob
             LfTvNCrqcI3Jkl2U5IX3utNhODH6v7xgy1Qahsn0fyb4zSAkje8b
             AWz4vIfj5pCMYxxm4fgV3q7ZYhm5eD"
       },
       {
         "protected": "eyJhbGciOiJIUzI1NiIsImtpZCI6IjAxOGMwYWU1LT
             RkOWItNDcxYi1iZmQ2LWVlZjMxNGJjNzAzNyJ9",
         "signature": "s0h6KThzkfBBBkLspW1h84VsJZFTsPPqMDA7g1Md7p
             0"
       }
     ]
   }
```


## Flattened JWS JSON Serialization Syntax
As the General JWS JSON Serialization Syntax, the JWS JSON Serialization represents digitally signed or MACed content as a JSON object. This representation is neither optimized for compactness nor URL-safe.

The flattened syntax is optimized for the single digital signature or MAC case.

*Line breaks added for readability*

```
     {
      "payload":"<payload contents>",
      "protected":"<integrity-protected header contents>",
      "header":<non-integrity-protected header contents>,
      "signature":"<signature contents>"
     }
```

Example
-------

```
   {
     "payload": "SXTigJlzIGEgZGFuZ2Vyb3VzIGJ1c2luZXNzLCBGcm9kbywg
         Z29pbmcgb3V0IHlvdXIgZG9vci4gWW91IHN0ZXAgb250byB0aGUgcm9h
         ZCwgYW5kIGlmIHlvdSBkb24ndCBrZWVwIHlvdXIgZmVldCwgdGhlcmXi
         gJlzIG5vIGtub3dpbmcgd2hlcmUgeW91IG1pZ2h0IGJlIHN3ZXB0IG9m
         ZiB0by4",
     "protected": "eyJhbGciOiJIUzI1NiJ9",
     "header": {
       "kid": "018c0ae5-4d9b-471b-bfd6-eef314bc7037"
     },
     "signature": "bWUSVaxorn7bEF1djytBd0kHv70Ly5pvbomzMWSOr20"
   }
```


## General JWE JSON Serialization Syntax
The JWE JSON Serialization represents encrypted content as a JSON object. 
This representation is neither optimized for compactness nor URL safe.

This syntax is optimized for more than one recipient.

*Line breaks added for readability*

```
     {
      "protected":"<integrity-protected shared header contents>",
      "unprotected":<non-integrity-protected shared header contents>,
      "recipients":[
       {"header":<per-recipient unprotected header 1 contents>,
        "encrypted_key":"<encrypted key 1 contents>"},
       ...
       {"header":<per-recipient unprotected header N contents>,
        "encrypted_key":"<encrypted key N contents>"}],
      "aad":"<additional authenticated data contents>",
      "iv":"<initialization vector contents>",
      "ciphertext":"<ciphertext contents>",
      "tag":"<authentication tag contents>"
     }
```

Example
-------

```
   {
     "recipients": [
       {
         "encrypted_key": "dYOD28kab0Vvf4ODgxVAJXgHcSZICSOp8M51zj
             wj4w6Y5G4XJQsNNIBiqyvUUAOcpL7S7-cFe7Pio7gV_Q06WmCSa-
             vhW6me4bWrBf7cHwEQJdXihidAYWVajJIaKMXMvFRMV6iDlRr076
             DFthg2_AV0_tSiV6xSEIFqt1xnYPpmP91tc5WJDOGb-wqjw0-b-S
             1laS11QVbuP78dQ7Fa0zAVzzjHX-xvyM2wxj_otxr9clN1LnZMbe
             YSrRicJK5xodvWgkpIdkMHo4LvdhRRvzoKzlic89jFWPlnBq_V4n
             5trGuExtp_-dbHcGlihqc_wGgho9fLMK8JOArYLcMDNQ",
         "header": {
           "alg": "RSA1_5",
           "kid": "frodo.baggins@hobbiton.example"
         }
       },
       {
         "encrypted_key": "ExInT0io9BqBMYF6-maw5tZlgoZXThD1zWKsHi
             xJuw_elY4gSSId_w",
         "header": {
           "alg": "ECDH-ES+A256KW",
           "kid": "peregrin.took@tuckborough.example",
           "epk": {
             "kty": "EC",
             "crv": "P-384",
             "x": "Uzdvk3pi5wKCRc1izp5_r0OjeqT-I68i8g2b8mva8diRhs
                 E2xAn2DtMRb25Ma2CX",
             "y": "VDrRyFJh-Kwd1EjAgmj5Eo-CTHAZ53MC7PjjpLioy3ylEj
                 I1pOMbw91fzZ84pbfm"
           }
         }
       },
       {
         "encrypted_key": "a7CclAejo_7JSuPB8zeagxXRam8dwCfmkt9-Wy
             TpS1E",
         "header": {
           "alg": "A256GCMKW",
           "kid": "18ec08e1-bfa9-4d95-b205-2b4dd1d4321d",
           "tag": "59Nqh1LlYtVIhfD3pgRGvw",
           "iv": "AvpeoPZ9Ncn9mkBn"
         }
       }
     ],
     "unprotected": {
       "cty": "text/plain"
     },
     "protected": "eyJlbmMiOiJBMTI4Q0JDLUhTMjU2In0",
     "iv": "VgEIHY20EnzUtZFl2RpB1g",
     "ciphertext": "ajm2Q-OpPXCr7-MHXicknb1lsxLdXxK_yLds0KuhJzfWK
         04SjdxQeSw2L9mu3a_k1C55kCQ_3xlkcVKC5yr__Is48VOoK0k63_QRM
         9tBURMFqLByJ8vOYQX0oJW4VUHJLmGhF-tVQWB7Kz8mr8zeE7txF0MSa
         P6ga7-siYxStR7_G07Thd1jh-zGT0wxM5g-VRORtq0K6AXpLlwEqRp7p
         kt2zRM0ZAXqSpe1O6FJ7FHLDyEFnD-zDIZukLpCbzhzMDLLw2-8I14FQ
         rgi-iEuzHgIJFIJn2wh9Tj0cg_kOZy9BqMRZbmYXMY9YQjorZ_P_JYG3
         ARAIF3OjDNqpdYe-K_5Q5crGJSDNyij_ygEiItR5jssQVH2ofDQdLCht
         azE",
     "tag": "BESYyFN7T09KY7i8zKs5_g"
   }
```


## Flattened JWE JSON Serialization Syntax
The flattened JWE JSON Serialization syntax is based upon the general syntax, but flattens it, optimizing it for the single-recipient case.

*Line breaks added for readability*

```
     {
      "protected":"<integrity-protected header contents>",
      "unprotected":<non-integrity-protected header contents>,
      "header":<more non-integrity-protected header contents>,
      "encrypted_key":"<encrypted key contents>",
      "aad":"<additional authenticated data contents>",
      "iv":"<initialization vector contents>",
      "ciphertext":"<ciphertext contents>",
      "tag":"<authentication tag contents>"
     }
```

Example
-------

```
   {
     "protected": "eyJhbGciOiJBMTI4S1ciLCJraWQiOiI4MWIyMDk2NS04Mz
         MyLTQzZDktYTQ2OC04MjE2MGFkOTFhYzgiLCJlbmMiOiJBMTI4R0NNIn
         0",
     "encrypted_key": "4YiiQ_ZzH76TaIkJmYfRFgOV9MIpnx4X",
     "aad": "WyJ2Y2FyZCIsW1sidmVyc2lvbiIse30sInRleHQiLCI0LjAiXSxb
         ImZuIix7fSwidGV4dCIsIk1lcmlhZG9jIEJyYW5keWJ1Y2siXSxbIm4i
         LHt9LCJ0ZXh0IixbIkJyYW5keWJ1Y2siLCJNZXJpYWRvYyIsIk1yLiIs
         IiJdXSxbImJkYXkiLHt9LCJ0ZXh0IiwiVEEgMjk4MiJdLFsiZ2VuZGVy
         Iix7fSwidGV4dCIsIk0iXV1d",
     "iv": "veCx9ece2orS7c_N",
     "ciphertext": "Z_3cbr0k3bVM6N3oSNmHz7Lyf3iPppGf3Pj17wNZqteJ0
         Ui8p74SchQP8xygM1oFRWCNzeIa6s6BcEtp8qEFiqTUEyiNkOWDNoF14
         T_4NFqF-p2Mx8zkbKxI7oPK8KNarFbyxIDvICNqBLba-v3uzXBdB89fz
         OI-Lv4PjOFAQGHrgv1rjXAmKbgkft9cB4WeyZw8MldbBhc-V_KWZslrs
         LNygon_JJWd_ek6LQn5NRehvApqf9ZrxB4aq3FXBxOxCys35PhCdaggy
         2kfUfl2OkwKnWUbgXVD1C6HxLIlqHhCwXDG59weHrRDQeHyMRoBljoV3
         X_bUTJDnKBFOod7nLz-cj48JMx3SnCZTpbQAkFV",
     "tag": "vOaH_Rajnpy_3hOtqvZHRA"
   }
```


