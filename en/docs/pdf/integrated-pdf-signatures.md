---
title: "Integrated PDF signatures"
slug: "integrated-pdf-signatures"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

Integrated PDF signatures are explained quite graphically and in more detail in the Adobe document [Digital Signatures in a PDF][1]. They furthermore are specified in the PDF specification ISO 32000-1:2008 made available [here by Adobe][2] in section 12.8 *Digital Signatures.*

Depending on your programming context, there are many PDF libraries supporting the creation of integrated PDF signatures and also many products using these libraries. Some of them are even available for free subject e.g. to the AGPL.

*(This article is essentially a copy of [this answer on the information security site][5], [this answer on stackoverflow][6], and some words from the [PDF specification][2].)*


  [1]: http://www.adobe.com/devnet-docs/acrobatetk/tools/DigSig/Acrobat_DigitalSignatures_in_PDF.pdf
  [2]: http://www.adobe.com/content/dam/Adobe/en/devnet/acrobat/pdfs/PDF32000_2008.pdf
  [5]: http://security.stackexchange.com/a/35131/16096
  [6]: http://stackoverflow.com/a/16711745/1729265

## Allowed and disallowed changes to a signed document
In the Adobe technical white paper [Adobe Acrobat 9 Digital Signatures, Changes and Improvements][1], especially its section "Allowed and disallowed changes", Adobe *clarifies the allowed changes* (as seen by Acrobat 9 and up) *that can be made to a certified or signed document without invalidating the signatures applied to the document.*

># Allowed actions for certified documents
>## Certified with no changes allowed
>### Allowed
>  * No changes allowed
>### Disallowed
>  * Digitally signing
>  * Supplying form field values
>  * Adding or editing annotations
>  * Adding form fields
>  * Changing page content
>## Certified with form fill-in and digital signatures allowed
>### Allowed
>  * Supplying form field values
>  * Digitally signing
>### Disallowed
>  * Adding or editing annotations
>  * Adding form fields
>  * Changing page content
>## Certified with annotations, form fill-in, and digital signatures, allowed
>### Allowed
>  * Adding or editing annotations
>  * Supplying form field values
>  * Digitally signing
>### Disallowed
>  * Adding form fields
>  * Changing page content
># Allowed actions for signed but uncertified documents
>### Allowed
>  * Adding signature fields (see *Limitations on adding signature fields to signed but uncertified documents*)
>  * Adding or editing annotations
>  * Supplying form field values
>  * Digitally signing
>### Disallowed
>  * Adding form fields other than signature fields
>  * Changing page content

(Even though not explicitly mentioned here, instantiating page templates most likely also is allowed whenever form fill-ins are allowed as that would conform to the PDF standard, cf. [ISO 32000-1][2] section 12.8.2.2.2.)


  [1]: http://wwwimages.adobe.com/www.adobe.com/content/dam/Adobe/en/devnet/reader/pdfs/readercomp_digitalsignatures.pdf
  [2]: http://www.adobe.com/content/dam/Adobe/en/devnet/acrobat/pdfs/PDF32000_2008.pdf


## How integrated PDF signatures are "integrated"
You want to add a signature to a PDF in a way that a standard conform PDF viewer (e.g. Adobe Reader) will recognize, display, and validate as an integrated PDF signature.

In that case you cannot simply externally create a signature covering the original PDF as is and expect to now have to merely somehow append that signature to the file. Instead you first have to build a new revision of the PDF document which includes a PDF AcroForm signature field whose value is a signature dictionary whose **/Contents** entry will eventually hold the signature of the whole new revision with the exception of the **/Contents** entry contents.

![Signature embedded in PDF][1]

  [1]: http://i.stack.imgur.com/DkekJ.png

## Multiple signatures in a single PDF
If multiple signatures are to be integrated into a PDF, this is done by means of incremental PDF updates (explicitly **not** by adding multiple SignerInfo structures to a single integrated CMS signature container!):

![Multiple signatures and incremental updates][2]

  [2]: http://i.stack.imgur.com/GKdYw.jpg

The cryptographic verification of these signatures merely guarantees that the byte range signed by the respective signature has not been tampered with. If does not guarantee that PDF objects defined in that range have not been replaced with other ones in any newer revision.

Checking in what way the additions in later revisions change the appearance of the content signed by an earlier signature is a separate verification task which is not trivial.

## The signed byte range
The [specification][2] says:

> A byte range digest shall be computed over a range of bytes in the file, that shall be indicated by the ByteRange entry in the signature dictionary. **This range should be the entire file, including the signature dictionary but excluding the signature value itself (the Contents entry). Other ranges may be used but since they do not check for all changes to the document, their use is not recommended.**

This seems to allow that you first create a signature for the original PDF and then append a new revision holding that signature indicating that range of signed bytes only contains that original revision, not the extended revision without only the signature.

In reality, though, PDF viewers (especially Adobe Reader) will only accept signatures which follow the recommendation that the signed *range should be the entire file, including the signature dictionary but excluding the signature value itself.*

Newer specifications, e.g. the ETSI PAdES specification ETSI TS 102 778 (cf. [section 5.1 item b in part 2][3] and [section 4.2 item c in part 3][4]) even make this recommendation officially a requirements, and so will ISO 32000-2.

  [2]: http://www.adobe.com/content/dam/Adobe/en/devnet/acrobat/pdfs/PDF32000_2008.pdf
  [3]: http://www.etsi.org/deliver/etsi_ts/102700_102799/10277802/01.02.01_60/ts_10277802v010201p.pdf
  [4]: http://www.etsi.org/deliver/etsi_ts/102700_102799/10277803/01.02.01_60/ts_10277803v010201p.pdf


## PDF signature types
A PDF document may contain the following standard types of signatures:

* Any number of **approval signatures** in signature form fields.
* At most one **certification signature** in a signature form field. It enables the author to specify what changes shall be permitted to be made to the document and what changes invalidate the author’s signature, cf. *"Allowed and disallowed changes to a signed document"* below.
* At most two **usage rights signatures** referenced from the PDF's permissions dictionary. Usage rights signatures shall be used to enable additional interactive features that may not be available by default in a reader. These signatures usually are not presented to the user but merely evaluated by the PDF reader program.

## Interoperable signature types
As the header already says, the following list contains "interoperable signature types" which are more or less strictly defined. The [PDF specification][1] specifies a way to also include completely custom signing schemes. But let us assume we are in an interoperable situation. The the collection of signature types burns down to:

* **adbe.x509.rsa_sha1** defined in [ISO 32000-1][2] section 12.8.3.2 *PKCS#1 Signatures*; the signature value **Contents** contain a *DER-encoded PKCS#1 binary data object*; this data object is a fairly naked signature, in case of RSA an encrypted structure containing the padded document hash and the hash algorithm.

* **adbe.pkcs7.sha1** defined in [ISO 32000-1][3] section 12.8.3.3 *PKCS#7 Signatures*; the signature value **Contents** contain a *DER-encoded PKCS#7 binary data object*; this data object is a big container object which can also contain meta-information, e.g. it may contain certificates for building certificate chains, revocation information for certificate revocation checks, digital time stamps to fix the signing time, ... *The SHA1 digest of the document’s byte range shall be encapsulated in the PKCS#7 SignedData field with ContentInfo of type Data. The digest of that SignedData shall be incorporated as the normal PKCS#7 digest.*

* **adbe.pkcs7.detached** defined in [ISO 32000-1][4] section 12.8.3.3 *PKCS#7 Signatures*; the signature value **Contents** contain a *DER-encoded PKCS#7 binary data object*, see above. *The original signed message digest over the document’s byte range shall be incorporated as the normal PKCS#7 SignedData field. No data shall be encapsulated in the PKCS#7 SignedData field.*

* **ETSI.CAdES.detached** defined in [ETSI TS 102 778-3][5] and will become integrated in ISO 32000-2; the signature value **Contents** contain a *DER-encoded SignedData object as specified in CMS*; CMS signature containers are close relatives to PKCS#7 signature containers, see above. This essentially is a differently profiled and stricter defined variant of adbe.pkcs7.detached.

* **ETSI.RFC3161** defined in [ETSI TS 102 778-4][6] and will become integrated in ISO 32000-2; the signature value **Contents** contain a *TimeStampToken as specified in RFC 3161*; time stamp tokens again are a close relative to PKCS#7 signature containers, see above, but they contain a special data sub-structure harboring the document hash, the time of the stamp creation, and information on the issuing time server.

I would propose studying the specifications I named and the documents referenced from there, mostly RFCs. Based on that knowledge you can easily find the appropriate BouncyCastle classes to analyze the different signature **Contents**.

*(Copied from [this answer](https://stackoverflow.com/a/25974969/1729265))*

  [1]: http://www.adobe.com/content/dam/Adobe/en/devnet/acrobat/pdfs/PDF32000_2008.pdf
  [2]: http://www.adobe.com/content/dam/Adobe/en/devnet/acrobat/pdfs/PDF32000_2008.pdf
  [3]: http://www.adobe.com/content/dam/Adobe/en/devnet/acrobat/pdfs/PDF32000_2008.pdf
  [4]: http://www.adobe.com/content/dam/Adobe/en/devnet/acrobat/pdfs/PDF32000_2008.pdf
  [5]: http://www.etsi.org/deliver/etsi_ts/102700_102799/10277803/01.02.01_60/ts_10277803v010201p.pdf
  [6]: http://www.etsi.org/deliver/etsi_ts/102700_102799/10277804/01.01.02_60/ts_10277804v010102p.pdf

