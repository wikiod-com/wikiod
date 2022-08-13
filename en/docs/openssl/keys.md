---
title: "Keys"
slug: "keys"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

## Syntax
- EVP_PKEY *EVP_PKEY_new(void);
- RSA * RSA_new(void);
- int RSA_generate_key_ex(RSA *rsa, int bits, BIGNUM *e, BN_GENCB *cb);
- int EVP_PKEY_assign_RSA(EVP_PKEY *pkey, RSA *key);
- int PEM_write_PrivateKey(FILE *fp, EVP_PKEY *x, const EVP_CIPHER *enc, unsigned char *kstr, int klen, pem_password_cb *cb, void *u);
- int PEM_write_bio_PrivateKey(BIO *bp, EVP_PKEY *x, const EVP_CIPHER *enc, unsigned char *kstr, int klen, pem_password_cb *cb, void *u);
- EVP_PKEY *PEM_read_PrivateKey(FILE *fp, EVP_PKEY **x, pem_password_cb *cb, void *u);
- EVP_PKEY *PEM_read_bio_PrivateKey(BIO *bp, EVP_PKEY **x, pem_password_cb *cb, void *u);
- void EVP_PKEY_free(EVP_PKEY *key);

## Generate RSA Key
In order to generate an RSA key, an `EVP_PKEY` must first be allocated with [`EVP_PKEY_new`](http://www.openssl.org/docs/crypto/EVP_PKEY_new.html):

    EVP_PKEY *pkey;
    pkey = EVP_PKEY_new();

An exponent for the key is also needed, which will require allocating a `BIGNUM` with [`BN_new`](https://www.openssl.org/docs/manmaster/crypto/BN_new.html) and then assigning with [`BN_set_word`](https://www.openssl.org/docs/manmaster/crypto/BN_zero.html):

    BIGNUM *bn;
    bn = BN_new();
    BN_set_word(bn, RSA_F4);

To generate the key, create a new `RSA` with [`RSA_new`](https://www.openssl.org/docs/manmaster/crypto/RSA_new.html) and call [`RSA_generate_key_ex`](http://www.openssl.org/docs/crypto/RSA_generate_key.html):

    RSA *rsa;
    rsa = RSA_new();
    RSA_generate_key_ex(
        rsa,  /* pointer to the RSA structure */
        2048, /* number of bits for the key - 2048 is a good value */
        bn,   /* exponent allocated earlier */
        NULL, /* callback - can be NULL if progress isn't needed */
    );

To assign the newly generated key to the `EVP_PKEY` structure, call [`EVP_PKEY_assign_RSA`](https://www.openssl.org/docs/manmaster/crypto/EVP_PKEY_set1_RSA.html):

    EVP_PKEY_assign_RSA(pkey, rsa);

The `RSA` structure will be automatically freed when the `EVP_PKEY` structure is freed. This is done with [`EVP_PKEY_free`](http://www.openssl.org/docs/crypto/EVP_PKEY_new.html):

    EVP_PKEY_free(pkey);

## Load Private Key
To load a private key directly from disk, use the [`PEM_read_PrivateKey`](https://www.openssl.org/docs/manmaster/crypto/PEM_read_X509.html) function:

    FILE *f;
    EVP_PKEY *pkey;
    f = fopen("key.pem", "rb");
    PEM_read_PrivateKey(
        f,     /* use the FILE* that was opened */
        &pkey, /* pointer to EVP_PKEY structure */
        NULL,  /* password callback - can be NULL */
        NULL   /* parameter passed to callback or password if callback is NULL */
    );

To load a private key from a `BIO`, use [`PEM_read_bio_PrivateKey`](https://www.openssl.org/docs/manmaster/crypto/PEM_read_X509.html):

    BIO *bio;
    bio = BIO_new_mem_buf((void *)input, input_len);
    PEM_read_bio_PrivateKey(
        bio,   /* BIO to read the private key from */
        &pkey, /* pointer to EVP_PKEY structure */
        NULL,  /* password callback - can be NULL */
        NULL   /* parameter passed to callback or password if callback is NULL */
    );

## Save Private Key
An `EVP_PKEY` can be saved directly to disk in a several formats. [`PEM_write_PrivateKey`](https://www.openssl.org/docs/manmaster/crypto/PEM_read_X509.html) is used to save `EVP_PKEY` in a PEM format:

    FILE *f;
    f = fopen("key.pem", "wb");
    PEM_write_PrivateKey(
        f,                  /* use the FILE* that was opened */
        pkey,               /* EVP_PKEY structure */
        EVP_des_ede3_cbc(), /* default cipher for encrypting the key on disk */
        "replace_me",       /* passphrase required for decrypting the key on disk */
        10,                 /* length of the passphrase string */
        NULL,               /* callback for requesting a password */
        NULL                /* data to pass to the callback */
    );

To save a private key to a `BIO`, use [`PEM_write_bio_PrivateKey`](https://www.openssl.org/docs/manmaster/crypto/PEM_read_X509.html):

    BIO *bio;
    bio = BIO_new(BIO_s_mem());
    PEM_write_bio_PrivateKey(
        bio,                /* BIO to write the private key to */
        pkey,               /* EVP_PKEY structure */
        EVP_des_ede3_cbc(), /* default cipher for encrypting the key on disk */
        "replace_me",       /* passphrase required for decrypting the key on disk */
        10,                 /* length of the passphrase string */
        NULL,               /* callback for requesting a password */
        NULL                /* data to pass to the callback */
    );

