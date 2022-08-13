---
title: "Password Hashing Functions"
slug: "password-hashing-functions"
draft: false
images: []
weight: 9499
type: docs
toc: true
---

As more secure web services avoid storing passwords in plain text format, languages such as PHP provide various (undecryptable) hash functions to support the more secure industry standard. This topic provides documentation for proper hashing with PHP.

## Syntax
- `string password_hash ( string $password , integer $algo [, array $options ] )`
- `boolean password_verify ( string $password , string $hash )`
- `boolean password_needs_rehash ( string $hash , integer $algo [, array $options ] )`
- `array password_get_info ( string $hash )`

Prior to PHP 5.5, you may use [the compatibility pack](https://github.com/ircmaxell/password_compat) to provide the `password_*` functions.  It is highly recommended that you use the compatibility pack if you are able to do so.

With or without the compatibility pack, [correct Bcrypt functionality through `crypt()` relies on PHP 5.3.7+](http://php.net/security/crypt_blowfish.php) otherwise you *must* restrict passwords to ASCII-only character sets.

> **Note:** If you use PHP 5.5 or below you're using an [unsupported version of PHP](http://php.net/supported-versions.php) which does not receive any security updates anymore. Update as soon as possible, you can update your password hashes afterwards.

## Algorithm Selection

### Secure algorithms

* **bcrypt** is your best option as long as you use key stretching to increase hash calculation time, since it makes [brute force attacks extremely slow](http://arstechnica.com/security/2015/08/cracking-all-hacked-ashley-madison-passwords-could-take-a-lifetime/).
* **argon2** is another option which [will be available in PHP 7.2](https://wiki.php.net/rfc/argon2_password_hash).

### Insecure algorithms

The following hashing algorithms are **insecure or unfit for purpose** and therefore **should not be used**. They were never suited for password hashing, as they're designed for fast digests instead of slow and hard to brute force password hashes.

**If you use any of them**, even including salts, you should **switch** to one of the recommended secure algorithms **as soon as possible**.

Algorithms considered insecure:
* **MD4** - [collision attack found in 1995](http://link.springer.com/article/10.1007%2Fs001459900047)
* **MD5** - [collision attack found in 2005](https://en.wikipedia.org/wiki/MD5#Collision_vulnerabilities)
* **SHA-1** - [collision attack demonstrated in 2015](https://en.wikipedia.org/wiki/SHA-1#Attacks)

Some algorithms can be safely used as message digest algorithm to prove authenticity, but **never as password hashing algorithm**:
* **SHA-2**
* **SHA-3**

Note, strong hashes such as SHA256 and SHA512 are unbroken and robust, however it is generally more secure to use **bcrypt** or **argon2** hash functions as brute force attacks against these algorithms are much more difficult for classical computers.

## Creating a password hash
Create password hashes using [`password_hash()`][1] to use the current industry best-practice standard hash or key derivation. At time of writing, the standard is [bcrypt][2], which means, that `PASSWORD_DEFAULT` contains the same value as `PASSWORD_BCRYPT`.

    $options = [
        'cost' => 12,
    ];
    
    $hashedPassword = password_hash($plaintextPassword, PASSWORD_DEFAULT, $options);

The third parameter is **not mandatory**.

The `'cost'` value should be chosen based on your production server's hardware. Increasing it will make the password more costly to generate. The costlier it is to generate the longer it will take anyone trying to crack it to generate it also. The cost should ideally be as high as possible, but in practice it should be set so it does not slow down everything too much. Somewhere between 0.1 and 0.4 seconds would be okay. Use the default value if you are in doubt.

<!-- if version [lt 5.5] -->    
On PHP lower than 5.5.0 the `password_*` functions are not available. You should use [the compatibility pack][3] to substitute those functions. Notice the compatibility pack requires PHP 5.3.7 or higher or a version that has the `$2y` fix backported into it (such as RedHat provides).

If you are not able to use those, you can implement password hashing with [`crypt()`][4] As `password_hash()` is implemented as a wrapper around the `crypt()` function, you need not lose any functionality.

    // this is a simple implementation of a bcrypt hash otherwise compatible
    // with `password_hash()`
    // not guaranteed to maintain the same cryptographic strength of the full `password_hash()`
    // implementation

    // if `CRYPT_BLOWFISH` is 1, that means bcrypt (which uses blowfish) is available
    // on your system
    if (CRYPT_BLOWFISH == 1) {
        $salt = mcrypt_create_iv(16, MCRYPT_DEV_URANDOM);
        $salt = base64_encode($salt);
        // crypt uses a modified base64 variant
        $source = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
        $dest = './ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
        $salt = strtr(rtrim($salt, '='), $source, $dest);
        $salt = substr($salt, 0, 22);
        // `crypt()` determines which hashing algorithm to use by the form of the salt string
        // that is passed in
        $hashedPassword = crypt($plaintextPassword, '$2y$10$'.$salt.'$');
    }

<!-- end version if -->

## Salt for password hash

Despite of reliability of crypt algorithm there is still vulnerability against [rainbow tables][5]. That's the reason, why it's recommended to use **salt**. 

A salt is something that is appended to the password before hashing to make source string unique. Given two identical passwords, the resulting hashes will be also unique, because their salts are unique. 

A random salt is one of the most important pieces of your password security. This means that even with a lookup table of known password hashes an attacker can’t match up your user’s password hash with the database password hashes since a random salt has been used. You should use always random and cryptographically secure salts. [Read more][6]

With [`password_hash()`][1] `bcrypt` algorithm, plain text salt is stored along with the resulting hash, which means that the hash can be transferred across different systems and platforms and still be matched against the original password.

<!-- if version [lt 7.0] -->  
Even when this is discouraged, you can use the `salt` option to define your own random salt.
 

     $options = [
            'salt' => $salt, //see example below
     ];
**Important**. If you omit this option, a random salt will be generated by password_hash() for each password hashed. This is the intended mode of operation.

<!-- end version if -->
<!-- if version [ gte 7.0] -->  
 The salt option has been [deprecated][7] as of PHP 7.0.0. It is now preferred to simply use the salt that is generated by default.  

<!-- end version if -->


  [1]: http://php.net/manual/en/function.password-hash.php
  [2]: https://en.wikipedia.org/wiki/Bcrypt
  [3]: https://github.com/ircmaxell/password_compat
  [4]: http://php.net/manual/en/function.crypt.php
  [5]: https://en.wikipedia.org/wiki/Rainbow_table
  [6]: http://www.springer.com/us/book/9781484221198
  [7]: http://php.net/manual/ru/function.password-hash.php

## Determine if an existing password hash can be upgraded to a stronger algorithm
If you are using the `PASSWORD_DEFAULT` method to let the system choose the best algorithm to hash your passwords with, as the default increases in strength you may wish to rehash old passwords as users log in

    <?php
    // first determine if a supplied password is valid
    if (password_verify($plaintextPassword, $hashedPassword)) {

        // now determine if the existing hash was created with an algorithm that is
        // no longer the default
        if (password_needs_rehash($hashedPassword, PASSWORD_DEFAULT)) {

            // create a new hash with the new default
            $newHashedPassword = password_hash($plaintextPassword, PASSWORD_DEFAULT);

            // and then save it to your data store
            //$db->update(...);
        }
    }
    ?>

If the password_* functions are not available on your system (and you cannot use the compatibility pack linked in the remarks below), you can determine the algorithm and used to create the original hash in a method similar to the following:

    <?php
    if (substr($hashedPassword, 0, 4) == '$2y$' && strlen($hashedPassword) == 60) {
        echo 'Algorithm is Bcrypt';
        // the "cost" determines how strong this version of Bcrypt is
        preg_match('/\$2y\$(\d+)\$/', $hashedPassword, $matches);
        $cost = $matches[1];
        echo 'Bcrypt cost is '.$cost;
    }
    ?>

## Verifying a password against a hash
`password_verify()` is the built-in function provided (as of PHP 5.5) to verify the validity of a password against a known hash.

    <?php
    if (password_verify($plaintextPassword, $hashedPassword)) {
        echo 'Valid Password';
    }
    else {
        echo 'Invalid Password.';
    }
    ?>

All supported hashing algorithms store information identifying which hash was used in the hash itself, so there is no need to indicate which algorithm you are using to encode the plaintext password with.

If the password_* functions are not available on your system (and you cannot use the compatibility pack linked in the remarks below) you can implement password verification with the `crypt()` function.  Please note that specific precautions must be taken to avoid [timing attacks](https://en.wikipedia.org/wiki/Timing_attack).

    <?php
    // not guaranteed to maintain the same cryptographic strength of the full `password_hash()`
    // implementation
    if (CRYPT_BLOWFISH == 1) {
        // `crypt()` discards all characters beyond the salt length, so we can pass in
        // the full hashed password
        $hashedCheck = crypt($plaintextPassword, $hashedPassword);

        // this a basic constant-time comparison based on the full implementation used
        // in `password_hash()`
        $status = 0;
        for ($i=0; $i<strlen($hashedCheck); $i++) {
            $status |= (ord($hashedCheck[$i]) ^ ord($hashedPassword[$i]));
        }

        if ($status === 0) {
            echo 'Valid Password';
        }
        else {
            echo 'Invalid Password';
        }
    }
    ?>

