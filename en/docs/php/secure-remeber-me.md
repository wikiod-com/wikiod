---
title: "Secure Remeber Me"
slug: "secure-remeber-me"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

I have been searching on this topic for sometime till i found this post https://stackoverflow.com/a/17266448/4535386 from ircmaxell, I think it deserves more exposure.

## “Keep Me Logged In” - the best approach
store the cookie with three parts. 

    function onLogin($user) {
        $token = GenerateRandomToken(); // generate a token, should be 128 - 256 bit
        storeTokenForUser($user, $token);
        $cookie = $user . ':' . $token;
        $mac = hash_hmac('sha256', $cookie, SECRET_KEY);
        $cookie .= ':' . $mac;
        setcookie('rememberme', $cookie);
    }

Then, to validate:

    function rememberMe() {
        $cookie = isset($_COOKIE['rememberme']) ? $_COOKIE['rememberme'] : '';
        if ($cookie) {
            list ($user, $token, $mac) = explode(':', $cookie);
            if (!hash_equals(hash_hmac('sha256', $user . ':' . $token, SECRET_KEY), $mac)) {
                return false;
            }
            $usertoken = fetchTokenByUserName($user);
            if (hash_equals($usertoken, $token)) {
                logUserIn($user);
            }
        }
    }

