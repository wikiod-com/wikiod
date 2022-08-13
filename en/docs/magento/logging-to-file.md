---
title: "Logging to file"
slug: "logging-to-file"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
 - public static function log($message, $level = null, $file = '', $forceLog = false)

## Parameters
| Parameter | Details |
|-----------|---------|
| string $message |  The message that will be logged  |
| integer $level  | Log level |
| string $file | Path and name with extension of file that will be saved to `var/log/`. If NULL or not specified then `system.log` will be used. |
| bool $forceLog | If set to `TRUE` log will be written even though developer mode is off and logging is inactive. |

# The Logging is turned off by default unless developer mode is active.
## All exceptions are logged in `exceptions.log` no matter if logging is enabled in configuration.

Logging can be enabled by logging into Magento Admin and proceeding to:
- System > Configuration (top bar)
- Developer (left menu)
- Log Settings section
- Select Yes from `Enabled` dropdown list.
- Save Configuration in the right top corner. 

# Message variable type
Even though documentation defines that message should be a string, if an array is passed there's a code block in that method to take care of that with `print_r`:
```
if (is_array($message) || is_object($message)) {
    $message = print_r($message, true);
}
```


# Log level
If the level parameter is set to null then DEBUG level is taken. 

`$level  = is_null($level) ? Zend_Log::DEBUG : $level;`
The levels are declared in file: `lib\Zend\log.php`
```
const EMERG   = 0;  // Emergency: system is unusable
const ALERT   = 1;  // Alert: action must be taken immediately
const CRIT    = 2;  // Critical: critical conditions
const ERR     = 3;  // Error: error conditions
const WARN    = 4;  // Warning: warning conditions
const NOTICE  = 5;  // Notice: normal but significant condition
const INFO    = 6;  // Informational: informational messages
const DEBUG   = 7;  // Debug: debug messages
```

Constants in form of `Zend_Log::INFO` or integer number in range specified above can be passed as log level parameter.



## Custom log file
    Mage::log('My log entry', null, 'mylogfile.log');

This wil log to

    /var/log/mylogfile.log

## Default logging
    Mage::log('My log entry');
    Mage::log('My log message: '.$myVariable);
    Mage::log($myArray);
    Mage::log($myObject);

This will log to `/var/log/system.log`

Objects and Arrays are automatically written via a `print_r()` directive. Watch out when using objects since these can get substantial in size.

    Mage::logException($e);
This will log exception trace string to `/var/log/exception.log`



