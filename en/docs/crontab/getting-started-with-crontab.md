---
title: "Getting started with crontab"
slug: "getting-started-with-crontab"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Creating a new cron on ubuntu (or most other linux os)
You can create a new cron entry by simply typing 

    crontab -e

on the command line. If it's the first time you want to edit your crontab(le), you will be prompted with an editor selection:

    no crontab for <user> - using an empty one
    
    Select an editor.  To change later, run 'select-editor'.
      1. /bin/ed
      2. /bin/nano        <---- easiest
      3. /usr/bin/vim.basic
      4. /usr/bin/vim.tiny
    
    Choose 1-4 [2]: 

Simply choose your editor by following the prompt, and your crontab will open with  an empty file (only containing some commented lines of explanation):

    # Edit this file to introduce tasks to be run by cron.
    #
    # Each task to run has to be defined through a single line
    # indicating with different fields when the task will be run
    # and what command to run for the task
    #
    # To define the time you can provide concrete values for
    # minute (m), hour (h), day of month (dom), month (mon),
    # and day of week (dow) or use '*' in these fields (for 'any').#
    # Notice that tasks will be started based on the cron's system
    # daemon's notion of time and timezones.
    #
    # Output of the crontab jobs (including errors) is sent through
    # email to the user the crontab file belongs to (unless redirected).
    #
    # For example, you can run a backup of all your user accounts
    # at 5 a.m every week with:
    # 0 5 * * 1 tar -zcf /var/backups/home.tgz /home/
    #
    # For more information see the manual pages of crontab(5) and cron(8)
    #
    # m h  dom mon dow   command

As you can see, there's already an example entry in the text:

    0 5 * * 1 tar -zcf /var/backups/home.tgz /home/

This would create a backup file called `home.tgz` inside `/var/backups/`. The timing for this crontab would be 

    every monday (first day of week) at 5:00 A.M.


If you had entered that line as your crontab, all you had to do now would be to save the crontabfile. For example with the `nano` editor, this is done with `<Ctrl> + <X>` - then confirm to save with `Y`.

To check your crontab, simply type 

    crontab -l 

in the console.


Some more information on crontimings you can choose:

     # * * * * *  command to execute
     # │ │ │ │ │
     # │ │ │ │ │
     # │ │ │ │ └───── day of week (0 - 6) (0 to 6 are Sunday to Saturday, or use names; 7 is Sunday, the same as 0)
     # │ │ │ └────────── month (1 - 12)
     # │ │ └─────────────── day of month (1 - 31)
     # │ └──────────────────── hour (0 - 23)
     # └───────────────────────── min (0 - 59)

Special characters in cronjobs are:

**Asterisk ( `*` )**

The asterisk indicates that the cron expression matches for all values of the field. E.g., using an asterisk in the 4th field (month) indicates every month.


**Slash ( `/` )**

Slashes describe increments of ranges. For example 3-59/15 in the 1st field (minutes) indicate the third minute of the hour and every 15 minutes thereafter. The form "*/..." is equivalent to the form "first-last/...", that is, an increment over the largest possible range of the field.

**Comma ( `,` )**

Commas are used to separate items of a list. For example, using "MON,WED,FRI" in the 5th field (day of week) means Mondays, Wednesdays and Fridays.

**Hyphen ( `-` )**

Hyphens define ranges. For example, 2000-2010 indicates every year between 2000 and 2010 AD, inclusive.

**Percent ( `%` )**

Percent-signs (%) in the command, unless escaped with backslash (), are changed into newline characters, and all data after the first % are sent to the command as standard input.




## Install crontab on Linux
 

   **Debian/Ubuntu** 

    # apt-get update & apt-get -y upgrade
    # apt-get install cron
    
   **Fedora/CentOS**

    # yum -y update
    # yum install vixie-cron
    
   **Arch**

    # pacman --noconfirm -Syu
    # pacman -S cronie

