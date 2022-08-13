---
title: "Twitch Chat (IRC) Bot"
slug: "twitch-chat-irc-bot"
draft: false
images: []
weight: 9932
type: docs
toc: true
---

Twitch Chat is a simple IRC chat. For any serious development, there are multiple documents for it, including the most comprehensive and general ressource: http://ircdocs.horse/

# Connection, Handshake

IRC is a basic, plaintext based TCP protocol. Connecting to Twitch works just like any regular IRC service with a difference in authenticating:

Connection Initiation > Handshake > Usage

The handshake is regularly the hardest part to get right:

After building up the connection to the server, you are required to provide ``PASS`` and **then** a ``NICK``, where ``PASS`` is an OAuth-Token (which you can generate [here][1]) and ``USER`` being the username to this OAuth token.

The handshake is then as following (``<`` being sent from client to server, ``>`` being sent from server to client):

    < PASS oauth:your_oauth_token
    < NICK your_username
    > :tmi.twitch.tv 001 your_username :connected to TMI
    > :tmi.twitch.tv 002 your_username :your host is TMI
    > :tmi.twitch.tv 003 your_username :this server is pretty new
    > :tmi.twitch.tv 004 your_username tmi.twitch.tv 0.0.1 w n
    > :tmi.twitch.tv 375 your_username :- tmi.twitch.tv Message of the day - 
    > :tmi.twitch.tv 372 your_username :- not much to say here
    > :tmi.twitch.tv 376 your_username :End of /MOTD command

Once you received either any of these ``MODE``, [``376``][2] or [``422``][3], you're good to go and can send the twitch server any commands, like:

    > JOIN :#gamesdonequick
    > PRIVMSG #gamesdonequick :Hello world!

A more throughout guide to client-server commands can be found [here][4].

# Twitch-specific Capabilities

While Twitch uses a standard IRC service, there are some events seen on the IRC service which correlate to activity in a channel on the Twitch website.  Examples here are slowmode being enabled or disabled, subscriber-only mode being enabled/disabled on a streamer's chat, hosting activity, and bits/cheer activity, among others.  

Details on these Twitch-specific capabilities are listed in the GitHub documentation for Twitch IRC, which can be found [here][5].



  [1]: https://www.twitch.tv/kraken/oauth2/clients/new
  [2]: http://defs.ircdocs.horse/defs/numerics.html#err-nomotd-422
  [3]: http://defs.ircdocs.horse/defs/numerics.html#rpl-endofmotd-376
  [4]: https://en.wikipedia.org/wiki/List_of_Internet_Relay_Chat_commands
  [5]: https://github.com/justintv/Twitch-API/blob/master/IRC.md#twitch-capabilities

## Python
Here is a simple Python command-line program which will connect to a Twitch channel as a bot and respond to a few simple commands.

Dependencies:

- [`irc` Python lib](https://pypi.python.org/pypi/irc) (`pip install irc` or `easy_install irc`)

Source: https://gist.github.com/jessewebb/65b554b5be784dd7c8d1

<!-- language: python -->
```
import logging
import sys

from irc.bot import SingleServerIRCBot


# config
HOST = 'irc.twitch.tv'
PORT = 6667
USERNAME = 'nickname'
PASSWORD = 'oauth:twitch_token'  # http://www.twitchapps.com/tmi/
CHANNEL = '#channel'


def _get_logger():
    logger_name = 'vbot'
    logger_level = logging.DEBUG
    log_line_format = '%(asctime)s | %(name)s - %(levelname)s : %(message)s'
    log_line_date_format = '%Y-%m-%dT%H:%M:%SZ'
    logger_ = logging.getLogger(logger_name)
    logger_.setLevel(logger_level)
    logging_handler = logging.StreamHandler(stream=sys.stdout)
    logging_handler.setLevel(logger_level)
    logging_formatter = logging.Formatter(log_line_format, datefmt=log_line_date_format)
    logging_handler.setFormatter(logging_formatter)
    logger_.addHandler(logging_handler)
    return logger_

logger = _get_logger()


class VBot(SingleServerIRCBot):
    VERSION = '1.0.0'

    def __init__(self, host, port, nickname, password, channel):
        logger.debug('VBot.__init__ (VERSION = %r)', self.VERSION)
        SingleServerIRCBot.__init__(self, [(host, port, password)], nickname, nickname)
        self.channel = channel
        self.viewers = []

    def on_welcome(self, connection, event):
        logger.debug('VBot.on_welcome')
        connection.join(self.channel)
        connection.privmsg(event.target, 'Hello world!')

    def on_join(self, connection, event):
        logger.debug('VBot.on_join')
        nickname = self._parse_nickname_from_twitch_user_id(event.source)
        self.viewers.append(nickname)

        if nickname.lower() == connection.get_nickname().lower():
            connection.privmsg(event.target, 'Hello world!')

    def on_part(self, connection, event):
        logger.debug('VBot.on_part')
        nickname = self._parse_nickname_from_twitch_user_id(event.source)
        self.viewers.remove(nickname)

    def on_pubmsg(self, connection, event):
        logger.debug('VBot.on_pubmsg')
        message = event.arguments[0]
        logger.debug('message = %r', message)
        # Respond to messages starting with !
        if message.startswith("!"):
            self.do_command(event, message[1:])

    def do_command(self, event, message):
        message_parts = message.split()
        command = message_parts[0]
        
        logger.debug('VBot.do_command (command = %r)', command)

        if command == "version":
            version_message = 'Version: %s' % self.VERSION
            self.connection.privmsg(event.target, version_message)
        if command == "count_viewers":
            num_viewers = len(self.viewers)
            num_viewers_message = 'Viewer count: %d' % num_viewers
            self.connection.privmsg(event.target, num_viewers_message)
        elif command == 'exit':
            self.die(msg="")
        else:
            logger.error('Unrecognized command: %r', command)

    @staticmethod
    def _parse_nickname_from_twitch_user_id(user_id):
        # nickname!username@nickname.tmi.twitch.tv
        return user_id.split('!', 1)[0]


def main():
    my_bot = VBot(HOST, PORT, USERNAME, PASSWORD, CHANNEL)
    my_bot.start()


if __name__ == '__main__':
    main()

