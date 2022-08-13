---
title: "Signals"
slug: "signals"
draft: false
images: []
weight: 9928
type: docs
toc: true
---

Flask supports signals using [Blinker][1].  Signal support is optional; they will only be enabled if Blinker is installed.

    pip install blinker

http://flask.pocoo.org/docs/dev/signals/

---

Signals are not asynchronous.  When a signal is sent, it immediately executes each of the connected functions sequentially.


  [1]: https://pythonhosted.org/blinker/

## Connecting to signals
Use a signal's `connect` method to connect a function to a signal.  When a signal is sent, each connected function is called with the sender and any named arguments the signal provides.

    from flask import template_rendered

    def log_template(sender, template, context, **kwargs):
        sender.logger.info(
            'Rendered template %(template)r with context %(context)r.',
            template=template, context=context
        )

    template_rendered.connect(log_template)

See the documentation on [built-in signals][1] for information about what arguments they provides.  A useful pattern is adding a `**kwargs` argument to catch any unexpected arguments.


  [1]: http://flask.pocoo.org/docs/dev/api/#core-signals-list

## Custom signals
If you want to [create and send signals][1] in your own code (for example, if you are writing an extension), create a new `Signal` instance and call [`send`][2] when the subscribers should be notified.  Signals are created using a [`Namespace`][3].

    from flask import current_app
    from flask.signals import Namespace

    namespace = Namespace()
    message_sent = namespace.signal('mail_sent')

    def message_response(recipient, body):
        ...
        message_sent.send(
            current_app._get_current_object(),
            recipient=recipient,
            body=body
        )

    @message_sent.connect
    def log_message(app, recipient, body):
        ...

---

Prefer using Flask's signal support over using Blinker directly.  It wraps the library so that signals remain optional if developers using your extension have not opted to install Blinker.


  [1]: http://flask.pocoo.org/docs/dev/signals/#creating-signals
  [2]: https://pythonhosted.org/blinker/index.html#blinker.base.Signal.send
  [3]: http://flask.pocoo.org/docs/dev/api/#flask.signals.Namespace

