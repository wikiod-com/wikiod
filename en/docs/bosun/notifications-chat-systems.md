---
title: "Notifications  Chat Systems"
slug: "notifications--chat-systems"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

In Bosun notifications are used for both new alert incidents and when an alert is acked/closed/etc. If you don't want the other events to trigger a notification add runOnActions = false to the notification definition.

See [Notification Overview][1] for more examples.


  [1]: https://www.wikiod.com/bosun/notifications-overview

## HipChat
[Bosun notifications][1] are assigned to alert definitions using warnNotification and critNotification and indicate where to send the rendered alert template when a new incident occur. The [${env.VARIABLENAME} syntax][2] can be used to load values from an Environmental Variable.

In order to post alerts to HipChat, start by creating an Integration named "Bosun".  The Integration will provide the URL necessary to post messages (including the token) as seen here:

[![HipChat screenshot][3]][3]

All that's left is to setup the template and notification:

    #Example template
    template hipchat.bandwidth {
        subject = `{"color":{{if lt (.Eval .Alert.Vars.dlspeed) (.Eval .Alert.Vars.dlcritval) }}"red"{{else}} {{if lt (.Eval .Alert.Vars.dlspeed) (.Eval .Alert.Vars.dlwarnval) }}"yellow"{{else}}"green"{{end}}{{end}},"message":"Server: {{.Group.host}}<br/>Metric: {{.Alert.Name}}<br/><br/>DL speed: {{.Eval .Alert.Vars.dlspeed | printf "%.2f" }}<br/>DL Warning threshold: {{.Alert.Vars.dlwarnval}}<br/>DL Critical threshold: {{.Alert.Vars.dlcritval}}<br/><br/>Notes: {{.Alert.Vars.notes}}<br/><br/>RunBook: <a href={{.Alert.Vars.runbook}} >wiki article</a>","notify":false,"message_format":"html"}`
    }
    
    #Example notification
    notification hipchat {
        #Create an Integration in HipChat to generate the POST URL
        #Example URL:  https://<YOURHIPCHATSERVER_FQDN>/v2/room/<ROOM_NUMBER>/<TOKEN>
        post = ${env.HIPCHAT_ROOM_ABC} 
        body = {{.}}
        contentType = application/json
    }


  [1]: https://bosun.org/configuration#notification
  [2]: https://www.wikiod.com/bosun/notifications-overview/1991/overview
  [3]: http://i.stack.imgur.com/LhJwf.png

## Slack Notifications
```
#Post to a slack.com chatroom via their Incoming Webhooks integration
notification slack{
    post = https://hooks.slack.com/services/abcdefg/abcdefg/abcdefghijklmnopqrstuvwxyz
    body = {"text": {{.|json}}}
}
#To customize the icon and user use:
#   body = {"text": {{.|json}}, "icon_emoji": ":hammer_and_wrench:", "username": "Bosun"}
```

