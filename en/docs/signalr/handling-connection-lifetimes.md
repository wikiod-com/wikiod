---
title: "Handling Connection Lifetimes"
slug: "handling-connection-lifetimes"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Parameters
| Parameter| Details |
| ------ | ------ |
| stopCalled| This value tells you how a user disconnected, if its set to true then the user explicitly closed the connection, otherwise they timed out.|

It's worth noting that at while in these functions you still have access to the Context, therefore you can get the connectionId and identify who/what has disconnected.

Remember, a user can have **more then one connectionId **, so think about how you want to store all the connection IDs for one user

## On connected
When ever a user connects to your hub, the `OnConnected()` is called. You can over ride this function and implement your own logic if you need to keep track of or limit the number of connections

      public override Task OnConnected()
        {
            //you logic here

            return base.OnConnected();
        }


## On Disconnected
Overloading the disconnect function allows you to handle what to do when a user disconnects. 


    public override Task OnDisconnected(bool stopCalled)
    {       
        //Your disconnect logic here

        return base.OnDisconnected(stopCalled);
    }

## Example of informing others in a group  that a user has disconnected
        /// <summary>
        /// Overrides the onDisconnected function and sets the user object to offline, this can be checked when other players interacts with them...
        /// </summary>
        /// <param name="stopCalled"></param>
        /// <returns></returns>
        public override Task OnDisconnected(bool stopCalled)
        {
            var user = GetUserDictionaryData();
            if (user != null)
            {
                user.Status = PlayerStatus.offline;
                var playerJson = ReturnObjectAsJSON(user.Player);
                Clients.OthersInGroup(user.GroupId).userDisconnected(playerJson);
            }

            return base.OnDisconnected(stopCalled);
        }

## No need to worry about connection in signalr.
If you want to notify other clients/users throughout the application ,you not need to worry about the connection because signalr new connection is created every time you visit other pages in the web app.

we can leverage  the users feature of signalr to achieve the same. see the example below:

Here we are creating based on the projectid and all others users of that group get notified when we access the $.connection.notificationHub.server.NotifyOthersInGroup(projectid,projectname); from client javascript file

//Hub Example of SignalR
 
public class NotificationHub : Hub
    {

        public static readonly ConcurrentDictionary<string, HashSet<string>>         
           ProjectLockUsers = new ConcurrentDictionary<string,HashSet<string>>();

        public void JoinGroup(int projectid)
        {
            string groupname = string.Format("ProjectLock_{0}", projectid);
            Groups.Add(Context.ConnectionId, groupname);

            AddUserToProjectLockGroup(groupname, Context.User.Identity.Name);
        }

        public void NotifyOthersInGroup(int projectid,string name)
        {
            string groupname = string.Format("ProjectLock_{0}", projectid);
            
            var allusers=null as HashSet<string>;

            if (ProjectLockUsers.TryGetValue(groupname, out allusers))
            {
                allusers.Remove(Context.User.Identity.Name);
                Clients.Users(allusers.ToList()).notifyUnlock(name);
            }
        }

        public override Task OnConnected()
        {
            return base.OnConnected();
        }
        public override Task OnDisconnected(bool stopCalled)
        {
            return base.OnDisconnected(stopCalled);
        }
        

        private void AddUserToProjectLockGroup(string projectId,string userId)
        {
            var userIds = null as HashSet<string>;            
            

            if (Sessions.TryGetValue(projectId, out userIds) == false)
            {
                userIds = Sessions[projectId] = new HashSet<string>();
            }

            userIds.Add(userId);

            ProjectLockUsers[projectId] = userIds;
        }        
       
    }


