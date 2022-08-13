---
title: "Getting started with ros"
slug: "getting-started-with-ros"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation
**Depending on your target machine, you need to choose a supported ROS Version (or vice-versa).** Although ROS installation is well documented in the ROS wiki, It might be confusing to find them. So, here's a table of the ROS Version, target platforms & architecture and the links for the appropriate install guides : 

| ROS Version | Platform             | Arch  |Status            |Install Guide Link |
| :---------: | :------------------- | ----- | ---------------- | :----------------: |
| **Kinetic** | Ubuntu 16.04 (Xenial)| amd64 / i386 / armhf |Supported | [Kinetic-Xenial-guide][1]
|             | Ubuntu 15.10 (Wily)  | amd64 / i386 |Supported | [Kinetic-Wily-guide][1]
|             | Debian 8 (Jessie)    | amd64 / arm64 | Supported | [Kinetic-Jessie-guide][2]
|             | OS X (Homebrew)      | --  | Experimental | [Kinetic-Homebrew-guide][3]
|             | Gentoo               | --  | Experimental | [Kinetic-Gentoo-guide][4]
|             | OpenEmbedded/Yocto   | --  | Experimental | [Kinetic-Yocto-guide][5]


Work in progress...!


  [1]: http://wiki.ros.org/kinetic/Installation/Ubuntu
  [2]: http://wiki.ros.org/kinetic/Installation/Debian
  [3]: http://wiki.ros.org/kinetic/Installation/OSX/Homebrew/Source
  [4]: http://wiki.ros.org/kinetic/Installation/Gentoo
  [5]: http://wiki.ros.org/hydro/Installation/OpenEmbedded

## Hello World Publisher
Create a workspace

    mkdir -p ~/catkin_ws/src
    cd ~/catkin_ws/src
    catkin_init_workspace

Build your workspace

    cd ~/catkin_ws/
    catkin_make

Source your setup file

    source devel/setup.bash

Create a new package named hello_world with some basic dependencies

    catkin_create_pkg hello_world std_msgs rospy roscpp

Navigate to your src directory and create a new file called talker.cpp

    cd hello_world/src
    touch talker.cpp

Edit your new file and paste this code in to publish a "hello world" message

    #include "ros/ros.h"
    #include "std_msgs/String.h"
    
    #include <sstream>
    
    int main(int argc, char **argv)
    {
      ros::init(argc, argv, "talker");
    
      ros::NodeHandle n;
    
      ros::Publisher chatter_pub = n.advertise<std_msgs::String>("chatter", 1000);
    
      ros::Rate loop_rate(10);
    
      int count = 0;
      while (ros::ok())
      {
        std_msgs::String msg;
    
        std::stringstream ss;
        ss << "hello world " << count;
        msg.data = ss.str();
    
        ROS_INFO("%s", msg.data.c_str());
    
        chatter_pub.publish(msg);
    
        ros::spinOnce();
    
        loop_rate.sleep();
        ++count;
      }
    
      return 0;
    }

Return to the root of your package directory

    cd ..

Add/uncomment these lines to your CMakeLists.txt
    
    catkin_package(
     INCLUDE_DIRS include
     LIBRARIES hello_world
    #  CATKIN_DEPENDS roscpp rospy std_msgs
    #  DEPENDS system_lib
    )

    include_directories(include ${catkin_INCLUDE_DIRS})
    
    add_executable(talker src/talker.cpp)
    target_link_libraries(talker ${catkin_LIBRARIES})
    add_dependencies(talker hello_world_generate_messages_cpp)


Return to the root of your workspace

    cd ..

Build your new publisher

    catkin_make  

Source your setup file again so that you have the new package and publisher

    source devel/setup.bash

Start ROS

    roscore

Leave roscore running and in a new terminal tab/window, start your publisher

    rosrun hello_world talker

Leave the publisher running and in ANOTHER new terminal tab/window, echo the output

    rostopic echo /chatter

