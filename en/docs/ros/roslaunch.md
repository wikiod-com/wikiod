---
title: "roslaunch"
slug: "roslaunch"
draft: false
images: []
weight: 9899
type: docs
toc: true
---

'node form the package' should be 'node from the package'

initially your say "starting" and "Stopping", but you don't explain how to do a controlled shutdown.

## launch ros nodes and load parameters from Yaml file
**roslaunch** is an important tool that manages the start and stop of ROS nodes. It takes one or more "*.launch" files as arguments. 

For this example, I will refer to the following (as asked in this [question][1]), so how can we execute those commands consecutively & automatically : 

    roscd stereo_camera
    rosparam load marvin_cameras.yaml
    rosrun stereo_camera stereo_camera __name:=bumblebeeLeft
    rosrun stereo_camera stereo_camera __name:=bumblebeeCenter
    
    roslaunch openni_launch_marvin kinect_left.launch
    roslaunch openni_launch_marvin kinect_center.launch 

**First of all**, let's `break up these commands in pieces`. As it can be seen, 4 ros commands are needed : *roscd*, *rosparam*, *rosrun* and *roslaunch*. Now let's start ! 

This is an example of why roslaunch is powerful, In fact, all those commands could be included in `one and only roslaunch file in ROS`. then all what we have to do is to launch this "solution.launch" file for a consecutive and automatic call for those commands.

To start, launch files are based on XML formatting, here's a basic launch file in ROS, we will name it "basic_example.launch" and it's included in a ROS package named "roslaunch_example": 

    <launch>
        
    </launch>

the command to execute this launch file is 

    $ roslaunch roslaunch_example basic_example.launch

following the specification :

    $ roslaunch package_name launch_file_name.launch

Because of our launch file doesn't include any commands, nothing will be executed, more on that later...

**Including nodes in Launch files :**  
any ROS node in any ROS package installed is call-able in launch files. to that we have to specify the package containing the node and it's name as specified in the package. 
for example : 

    rosrun stereo_camera stereo_camera __name:=bumblebeeLeft
    rosrun stereo_camera stereo_camera __name:=bumblebeeCenter


`stereo_camera` is a node form the package `stereo_camera` and the arguments specified are it's name `__name:=bumblebeeLeft` and `__name:=bumblebeeCenter`.

To add those nodes, we have to add the following lines: 

    <launch>    
        <node name="$(arg name)" pkg="stereo_camera" type="stereo_camera" output="screen">
              <param name="name" value="bumblebeeLeft" />
        </node>

        <node name="$(arg name)" pkg="stereo_camera" type="stereo_camera" output="screen">
              <param name="name" value="bumblebeeCenter" />
        </node>
    </launch>


Executing this launch file, we'll have the two nodes running. 

> adding a parameter to a ROS node :


As can be seen, we added a parameter "name" for the nodes as : 

    <param name="name" value="bumblebeeCenter" />

In fact, we can add as much parameters as we want (as created above) and then refer to them by calling 
`"$(arg parameter_name)"` instead of fixing it's value.

> Specifying the output :

The output tag can be set to "screen", if you need to see the node log `on the terminal` or "log" to save the log to the log files in `(~/.ros)`.

**Including other ROS launch files in a ROS launch file :** 

As stated [here][2], The <include> tag enables you to import another roslaunch XML file into the current file. It will be imported within the current scope of your document, including <group> and <remap> tags. All content in the include file will be imported except for the <master> tag: the <master> tag is only obeyed in the top-level file.

So, all to executes these commands 

    roslaunch openni_launch_marvin kinect_left.launch
    roslaunch openni_launch_marvin kinect_center.launch 

from a launch file, all we have to do is adding the following lines : 

    <include file="$(find openni_launch_marvin)/launch/kinect_left.launch" />
    <include file="$(find openni_launch_marvin)/launch/kinect_center.launch" />

> roscd to a package in ROS launch file 

In order to find the launch file than we want to include, we don't need to specify the full path. Instead, roslaunch provides the  `"$(find package_name)"` directive, this way, we can refer to our launch file `relative to the package racine`.  
In the above example, I assumed that the file "kinect_center.launch" is in the "openni_launch_marvin)/launch/" folder. 

**Loading parameters from YAML File :** 

In order to load parameters from a YAML file in ROS, ROS provides the "rosparam" tag.
As stated in the [wiki][3] : "The <rosparam> tag enables the use of rosparam YAML files for loading and dumping parameters from the ROS Parameter Server. It can also be used to remove parameters. The <rosparam> tag can be put inside of a <node> tag, in which case the parameter is treated like a private name."

Using this tag, we can load our YAML file in the launch file by adding this line :   

    <rosparam command="load" file="$(find marvin_cameras)/config/marvin_cameras.yaml" />

As used above, I assumed that the YAML file "marvin_cameras.yaml" is in the "marvin_cameras/config/" folder.

**Assembling all pieces**

Now that we have created separately our launch file contents, let's assemble them in one big launch file "solution.launch".

*solution.launch* 

    <launch> 

        <rosparam command="load" file="$(find marvin_cameras)/config/marvin_cameras.yaml" />  
 
        <node name="$(arg name)" pkg="stereo_camera" type="stereo_camera" output="screen">
              <param name="name" value="bumblebeeLeft" />
        </node>

        <node name="$(arg name)" pkg="stereo_camera" type="stereo_camera" output="screen">
              <param name="name" value="bumblebeeCenter" />
        </node>

        <include file="$(find openni_launch_marvin)/launch/kinect_left.launch" />
        <include file="$(find openni_launch_marvin)/launch/kinect_center.launch" />

    </launch>

Now, we have our one and only roslaunch file for executing all the commands  consecutively and automatically.


  [1]: http://stackoverflow.com/questions/17483893/ros-execute-multiple-commands-from-file?rq=1
  [2]: http://wiki.ros.org/roslaunch/XML/include
  [3]: http://wiki.ros.org/roslaunch/XML/rosparam

