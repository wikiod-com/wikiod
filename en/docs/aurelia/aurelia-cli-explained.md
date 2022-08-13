---
title: "Aurelia CLI Explained"
slug: "aurelia-cli-explained"
draft: false
images: []
weight: 9932
type: docs
toc: true
---

## Setting Up Environment for Aurelia-cli Explained
 - **OS:** Mac OS X 10.11 (Should work on Windows / Linux since we are using Vagrant)
    -  Vagrant 1.8.4 Installed
 - **Directory Structure on Host OS (Mac OS):**
   - /path/to/project
     - /provision
        - /packages
            - Note: If you use different vesions, be sure to update
                Variables at top of provision.sh script below.
            - atom.x86_64.rpm (Download: [Atom][1])
            - node-v6.4.0-linux-x64.tar.xz (Download: [Node][2])
     - /vagrant
        - Vagrantfile (File contents below)
        - provision.sh (File contents below)
 - **Starting up the Virtual Machine** ($ == Terminal prompt)
    - In Mac OS Terminal
    - `$cd /path/to/project/vagrant`
    - `$vagrant up`
        - Downloads CentOS 7 vagrant box, runs provision script
        - Launches VM window outside of your Mac OS terminal
    - When all done, log into VM using gui
        - User: vagrant
        - PW: vagrant
    - Launch X Windows:
        - `$startx` (Starts a Gnome UI)
 - **Setting up the VM**
    - Launch a Terminal window (Applications Drop Down Menu / Utilities)
    - Set up sudo to run "npm"
        - Get path to npm: `$which npm` (/opt/node-v6.4.0-linux-x64/bin)
        - Add to visudo "secure_path"
            - `$sudo visudo` (Requires basic knowledge of Vi/Vim)
            - Type "i" to go into Insert mode
            - Use arrow keys to navigate to "secure_path" line
            - Append ":/opt/node-v6.4.0-linux-x64/bin" to "secure_path"
            - Type "esc" to exit Insert mode
            - Type ":wq" to write changes and quit Vi
    - Update npm, fixes critical bugs in aurelia cli
        - `$npm install npm -g`
    - Install aurelia cli globally
        - `$npm install aurelia-cli -g`
  - **Starting/Stopping the VM**
    - In Mac OS Terminal
    - `$cd /path/to/project/vagrant` (If you're not already in the directory with the Vagrantfile)
    - `$vagrant halt` (Keeps all files, closes GUI, shuts down VM)
    - `$vagrant up` (Restart VM, login via GUI when it's ready)
    - `$vagrant destroy` (Will need to download everything again, your local files will be deleted. Use this to start from scratch.)

 Now you're ready to start building your website using Aurelia CLI!

>Vagrantfile
>
>         Vagrant.configure("2") do |config|
>             config.vm.box = "centos/7"
>             config.vm.hostname = "dev"
>         
>             config.vm.provider "virtualbox" do |vb|
>                 vb.gui = true
>                 vb.cpus = "4"
>                 vb.memory = "3092"
>             end
>         
>             # Networking
>             config.vm.network "private_network", ip: "192.168.0.3"
>             config.vm.network :forwarded_port, guest: 80, host: 80 # nginx
>             config.vm.network :forwarded_port, guest: 9000, host: 9000 # au run
>             config.vm.network :forwarded_port, guest: 3001, host:3001 # BrowserSynch
>         
>             # Shares
>             config.vm.synced_folder "../provision", "/home/vagrant/provision"
>         
>             # Provision
>             config.vm.provision "shell", path: "provision.sh"
>         end

> provision.sh
>
>     HOME=/home/vagrant
>     NODE=node-v6.4.0-linux-x64
>     EPEL=epel-release-latest-7.noarch.rpm
>     ATOM=atom.x86_64
>     
>     echo "************************************"
>     echo "Provisioning virtual machine..."
>     echo "************************************"
>     sudo cd $HOME
>     
>     echo "***********************"
>     echo "Updating yum..."
>     echo "***********************"
>     sudo yum clean all
>     sudo yum -y install deltarpm yum-utils
>     sudo yum -y update --exclude=kernel*
>     
>     echo "***********************"
>     echo "Updating yum, installing Dev Tools..."
>     echo "***********************"
>     sudo yum -y groupinstall "Base"
>     sudo yum -y groupinstall "GNOME Desktop"
>     sudo yum -y groupinstall "Development Tools"
>     
>     echo "***********************"
>     echo "Installing tools..."
>     echo "***********************"
>     sudo yum install -y git tar gcc vim unzip wget curl tree nginx
>     
>     if [ -d "/opt/$NODE" ]
>     then
>       echo "**********************************"
>       echo "Node already installed..."
>       echo "**********************************"
>     else
>       echo "**********************************"
>       echo "Installing Node and update npm..."
>       echo "**********************************"
>       sudo cp /home/vagrant/provision/packages/$NODE.tar.xz /opt
>       sudo tar -xpf /opt/$NODE.tar.xz -C /opt
>       sudo echo "export PATH=\"$PATH:/opt/$NODE/bin\"" >> $HOME/.bash_profile
>       sudo echo "export PATH=\"$PATH:/opt/$NODE/bin\"" >> /root/.bash_profile
>       sudo source $HOME/.bash_profile
>     fi
>     
>     echo "**********************************"
>     echo "Installing Atom..."
>     echo "**********************************"
>     sudo rpm -ivh /home/vagrant/provision/packages/$ATOM.rpm
>     
>     echo "******************************"
>     echo "Installing EPEL..."
>     echo "******************************"
>     wget -P /etc/yum.repos.d http://dl.fedoraproject.org/pub/epel/$EPEL
>     sudo rpm -ivh /etc/yum.repos.d/$EPEL
>     
>     echo "***********************"
>     echo "Git setup..."
>     echo "***********************"
>     git config --global user.email "your@email.com"
>     git config --global user.name "Your Name"
>     git config --global github.user "Username"
>     
>     echo "***********************"
>     echo "Don't forget to:"
>     echo "sudo visudo"
>     echo "Append npm path to secure_path: /opt/$NODE/bin"
>     echo "sudo npm install npm -g"
>     echo "sudo npm install aurelia-cli -g"
>     echo "***********************"
>     
>     
>     echo "*********************************"
>     echo "VM Provisioning Complete!"
>     echo "*********************************"



  [1]: http://atom.io
  [2]: http://nodejs.org/en/

## Aurelia CLI Basics Explained
This Example assumes you setup as explained in Example: **Setting Up Environment for aurelia-cli Explained** of this document.

**Creating a new project**
- In main host os, open terminal (Mac OS in my case)
- `$cd /path/to/project/vagrant`
- `$vagrant up` (Launches VM GUI)
- Log into VM via UI
    - User: vagrant / PW: vagrant
    - `$startx`
    - Windows X starts Gnome session
    - Open a terminal in VM
- `$cd /home/vagrant`
- `$au new` (starts a series of prompts to setup project)
    
    - **Prompt:** Please enter a name for your new project below.
    - `$[aurelia-app]> MyProject` (Press Enter) (This will be the name of project main directory)
    - **Prompt:** Would you like to use the default setup or customize your choices?
    - `$[Default ESNext] > 3` (Press Enter) (Custom options)
    - **Prompt:** What transpiler would you like to use?
    - `$[Babel] >` (Press Enter) (Lets use Bable to transpile our ESNext to ES5)
    - **Prompt:** What css processor would you like to use?
    - `$[none] > 3` (If you want to use Sass for your css)
    - **Prompt** Would you like to configure unit testing?
    - `$[Yes] > 2` (Lets not setup testing for simplicity) 
    - **Prompt** What is your default code editor?
    - `$[Visual Studio Code] > 2` (Atom, if you followed setup for this document)
    - **Outputs Setup Summary**
        - Project Configuration
        - **Name:** MyProject
        - **Platform:** Web
        - **Transpiler:** Babel
        - **CSS Processor:** Sass
        - **Unit Test Runner:** None
        - **Editor:** Atom
    - **Prompt** Would you like to create this project?
    - `$[Yes] > 1` (Press Enter)
    - **Prompt** Would you like to install the project dependencies?
    - `$[Yes] > 1` (Press Enter)

    Creates directory structure and downloads all the dependencies via npm.
    
    You're now done and ready to start building!



**Basic Project directory structure**

- /home/vagrant/MyProject
    - /aurelia_project (Configuration files, Gulp Tasks, Generators)
    - /scripts (Builds publish here)
    - /src (This is where you will be wroking primarily)
        - resources (Put your images/fonts/icons here.)
        - app.js (The default View-Model)
        - app.html (The default View for an Aurelia App. The main entry point)
        - environment.js
        - main.js
    - favicon.ico
    - index.html (No need to edit this file)
    - package.json (npm install --save adds entries to this file)
    
**Development cycle on the VM**
- Open Atom, add project folder: /home/vagrant/MyProject
    - This is where you will be editing the html/js/scss files
- Open a Terminal:
    - `$au run --watch` (Builds app, starts BrowserSynch web server)
- Open web browser of choice in VM and go to: http://localhost:9000
    - As you edit your code and save files, you will see the updates live
- If you setup using Vagrantfile above, then you can use browser in Host OS (Mac OS) and go to: http://192.168.0.3:9000



## Host your Aurelia Cli app on Nginx
**Environment**

This Example assumes you setup as explained in Example: **Setting Up Environment for Aurelia-cli Explained** of this document.

Summary of setup:
- On Mac OS X with Vagrant 1.8.4
- `$cd /path/to/project/vagrant`
- `$vagrant up`
- Log into VM via UI as User:vagrant / PW:vagrant
- `$cd /home/vagrant/MyProject` (Where you setup your Aurelia project as described in "Aurelia Cli Basics Explained" above.)
- EPEL repository should be setup

**Basic Project directory structure**

- /home/vagrant/MyProject
    - /aurelia_project
    - /scripts
    - /src
        - resources
        - app.js
        - app.html
        - environment.js
        - main.js
    - favicon.ico
    - index.html
    - package.json

**Build your App**
- `$au build` (Creates bundled files in projects /scripts directory)

**Setting up Nginx**

- `$sudo yum install nginx`
- Start the sever: `$sudo nginx`
- Edit nginx.conf file (See below)
    - Set root location to /var/www/html
    - Set resources location to /var/www/
- Reload the server after editing the file: `$sudo nginx -s reload`

**Copy files from Aurelia project to server**

    $sudo cp /home/vagrant/MyProject/index.html /var/www/html/
    $sudo cp -R /home/vagrant/MyProject/scripts /var/www/html/
    $sudo cp mkdir /var/www/src/
    $sudo cp -R /home/vagrant/MyProject/src/resources /var/www/src/

- Go to http://localhost on web browser in VM, you should see the default Aurelia app.
- If you setup as described above, port 80 is forwarded by VM, so you can go to a browser on your host OS and go to http://192.168.0.3:80 and see the site there as well.

    
>/etc/nginx/nginx.conf
>
>     # For more information on configuration, see:
>     #   * Official English Documentation: http://nginx.org/en/docs/
>     #   * Official Russian Documentation: http://nginx.org/ru/docs/
>     
>     user nginx;
>     worker_processes auto;
>     error_log /var/log/nginx/error.log;
>     pid /run/nginx.pid;
>     
>     # Load dynamic modules. See /usr/share/nginx/README.dynamic.
>     include /usr/share/nginx/modules/*.conf;
>     
>     events {
>         worker_connections 1024;
>     }
>     
>     http {
>         log_format  main  '$remote_addr - $remote_user [$time_local] "$request" '
>                           '$status $body_bytes_sent "$http_referer" '
>                           '"$http_user_agent" "$http_x_forwarded_for"';
>     
>         access_log  /var/log/nginx/access.log  main;
>     
>         sendfile            on;
>         tcp_nopush          on;
>         tcp_nodelay         on;
>         keepalive_timeout   65;
>         types_hash_max_size 2048;
>     
>         include             /etc/nginx/mime.types;
>         default_type        application/octet-stream;
>     
>         # Load modular configuration files from the /etc/nginx/conf.d directory.
>         # See http://nginx.org/en/docs/ngx_core_module.html#include
>         # for more information.
>     
>         server {
>             listen       80 default_server;
>             listen       [::]:80 default_server;
>             server_name  _;
>     
>             # Load configuration files for the default server block.
>             include /etc/nginx/default.d/*.conf;
>     
>         root /var/www/html/;
>     
>         location /src/resources {
>             root /var/www/;
>         }
>     
>             error_page 404 /404.html;
>                 location = /40x.html {
>             }
>     
>             error_page 500 502 503 504 /50x.html;
>                 location = /50x.html {
>             }
>         }
>     }
 

