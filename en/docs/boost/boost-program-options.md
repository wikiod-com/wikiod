---
title: "Boost Program Options"
slug: "boost-program-options"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Basic Usage
Boost program options provides a simple and safe way to parse and handle command line arguments.

    #include <boost/program_options.hpp>
    #include <string>
    #include <iostream>

    int main(int argc, char** argv) {
      namespace po = boost::program_options;
    
      po::variables_map vm;
      po::options_description desc("Allowed Options");
    
      // declare arguments
      desc.add_options()
        ("name", po::value<std::string>()->required(), "Type your name to be greeted!");
    
      // parse arguments and save them in the variable map (vm)
      po::store(po::parse_command_line(argc, argv, desc), vm);
    
      std::cout << "Hello " << vm["name"].as<std::string>() << std::endl;
    
      return 0;
    }

Compile and run with:

     $ g++ main.cpp -lboost_program_options && ./a.out --name Batman
    Hello Batman

You can output a `boost::program_options::options_description` object to print the expected argument format:

    std::cout << desc << std::endl;

would produce:

    Allowed Options:
      --name arg                Type your name to be greeted!

## Error Handling
`boost::program_options::notify` can be used to report any errors in the paramters passing

    #include <boost/program_options.hpp>
    #include <string>
    #include <iostream>

    int main(int argc, char** argv) {
      namespace po = boost::program_options;
    
      po::variables_map vm;
      po::options_description desc("Allowed Options");
    
      // declare options
      desc.add_options()
        ("name", po::value<std::string>()->required(), "Type your name to be greeted!");
    
      // parse arguments
      po::store(po::parse_command_line(argc, argv, desc), vm);
    
      // check arguments
      try {
        po::notify(vm);
      } catch (std::exception& e) {
        std::cout << "Error: " << e.what() << std::endl;
        std::cout << desc << std::endl;
        return 1;
      }
    
      // program logic
      std::cout << "Hello " << vm["name"].as<std::string>() << std::endl;
    
      return 0;
    }

Passing illegal arguments produces helpful errors messages

     $ ./a.out
    Error: the option '--name' is required but missing
    Allowed Options:
      --name arg            Type your name to be greeted!




## Default Values
A default valued command line argument can be specified easily:

    // declare options
    desc.add_options()
      ("name", po::value<std::string>()->required(), "Type your name to be greeted!")
      ("rank", po::value<std::string>()->default_value("Dark Knight"), "Your rank");

Its value is also added to the variable map:

      std::cout << "Hello " << vm["name"].as<std::string>() << " " << vm["rank"].as<std::string>() << std::endl;


The default value is shown in the description...

    $ ./a.out
    Error: the option '--name' is required but missing
    Allowed Options:
      --name arg                Type your name to be greeted!
      --rank arg (=Dark Knight) Your rank

... and used if not specified...

    $ ./a.out --name Batman
    Hello Batman Dark Knight

... but can be  overwritten at command line:

     $ ./a.out --name Batman --rank FlyingSquirrel
    Hello Batman FlyingSquirrel



## Switches
A switch is a command line argument which takes no value. It can be specified with:

    desc.add_options()
      ("hidden", po::bool_switch()->default_value(false), "Hide your name");

And used with:

    if (vm["hidden"].as<bool>())
       std::cout << "Hello *****" << std::endl;

from the command line:

     $ ./a.out --name Batman --hidden
    Hello *****

and in the description it shows as:

    Allowed Options:
      --name arg                Type your name to be greeted!
      --rank arg (=Dark Knight) Your rank
      --hidden                  Hide your name

