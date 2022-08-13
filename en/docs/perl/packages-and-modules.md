---
title: "Packages and modules"
slug: "packages-and-modules"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## Syntax
- require Module::Name; # Require by name from @INC
- require "path/to/file.pm"; # Require by relative path from @INC
- use Module::Name; # require and default import at BEGIN
- use Module::Name (); # require and no import at BEGIN
- use Module::Name (@ARGS); # require and import with args at BEGIN
- use Module::Name VERSION; # require, version check, and default import at BEGIN
- use Module::Name VERSION (); # require, version check, and no import at BEGIN
- use Module::Name VERSION (@ARGS); # require, version check, import with args at BEGIN
- do "path/to/file.pl"; # load and eval the given file

## Using a module
    use Cwd;

This will import the `Cwd` module at compile time and import its default symbols, i.e. make some of the module's variables and functions available to the code using it. (See also: [`perldoc -f use`](http://perldoc.perl.org/functions/use.html).)

Generally this is will do the right thing. Sometimes, however, you will want to control which symbols are imported. Add a list of symbols after the module name to export:

    use Cwd 'abs_path';

If you do this, only the symbols you specify will be imported (ie, the default set will not be imported).

When importing multiple symbols, it is idiomatic to use the `qw()` list-building construct:

    use Cwd qw(abs_path realpath);

Some modules export a subset of their symbols, but can be told to export everything with `:all`:

    use Benchmark ':all';

(Note that not all modules recognize or use the `:all` tag).

## Loading a module at runtime
    require Exporter;

This will ensure that the `Exporter` module is loaded at runtime if it hasn't already been imported. (See also: [`perldoc -f require`](http://perldoc.perl.org/functions/require.html).)

**N.B.:** Most users should `use` modules rather than `require` them. Unlike `use`, `require` does not call the module's import method and is executed at runtime, not during the compile.

This way of loading modules is useful if you can't decide what modules you need before runtime, such as with a plugin system:

    package My::Module;
    my @plugins = qw( One Two );
    foreach my $plugin (@plugins) {
        my $module = __PACKAGE__ . "::Plugins::$plugin";
        $module =~ s!::!/!g;
        require "$module.pm";
    }

This would try to load `My::Package::Plugins::One` and `My::Package::Plugins::Two`.
`@plugins` should of course come from some user input or a config file for this to make sense.
Note the substitution operator `s!::!/!g` that replaces each pair of colons with a slash. This is because you can load modules using the familiar module name syntax from `use` only if the module name is a bareword. If you pass a string or a variable, it must contain a file name.

## Using a module inside a directory
    use lib 'includes';
    use MySuperCoolModule;

`use lib 'includes';` adds the relative directory `includes/` as another module search path in `@INC`. So assume that you have a module file `MySyperCoolModule.pm` inside `includes/`, which contains:

    package MySuperCoolModule;

If you want, you can group as many modules of your own inside a single directory and make them findable with one `use lib` statement.

At this point, using the subroutines in the module will require prefixing the subroutine name with the package name:

    MySuperCoolModule::SuperCoolSub_1("Super Cool String");

To be able to use the subroutines without the prefix, you need to export the subroutine names so that they are recognised by the program calling them. Exporting can be set up to be automatic, thus:

    package MySuperCoolModule;
    use base 'Exporter';
    our @EXPORT = ('SuperCoolSub_1', 'SuperCoolSub_2');

Then in the file that `use`s the module, those subroutines will be automatically available:

    use MySuperCoolModule;
    SuperCoolSub_1("Super Cool String");

Or you can set up the module to conditionally export subroutines, thus:

    package MySuperCoolModule;
    use base 'Exporter';
    our @EXPORT_OK = ('SuperCoolSub_1', 'SuperCoolSub_2');

In which case, you need to explicitly request the desired subroutines to be exported in the script that `use`s the module:

    use MySuperCoolModule 'SuperCoolSub_1';
    SuperCoolSub_1("Super Cool String");


## Executing the contents of another file
    do './config.pl';

This will read in the contents of the config.pl file and execute it. (See also: [`perldoc -f do`](http://perldoc.perl.org/functions/do.html).)

**N.B.:** Avoid `do` unless golfing or something as there is no error checking. For including library modules, use `require` or `use`.


## CPAN.pm


## List all installed modules


