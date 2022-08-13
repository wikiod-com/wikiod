---
title: "Monostate"
slug: "monostate"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

As a side note, a few advantages of the `Monostate` pattern over the `Singleton`:

* There is no 'instance` method to be able to access an instance of the class.
* A `Singleton` does not conform to the Java beans notation, but a `Monostate` does.
* Lifetime of instances can be controlled.
* Users of the `Monostate` don't know that they are using a `Monostate`.
* Polymorphism is possible.

## The Monostate Pattern
The `Monostate` pattern is usually referred to as _syntactic sugar_ over the `Singleton` pattern or as a _conceptual `Singleton`_.

It avoids all the complications of having a single instance of a class, but all the instances use the same data.  
This is accomplished mostly by using `static` data members.  
One of the most important feature is that it's absolutely transparent for the users, that are completely unaware they are working with a `Monostate`. Users can create as many instances of a `Monostate` as they want and any instance is good as another to access the data.

The `Monostate` class comes usually with a companion class that is used to update the settings if needed.

It follows a minimal example of a `Monostate` in C++:

    struct Settings {
        Settings() {
            if(!initialized) {
                initialized = true;
                // load from file or db or whatever
                // otherwise, use the SettingsEditor to initialize settings
                Settings::width_ = 42;
                Settings::height_ = 128;
            }
        }

        std::size_t width() const noexcept { return width_; }
        std::size_t height() const noexcept { return height_; }

    private:
        friend class SettingsEditor;

        static bool initialized;
        static std::size_t width_;
        static std::size_t height_;
    };

    bool Settings::initialized = false;
    std::size_t Settings::width_;
    std::size_t Settings::height_;

    struct SettingsEditor {
        void width(std::size_t value) noexcept { Settings::width_ = value; }
        void height(std::size_t value) noexcept { Settings::height_ = value; }
    };

Here is an example of a simple implementation of a `Monostate` in Java:

    public class Monostate {
        private static int width;
        private static int height;

        public int getWidth() {
            return Monostate.width;
        }

        public int getHeight() {
            return Monostate.height;
        }

        public void setWidth(int value) {
            Monostate.width = value;
        }

        public void setHeight(int value) {
            Monostate.height = value;
        }

        static {
            width = 42;
            height = 128;
        }
    }

## Monostate-based hierarchies
In contrasto to the `Singleton`, the `Monostate` is suitable to be inherited to extend its functionalities, as long as member methods are not `static`.  
It follows a minimal example in C++:

    struct Settings {
        virtual std::size_t width() const noexcept { return width_; }
        virtual std::size_t height() const noexcept { return height_; }

    private:
        static std::size_t width_;
        static std::size_t height_;
    };

    std::size_t Settings::width_{0};
    std::size_t Settings::height_{0};

    struct EnlargedSettings: Settings {
        std::size_t width() const noexcept override { return Settings::height() + 1; }
        std::size_t height() const noexcept override { return Settings::width()  + 1; }
    };

