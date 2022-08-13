---
title: "Async boostprocess"
slug: "async-boostprocess"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Using all 3 pipes of a child process asynchronously.
    #include <vector>
    #include <string>
    #include <boost/process.hpp>
    #include <boost/asio.hpp>
    #include <boost/process/windows.hpp>
    
    int Run(
        const std::string& exeName,         ///< could also be UTF-16 for Windows
        const std::string& args,            ///< could also be UTF-16 for Windows
        const std::string& input,           ///< [in] data for stdin
        std::string& output,                ///< [out] data from stdout
        std::string& error                  ///< [out] data from stderr
    )
    {
        using namespace boost;
    
        asio::io_service ios;
    
        // stdout setup
        //
        std::vector<char> vOut(128 << 10);           // that worked well for my decoding app.
        auto outBuffer{ asio::buffer(vOut) };
        process::async_pipe pipeOut(ios);
    
        std::function<void(const system::error_code & ec, std::size_t n)> onStdOut;
        onStdOut = [&](const system::error_code & ec, size_t n)
        {
            output.reserve(output.size() + n);
            output.insert(output.end(), vOut.begin(), vOut.begin() + n);
            if (!ec)
            {
                asio::async_read(pipeOut, outBuffer, onStdOut);
            }
        };
    
        // stderr setup
        //
        std::vector<char> vErr(128 << 10);
        auto errBuffer{ asio::buffer(vErr) };
        process::async_pipe pipeErr(ios);
    
        std::function<void(const system::error_code & ec, std::size_t n)> onStdErr;
        onStdErr = [&](const system::error_code & ec, size_t n)
        {
            error.reserve(error.size() + n);
            error.insert(error.end(), vErr.begin(), vErr.begin() + n);
            if (!ec)
            {
                asio::async_read(pipeErr, errBuffer, onStdErr);
            }
        };
    
        // stdin setup
        //
        auto inBuffer{ asio::buffer(input) };
        process::async_pipe pipeIn(ios);
    
        process::child c(
            exeName + " " + args,                   // exeName must be full path
            process::std_out > pipeOut, 
            process::std_err > pipeErr, 
            process::std_in < pipeIn
        );
    
        asio::async_write(pipeIn, inBuffer, 
            [&](const system::error_code & ec, std::size_t n) 
            {
                pipeIn.async_close();                     //  tells the child we have no more data
            });
    
        asio::async_read(pipeOut, outBuffer, onStdOut);
        asio::async_read(pipeErr, errBuffer, onStdErr);
    
        ios.run();
        c.wait();
        return c.exit_code();                            // return the process' exit code.
    }



## IMPORTANT for boost 1.64
There is a bug/fix for boost 1.64, the bug only affects Windows, apparently.

reference: https://github.com/klemens-morgenstern/boost-process/issues/90
and    https://github.com/klemens-morgenstern/boost-process/commit/74814e46c1614850a8e447fd689c21cf82f36ceb

    


in file `boost\process\detail\windows\async_pipe.hpp, line 79`:
    
        ~async_pipe()
        {
    //fix
            //if (_sink .native()  != ::boost::detail::winapi::INVALID_HANDLE_VALUE_)
            //    ::boost::detail::winapi::CloseHandle(_sink.native());
            //if (_source.native() != ::boost::detail::winapi::INVALID_HANDLE_VALUE_)
            //    ::boost::detail::winapi::CloseHandle(_source.native());
            boost::system::error_code ec;
            close(ec);
    //fix
        }


