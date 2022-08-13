---
title: "boost String Algorithms Library"
slug: "boost-string-algorithms-library"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

[**Boost Documention on String Algrorithms**][bststralgs]

[bststralgs]: http://www.boost.org/doc/libs/1_61_0/doc/html/string_algo.html

## boost::split()
    #include <iostream>
    #include <vector>
    #include <string>
    #include <boost/algorithm/string.hpp>

    using namespace std;

    int main()
    {
    
        // String to split

        string str = "You're supposed to see this!|NOT THIS!!!!!!";

        // Line container

        vector<string> lines;

        // Splits string

        boost::split(lines, str, boost::is_any_of("|"), boost::token_compress_on);

        // Outputs 1 half of the split string

        cout << lines.at(0).c_str() << endl;

        // Waits for input before program exits

        cin.get();

        return 0;
    }

The following is the program in *[psuedocode][1]*:

**Declare** [string][2] *str* and **set** it to *"You're supposed to see this!|NOT THIS!!!!!!"*.

**Declare** [vector][3] *lines* of **type** string.

**Split** string *str* into vector *lines* if regex *"|"* is found.

**Print** object at index 0 in lines.

**Get** input.

[1]: https://en.wikipedia.org/wiki/Pseudocode
[2]: https://www.wikiod.com/docs/c%2b%2b/488/stdstring#t=201611012349132871428
[3]: https://www.wikiod.com/docs/c%2b%2b/511/stdvector

## Replace Algrorithms
   
*boost::replace_all():*
=======================

    #include <iostream>
    #include <string>
    #include <boost/algorithm/string.hpp>

    using namespace std;

    int main()
    {
    
        // String to replace characters in

        string str = "Darn you, Darn you to the 5th power!!!";

        // Replace "Darn" with "*CENSORED*"

        boost::replace_all(str, "Darn", "*CENSORED*");

        // Print str

        cout << str.c_str() << endl;

        // Waits for program to exit

        cin.get();

        return 0;
    }

*boost::replace_first():*
========================

    #include <iostream>
    #include <string>
    #include <boost/algorithm/string.hpp>

    using namespace std;

    int main()
    {
    
        // String to replace characters in

        string str = "Darn you, Darn you to the 5th power!!!";

        // Replace the first instance of "Darn" with "*CENSORED*"

        boost::replace_first(str, "Darn", "*CENSORED*");

        // Print str

        cout << str.c_str() << endl;

        // Waits for program to exit

        cin.get();

        return 0;
    }

*boost_replace_last():*
=======================

    #include <iostream>
    #include <string>
    #include <boost/algorithm/string.hpp>

    using namespace std;

    int main()
    {
    
        // String to replace characters in

        string str = "Darn you, Darn you to the 5th power!!!";

        // Replace the last instance of "Darn" with "*CENSORED*"

        boost::replace_last(str, "Darn", "*CENSORED*");

        // Print str

        cout << str.c_str() << endl;

        // Waits for program to exit

       cin.get();

       return 0;
    }

## Case conversion methods
*to_upper():*
============

    #include <iostream>
    #include <string>
    #include <boost/algorithm/string.hpp>

    using namespace std;

    int main()
    {
    
        // String to convert characters to uppercase

        string str = "ThIS iS SUpPoSEd tO Be UpPeR CAsE.";

        // Convert characters in str to upper case

        boost::to_upper(str);

        // Print str

        cout << str.c_str() << endl;

        // Waits for program to exit

        cin.get();

        return 0;
    }

*to_lower():*
=============

    #include <iostream>
    #include <string>
    #include <boost/algorithm/string.hpp>

    using namespace std;

    int main()
    {
    
        // String to convert characters to lowercase

        string str = "ThIS iS SUpPoSEd tO Be LoWer CAsE.";

        // Convert characters in str to lower case

        boost::to_lower(str);

        // Print str

        cout << str.c_str() << endl;

        // Waits for program to exit

        cin.get();

        return 0;
    }


