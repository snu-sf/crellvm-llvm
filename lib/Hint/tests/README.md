### These examples and README should be compensated more

# Boost
* If you meet an error `'boost/any.hpp' file not found`, add `-I $PATH_TO_BOOST` in shell script or command line compile option.

# Cereal
* Add -I $PATH_TO_CEREAL in shell script or command line compile option for using cereal.

# Shell script
* Below example is about how to use Intruder.
```
#/usr/bin/env bash

clang++ -std=c++11 -I $PATH_TO_BOOST Intruder_example.cpp -o example && ./example
```

