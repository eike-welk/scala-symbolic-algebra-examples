#! /bin/bash
#Create the scaladoc API documentation.

cd scaladoc
scaladoc `find ../src -name "*.scala"`

#konqueror index.html

