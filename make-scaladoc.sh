#! /bin/bash
#Create the scaladoc API documentation.

rm -rf scaladoc
mkdir scaladoc

cd scaladoc
scaladoc `find ../src -name "*.scala"`

#konqueror index.html

