#!/bin/bash

rm -rf target/classes/pdd1/*.class program.jar
javac -classpath `$HADOOP_PREFIX/bin/yarn classpath` -d target/classes/ src/pdd1/*.java
cd target/classes/
jar cfe ../../program.jar pdd1.MainApp pdd1/*.class
