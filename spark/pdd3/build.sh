#!/bin/bash

wget -N https://piccolo.link/sbt-1.1.4.tgz
tar zxvf sbt-1.1.4.tgz
./sbt/bin/sbt assembly
cp -f target/scala-2.11/pdd3-assembly-1.0.jar solution.jar
