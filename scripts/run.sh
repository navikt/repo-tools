#!/bin/sh

file_name="/opt/app/secrets.properties"
if [ -f $file_name ]
then
   echo "Reading env from $file_name"
   source $file_name
 else
  echo "$file_name does not exist"
fi

/opt/app/bin/repo-tools

sleep 60
