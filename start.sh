#!/bin/bash
# Copyright (c) by GemTalk Systems LLC 2013, 2019

# Script used to start WebTools

if [ ! -f ".topazini" ]; then
    cp topazini .topazini
fi

export WEBTOOLS=`pwd`
if [ ! -f "server.pem" ]; then
  if [ "$DOMAIN" = "" ]; then
    export DOMAIN=`hostname`
  fi
  (cd openssl; ./make_certs.sh)
  EXITCODE="$?"
  if [ ! "$EXITCODE" == 0 ]; then
    echo "Error $EXITCODE at line $LINENO"
    exit 1
  fi
  cp openssl/server.pem .
fi

topaz -l -T 50000 << EOF
input install.tpz
run
(System myUserProfile symbolList last at: #'Server') run
%
