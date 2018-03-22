#! /bin/sh
#set -x
#=========================================================================
# Copyright (C) GemTalk Systems, LLC. 2013.  All Rights Reserved.
#
# Name - make_certs.sh
#
#  Shell script to create certificate files for WebTools.
#
#  This script is based on $GEMSTONE/examples/openssl/make_example_certs.sh.
#
#=========================================================================

# Log file for the output of this script.
LOG_FILE=make_certs.log

# Place to put the full path to the openssl executable.
# Otherwise, it is assumed to be in your path.
#
# OPENSSL=/usr/bin/openssl

# Do some checks before starting

if [ "$OPENSSL" = "" ]; then
#  "Just hope it's in the path..."
  OPENSSL=openssl
fi

echo "" > $LOG_FILE
if [ ! -f "$LOG_FILE" ]; then
 echo "Error: could not open the log file '$LOG_FILE'"
 exit 1
fi

if [ "$DOMAIN" = "" ]; then
 echo "Error: \$DOMAIN must be defined"
 exit 1
fi

# update server config file to match local domain
sed "s/DOMAIN/$DOMAIN/g" server.config.template > server.config

# ##############################################################################
# Step 1: Create the root private key and certificate
# ##############################################################################

echo "Creating root private key" >>$LOG_FILE 2>&1 
$OPENSSL req \
  -newkey rsa:1024 \
  -sha1 \
  -config  root.config \
  -keyout  rootkey.pem \
  -out     rootkey.pem \
  -passout pass:ROOTKEY \
  >> $LOG_FILE 2>&1
# rootkey.pem is used only to create root certificate

EXITCODE="$?"
if [ $EXITCODE -ne 0 ]; then
  echo "Error $EXITCODE at line $LINENO"
  exit 1
fi

echo $"\nCreating root certificate (using root key)" >>$LOG_FILE 2>&1 
$OPENSSL x509 \
  -req \
  -in 			rootkey.pem \
  -sha1 \
  -days 		3650 \
  -extensions 	v3_ca \
  -signkey 		rootkey.pem \
  -out 			rootcert.pem \
  -passin 		pass:ROOTKEY \
  >> $LOG_FILE 2>&1
# rootkey.pem goes into root.pem
# rootcert.pem goes into root.pem, serverCA.pem, and server.pem
EXITCODE="$?"
if [ $EXITCODE -ne 0 ]; then
  echo "Error $EXITCODE at line $LINENO"
  exit 1
fi

echo $"\nCombining pieces into root certificate" >>$LOG_FILE 2>&1 
cat \
  rootcert.pem \
  rootkey.pem \
  >> root.pem
  
rm rootkey.pem

echo $"\nRoot certificate" >>$LOG_FILE 2>&1 
$OPENSSL x509 -subject -issuer -noout -in root.pem >>$LOG_FILE 2>&1

# ##############################################################################
# Step 2: Create the CA private key and certificate
# ##############################################################################

echo $"\n\nCreating server CA private key" >>$LOG_FILE 2>&1 
$OPENSSL req \
  -newkey 	rsa:1024 \
  -sha1 \
  -config 	certificateAuthority.config \
  -keyout 	serverCAkey.pem \
  -out 		serverCAreq.pem \
  -passout 	pass:SERVERCAKEY \
  >> $LOG_FILE 2>&1 
# serverCAkey.pem goes into serverCA.pem
# serverCAreq.pem is used only to create serverCAcert.pem

EXITCODE="$?"
if [ $EXITCODE -ne 0 ]; then
  echo "Error $EXITCODE at line $LINENO"
  exit 1
fi

echo $"\nCreating server CA certificate signed by the root private key" >>$LOG_FILE 2>&1 
$OPENSSL x509 \
  -req \
  -in 				serverCAreq.pem \
  -sha1 \
  -extensions 		v3_ca \
  -days 			3650 \
  -CA 				root.pem \
  -CAkey 			root.pem \
  -CAcreateserial \
  -out 				serverCAcert.pem \
  -passin 			pass:ROOTKEY \
  >> $LOG_FILE 2>&1 
# serverCAcert.pem goes into serverCA.pem and server.pem

EXITCODE="$?"
if [ $EXITCODE -ne 0 ]; then
  echo "Error $EXITCODE at line $LINENO"
  exit 1
fi

rm root.pem \
  root.srl \
  serverCAreq.pem

echo $"\nCombining pieces into server CA certificate" >>$LOG_FILE 2>&1 
cat \
  serverCAcert.pem \
  serverCAkey.pem \
  rootcert.pem \
  > serverCA.pem

rm serverCAkey.pem

echo $"\nFinished creating server authority key and certificate" >>$LOG_FILE 2>&1 
$OPENSSL x509 -subject -issuer -noout -in serverCA.pem >>$LOG_FILE 2>&1 

# ##############################################################################
# Step 3: create the server private key and certificate
# ##############################################################################

echo $"\nCreating server private key" >>$LOG_FILE 2>&1 
$OPENSSL req \
  -newkey 	rsa:1024 \
  -config 	server.config \
  -sha1 \
  -keyout 	serverkey.pem \
  -out 		serverreq.pem \
  -passout 	pass:SERVERKEY \
  >> $LOG_FILE 2>&1 
# serverkey.pem has pass phrase removed and is added to server.pem
# serverreq.pem is used only to create servercert.pem

EXITCODE="$?"
if [ $EXITCODE -ne 0 ]; then
  echo "Error $EXITCODE at line $LINENO"
  exit 1
fi

echo $"\nCreating server certificate signed by the server CA private key" >>$LOG_FILE 2>&1 
$OPENSSL x509 \
  -req \
  -in 				serverreq.pem \
  -sha1 \
  -days 			3650 \
  -extensions 		usr_cert \
  -CA 				serverCA.pem \
  -CAkey 			serverCA.pem \
  -CAcreateserial \
  -out 				servercert.pem \
  -passin 			pass:SERVERCAKEY \
  >> $LOG_FILE 2>&1 
# servercert.pem is used to create server.pem

EXITCODE="$?"
if [ $EXITCODE -ne 0 ]; then
  echo "Error $EXITCODE at line $LINENO"
  exit 1
fi

rm serverreq.pem

echo $"\nRemoving pass phrase from server private key" >>$LOG_FILE 2>&1 
$OPENSSL rsa \
  -in 		serverkey.pem \
  -out 		serverkey_NO_PASS.pem \
  -passin 	pass:SERVERKEY \
  >>$LOG_FILE 2>&1 

echo $"\nCombining pieces into server certificate" >>$LOG_FILE 2>&1 
cat \
  servercert.pem \
  serverkey_NO_PASS.pem \
  serverCAcert.pem \
  rootcert.pem \
  > server.pem

rm rootcert.pem \
  serverCA.srl \
  serverCAcert.pem \
  serverkey.pem \
  servercert.pem \
  serverkey_NO_PASS.pem

echo $"\nFinished creating server certificate" >>$LOG_FILE 2>&1 
$OPENSSL x509 -subject -issuer -noout -in server.pem >>$LOG_FILE 2>&1 

echo $"\n\nVerifying certificate server.pem :" >>$LOG_FILE 2>&1 
$OPENSSL verify \
  -CAfile 	serverCA.pem \
  -purpose 	sslclient \
  server.pem \
  >>$LOG_FILE 2>&1 

exit 0
