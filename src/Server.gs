! Copyright (c) 2011-2013, 2019 GemTalk Systems LLC. All Rights Reserved.

doit
Object subclass: 'Server'
  instVarNames: #( server socket resultCode
                    httpHeaders stream startTime)
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: WebTools
  options: #()

%

! ------------------- Class comment for Server
doit
Server comment: 
''
%

! Remove existing behavior from Server
doit
Server removeAllMethods.
Server class removeAllMethods.
%
! ------------------- Class methods for Server
category: 'other'
set compile_env: 0
classmethod: Server
run
"
	Server run.
"
	self startSecureServerAtPort: 8080.
%
category: 'other'
set compile_env: 0
classmethod: Server
startSecureServerAtPort: anInteger
"
	If anInteger is nil then assign a random port.
	GsSocket closeAll.
	Server startSecureServerAtPort: 8080.
"
	| delay |
	delay := Delay forSeconds: 5.
	GsFile stdout nextPutAll: (self new startSecureServerAtPort: anInteger); lf; flush.
	[System commitTransaction] whileTrue: [delay wait].
%
category: 'other'
set compile_env: 0
classmethod: Server
startServerAtPort: anInteger
"
	If anInteger is nil then assign a random port.
	Server startServerAtPort: 8080.
"
	| delay |
	delay := Delay forSeconds: 5.
	GsFile stdout nextPutAll: (self new startServerAtPort: anInteger); lf; flush.
	[System commitTransaction] whileTrue: [delay wait].
%
! ------------------- Instance methods for Server
category: 'Request Handler'
set compile_env: 0
method: Server
beNoCache

	httpHeaders
		at: 'Cache-Control'			
		put: 'no-cache'.
%
category: 'Request Handler'
set compile_env: 0
method: Server
contentType: aString

	httpHeaders
		at: 'Content-Type'
		put: aString.
%
category: 'Request Handler'
set compile_env: 0
method: Server
contentTypes

	^KeyValueDictionary new
		at: 'css'		put: 'text/css';
		at: 'gif'		put: 'image/gif';
		at: 'html'	put: 'text/html; charset=UTF-8';
		at: 'ico'		put: 'image/x-icon';
		at: 'js'		put: 'text/javascript';
		at: 'png'		put: 'image/png';
		at: 'json'	put: 'text/json';
		yourself.
%
category: 'Request Handler'
set compile_env: 0
method: Server
doAnswer

	| string numWritten |
	string := stream contents.
	stream := (WriteStream on: String new)
		nextPutAll: 'HTTP/1.1 ';
		nextPutAll: resultCode printString; space;
		nextPutAll: self reasonPhrase; 
		nextPut: Character cr;
		nextPut: Character lf;
		yourself.
	httpHeaders
		at: 'Content-Length'			
		put: string size printString.
	httpHeaders keys asSortedCollection do: [:each | 
		stream 
			nextPutAll: each;
			nextPutAll: ': ';
			nextPutAll: (httpHeaders at: each);
			nextPut: Character cr;
			nextPut: Character lf;
			yourself.
	].
	stream
		nextPut: Character cr;
		nextPut: Character lf;
		nextPutAll: string;
		yourself.
	socket writeWillNotBlock ifFalse: [self error: 'socket write will block'].
	string := stream contents.
	numWritten := socket 			
		linger: true length: 60; "wait up to a minute for data to finish writing"
		write: string.
	socket shutdownWriting.
	[	"it seems that linger is not sufficient"
		(Delay forSeconds: 5) wait.
		socket close.
	] fork.
	numWritten == string size ifFalse: [
		string := 'Tried to write ' , string size printString , ', but wrote ' , numWritten printString.
		GsFile stderr nextPutAll: string; lf.
		self error: string.
		^self.
	].
%
category: 'Request Handler'
set compile_env: 0
method: Server
encode: aString

	| readStream writeStream |
	readStream := ReadStream on: aString.
	writeStream := WriteStream on: String new.
	[
		readStream atEnd not.
	] whileTrue: [
		| next index |
		next := readStream next.
		index := #($" $& $' $< $>) indexOf: next.
		0 < index ifTrue: [
			writeStream nextPutAll: (#('&quot;' '&amp;' '&apos;' '&lt;' '&gt;') at: index).
		] ifFalse: [
			writeStream nextPut: next.
		].
	].
	^writeStream contents.
%
category: 'Request Handler'
set compile_env: 0
method: Server
errorExpectedGetOrPost: aString

	resultCode := 404.
	self htmlWithBody: 'Expected a GET or POST request but got ' , aString printString , '!'.
%
category: 'Request Handler'
set compile_env: 0
method: Server
errorNotFound: aString

	resultCode := 404.
	self htmlWithBody: aString printString , ' not Found!'.
%
category: 'Request Handler'
set compile_env: 0
method: Server
handleRequest

	"Called on a copy of the instance accepting connections"
	| string readStream type line headers contentLength path args selector class pieces |
	string := String new.
	[
		string size < 20.
	] whileTrue: [
		[
			string addAll: (socket readString: 1000).
		] on: Error do: [:ex | 
			GsFile stderr nextPutAll: ex description; lf.
			socket close.
			^self.
		].
	].
	readStream := ReadStream on: string.
	type := readStream upTo: Character space.
	(type = 'GET' or: [type = 'POST']) ifFalse: [^self errorExpectedGetOrPost: type].
	path := readStream upTo: Character space.
	string := readStream upTo: Character cr.
	readStream peekFor: Character lf.
	headers := Dictionary new.
	[
		line := readStream upTo: Character cr.
		readStream peekFor: Character lf.
		line notEmpty.
	] whileTrue: [
		| index |
		index := line indexOf: $:.
		headers
			at: (line copyFrom: 1 to: index - 1)
			put: (line copyFrom: index + 1 to: line size) trimBlanks.
	].
	(contentLength := headers at: 'Content-Length' ifAbsent: [nil]) notNil ifTrue: [
		contentLength := contentLength asNumber.
		string := readStream upToEnd.
		100 timesRepeat: [
			string size < contentLength ifTrue: [
				string addAll: (socket readString: 10000).
				string size < contentLength ifTrue: [
					(Delay forMilliseconds: 10) wait.
				].
			].
		].
	].
	type = 'GET' ifTrue: [
		pieces := path subStrings: $?.
		path := pieces at: 1.
		string := 1 < pieces size 
			ifTrue: [pieces at: 2]
			ifFalse: [''].
	].
	args := Dictionary new.
	(string subStrings: $&) do: [:each | 
		| index key value values |
		index := each indexOf: $=.
		key := each copyFrom: 1 to: index - 1.
		value := self translate: (each copyFrom: index + 1 to: each size).
		(6 < key size and: [(key copyFrom: key size - 5 to: key size) = '%5B%5D']) ifTrue: [
			key := key copyFrom: 1 to: key size - 6.
			values := args at: key ifAbsent: [Array new].
			values add: value.
			value := values.
		].
		args
			at: key
			put: value.
	].
	path = '/' ifTrue: [path := '/index.html'].
	pieces := path subStrings: $/.
	selector := (pieces at: 2) asSymbol.
	(self class canUnderstand: selector) ifTrue: [
		^self returnJSON: [self perform: selector].
	].
	class := WebTools at: selector ifAbsent: [nil].
	class notNil ifTrue: [
		^self returnJSON: [
			selector := 3 = pieces size 
				ifTrue: [(pieces at: 3) asSymbol]
				ifFalse: [#'json'].
			(class arguments: args) perform: selector.
		].
	].
	self handleRequestFor: path.
%
category: 'Request Handler'
set compile_env: 0
method: Server
handleRequestFor: aString

	| gsFile |
	(gsFile := GsFile openReadOnServer: '$WEBTOOLS/htdocs' , aString) notNil ifTrue: [
		| type |
		type := (aString subStrings: $.) last.
		stream nextPutAll: gsFile contents.
		gsFile close.
		resultCode := 200.
		self contentType: (self contentTypes at: type ifAbsent: ['text/html; UTF-8']).
		self doAnswer.
		^self.
	].
	self errorNotFound: aString.
%
category: 'Request Handler'
set compile_env: 0
method: Server
handleRequestOn: aSocket
	"Called on a *copy* of the instance accepting connections
	so we can keep local stuff in instance variables and handle
	requests in parallel."

	startTime := Time millisecondClockValue.
	System commitTransaction.
	server := nil.
	socket := aSocket.
	resultCode := nil.
	stream := WriteStream on: String new.
	self 
		initializeHeaders;
		handleRequest;
		yourself.
	System commitTransaction.
%
category: 'Request Handler'
set compile_env: 0
method: Server
handleSecureRequestOn: aSocket

	| certificatePath flag certError string |
	aSocket disableCertificateVerification.
	certificatePath := '$WEBTOOLS/server.pem'.
	flag := aSocket 
		useCertificateFile: certificatePath
		withPrivateKeyFile: certificatePath
		privateKeyPassphrase: nil.
	flag ifFalse: [
		GsSecureSocket fetchErrorStringArray do: [:each | 
			GsFile stderr nextPutAll: each; lf.
		].
		self error: 'GsSecureSocket>>#''useCertificateFile:withPrivateKeyFile:privateKeyPassphrase:'' failed. See stderr for details'.
	].
	flag := aSocket setCipherListFromString: 'ALL:!ADH:@STRENGTH'.
	flag ifFalse: [
		self error: 'GsSecureSocket>>#''setCipherListFromString:'' failed'.
	].
	aSocket secureAccept ifTrue: [^self handleRequestOn: aSocket].
	certError := GsSecureSocket fetchLastCertificateVerificationErrorForServer.
	certError isNil ifTrue: [	"This seems to happen when we come in as HTTP rather than HTTPS"
		| crlf string i j  |
		crlf := Character cr asString , Character lf asString.
		string := String new.
		aSocket changeClassTo: GsSocket.
		10 timesRepeat: [
			(aSocket readWillNotBlockWithin: 10) ifTrue: [
				string addAll: (aSocket readString: 1000).
				i := string indexOfSubCollection: 'Host: ' startingAt: 1.
				0 < i ifTrue: [
					j := string indexOfSubCollection: crlf startingAt: i.
					0 < j ifTrue: [
						string := string copyFrom: i + 6 to: j - 1.
						string := 'HTTP/1.1 303 See Other' , crlf , 
									'Location: https://' , string , crlf , crlf.
						aSocket  write: string; close.
						^self
					].
				].
			].
		].
		GsFile stdout nextPutAll: string; lf.
		string := aSocket peerName , ' (' , aSocket peerAddress printString , ')'.
		aSocket close.
		GsFile stderr nextPutAll: 'Unspecified certificate verification error from ' , string; lf.
		^self.
	]. 
	string := socket fetchLastIoErrorString.
	string isNil ifFalse: [
		self error: string.
	].
	GsSecureSocket fetchErrorStringArray do: [:each | 
		GsFile stderr nextPutAll: each; lf.
	].
	self error: 'GsSecureSocket>>#''secureAccept'' failed. See stderr for details'.
%
category: 'Request Handler'
set compile_env: 0
method: Server
handleSecureRequestOnA: aSocket

	| certError string |
	aSocket secureAccept ifTrue: [^self handleRequestOn: aSocket].
	certError := GsSecureSocket fetchLastCertificateVerificationErrorForServer.
	certError isNil ifTrue: [
		self error: 'GsSecureSocket class>>#''fetchLastCertificateVerificationErrorForServer'' returned nil'.
	]. 
	string := socket fetchLastIoErrorString.
	string isNil ifFalse: [
		self error: string.
	].
	GsSecureSocket fetchErrorStringArray do: [:each | 
		GsFile stderr nextPutAll: each; lf.
	].
	self error: 'GsSecureSocket>>#''secureAccept'' failed. See stderr for details'.
%
category: 'Request Handler'
set compile_env: 0
method: Server
htmlForError: ex

	| description |
	((description := ex description) isKindOf: String) ifFalse: [description := description printString].
	stream := WriteStream on: String new.
	stream nextPutAll: '<h3>' , description , '</h3>'.
	((GsProcess stackReportToLevel: 100) subStrings: Character lf) do: [:each | 
		stream nextPutAll: each , '<br />'.
	].
	^stream contents.
%
category: 'Request Handler'
set compile_env: 0
method: Server
htmlWithBody: aString

	stream nextPutAll: 
'<!DOCTYPE html PUBLIC ''-//W3C//DTD XHTML 1.0 Strict//EN'' ''http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd''>
<html xmlns=''http://www.w3.org/1999/xhtml'' xml:lang=''en'' lang=''en''>
<head><title>GemStone/S Web Tools</title></head>
<body>' , aString , '</body></html>'.
	self doAnswer.
%
category: 'Request Handler'
set compile_env: 0
method: Server
initializeHeaders

	| dateTime dateString |
	dateTime := DateTime now.
	dateString := (WriteStream on: String new)
		nextPutAll: (#('Sun' 'Mon' 'Tue' 'Wed' 'Thu' 'Fri' 'Sat') at: dateTime dayOfWeekGmt); space;
		nextPutAll: (dateTime asStringGmtUsingFormat: #(1 2 3 $  2 1 $: true true false false));
		nextPutAll: ' GMT';
		contents.
	httpHeaders := Dictionary new
		at: 'Accept-Ranges'			put: 'bytes';
		at: 'Allow'						put: 'GET';
		at: 'Cache-Control'			put: 'max-age=0'; "86400';"
		at: 'Content-Encoding'		put: 'none';
		at: 'Content-Language'		put: 'en';
		at: 'Content-Type'			put: 'text/html; charset=utf-8';
		at: 'Date' 						put: dateString;
		at: 'Server'						put: 'GemStone/S 64 Bit Server';
		yourself.
%
category: 'Request Handler'
set compile_env: 0
method: Server
reasonPhrase

	^(Dictionary new
		at: 200 put: 'OK';
		at: 404 put: 'Not Found';
		at: 405 put: 'Method Not Allowed';
		at: 426 put: 'Upgrade Required';
		yourself)
		at: resultCode
		ifAbsent: ['Unknown Error'].
%
category: 'Request Handler'
set compile_env: 0
method: Server
returnJSON: aBlock

	[
		| data |
		resultCode := 200.
		self contentType: 'text/json; charset=UTF-8'.
		self beNoCache.
		data := [
			aBlock value.
		] on: Error , Admonition do: [:ex | 
			ex return: (Dictionary new 
				at: '_error' put: ex description; 
				at: '_stack' put: (GsProcess stackReportToLevel: 50);
				yourself).
		].
		data at: '_time' put: (Time millisecondClockValue - startTime).
		data printJsonOn: stream.
		self doAnswer.
	] on: Error , Admonition do: [:ex | 
		resultCode := 500.
		self htmlForError: ex.
		self doAnswer.
		ex return.
	].
%
category: 'Web Server'
set compile_env: 0
method: Server
serverURL

	^'https://' , ((System descriptionOfSession: System session) at: 3) , ':' , socket port printString , '/'.
%
category: 'Web Server'
set compile_env: 0
method: Server
setupSecureServerSocketAtPort: anInteger

	socket := GsSecureSocket newServer.
	(socket makeServerAtPort: anInteger) isNil ifTrue: [
		| string |
		string := socket lastErrorString.
		socket close.
		self error: string.
	].
%
category: 'Web Server'
set compile_env: 0
method: Server
setupServerSocketAtPort: anInteger

	socket := GsSocket new.
	(socket makeServerAtPort: anInteger) isNil ifTrue: [
		| string |
		string := socket lastErrorString.
		socket close.
		self error: string.
	].
%
category: 'Web Server'
set compile_env: 0
method: Server
startForegroundServerAtPort: anInteger

	self setupServerSocketAtPort: anInteger.
	GsFile stdout nextPutAll: self serverURL; lf.
	[
		[true] whileTrue: [(Delay forSeconds: 5) wait].
	] forkAt: Processor userBackgroundPriority.
	[
		[
			true.
		] whileTrue: [
			(socket readWillNotBlockWithin: 1000) ifTrue: [
				self copy handleRequestOn: socket accept.
			].
		].
	] ensure: [
		socket close.
	].
%
category: 'Web Server'
set compile_env: 0
method: Server
startSecureServerAtPort: anInteger

	self setupSecureServerSocketAtPort: anInteger.
	server := [
		[
			[
				Processor yield.
				true.
			] whileTrue: [
				(socket readWillNotBlockWithin: 1000) ifTrue: [
					[:aServer :aSocket |
						aSocket isNil ifTrue: [
							self error: socket lastErrorString.
						].
						aServer handleSecureRequestOn: aSocket.
					] forkAt: Processor userBackgroundPriority
						with: (Array 
							with: self copy
							with: socket accept).
				].
			].
		] ensure: [
			socket close.
		].
	] forkAt: Processor userBackgroundPriority.
	^self serverURL.
%
category: 'Web Server'
set compile_env: 0
method: Server
startServerAtPort: anInteger

	self setupServerSocketAtPort: anInteger.
	server := [
		[
			[
				Processor yield.
				true.
			] whileTrue: [
				(socket readWillNotBlockWithin: 1000) ifTrue: [
					[:aServer :aSocket |
						aServer handleRequestOn: aSocket.
					] forkAt: Processor userBackgroundPriority
						with: (Array 
							with: self copy
							with: socket accept).
				].
			].
		] ensure: [
			socket close.
		].
	] forkAt: Processor userBackgroundPriority.
	^self serverURL.
%
category: 'Web Server'
set compile_env: 0
method: Server
stopServer

	server notNil ifTrue: [
		server terminate.
		server := nil.
	].
%
category: 'Json'
set compile_env: 0
method: Server
tools

	| list |
	list := WebTools select: [:each | each isClass and: [each ~~ Tool and: [each isSubclassOf: Tool]]].
	list := list reject: [:each | each sortOrder isNil].
	list := list asSortedCollection: [:a :b | a sortOrder <= b sortOrder].
	list := list asArray collect: [:each | 
		Dictionary new
			at: 'file' put: each fileName;
			at: 'name' put: each displayName;
			at: 'description' put: each description;
			yourself.
	].
	^Dictionary new
		at: #'tools'	put: list;
		yourself.
%
category: 'Request Handler'
set compile_env: 0
method: Server
translate: aString

	| readStream writeStream string |
	readStream := ReadStream on: aString.
	writeStream := WriteStream on: String new.
	[
		readStream atEnd not.
	] whileTrue: [
		| char |
		char := readStream next.
		char = $+ ifTrue: [
			writeStream space.
		] ifFalse: [
			char = $% ifTrue: [
				| array value |
				array := #($0 $1 $2 $3 $4 $5 $6 $7 $8 $9 $A $B $C $D $E $F).
				value := (array indexOf: readStream next) - 1 * 16 + (array indexOf: readStream next) - 1.
				writeStream nextPut: (Character codePoint: value).
			] ifFalse: [
				writeStream nextPut: char.
			].
		]
	].
	string := writeStream contents.
	string = 'null' ifTrue: [^nil].
	string = 'true' ifTrue: [^true].
	string = 'false' ifTrue: [^false].
	^string
%
doit
Server category: 'WebTools'
%
