! Copyright (c) 2011-2013 GemTalk Systems LLC. All Rights Reserved.

doit
Object subclass: 'Server'
  instVarNames: #( server socket resultCode
                    httpHeaders stream startTime password)
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
	self startServerAtPort: 8080.
%
category: 'other'
set compile_env: 0
classmethod: Server
runInForeground
"
	Server runInForeground.
"
	self new 
		password: nil; "'swordfish';"
		startForegroundServerAtPort: 8080.
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
		GsFile stdout nextPutAll: string.
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
errorExpectedGetOrPost

	resultCode := 404.
	self htmlWithBody: 'Expected a GET or POST request!'.
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
	| readStream type string line headers contentLength path args selector class pieces |
	readStream := ReadStream on: (socket readString: 1000).
	type := readStream upTo: Character space.
	(type = 'GET' or: [type = 'POST']) ifFalse: [^self errorExpectedGetOrPost].
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
	password notNil ifTrue: [
		| line cookies cookie |
		line := headers at: 'Cookie' ifAbsent: [''].
		cookies := (line subStrings: $;)
			inject: Dictionary new
			into: [:dict :each | 
				| index key value |
				index := each indexOf: $=.
				key := (each copyFrom: 1 to: index - 1) trimBlanks.
				value := (each copyFrom: index + 1 to: each size) trimBlanks.
				dict at: key put: value.
				dict].
		cookie := cookies at: 'password' ifAbsent: [nil].
		cookie ~= password ifTrue: [
			^self handleRequestFor: '/Password.html'.
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
	(gsFile := GsFile openReadOnServer: '$WEBTOOLS' , aString) notNil ifTrue: [
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
category: 'Accessors'
set compile_env: 0
method: Server
password: aString

	password := aString.
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

	^'http://' , ((System descriptionOfSession: System session) at: 3) , ':' , socket port printString , '/'.
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
