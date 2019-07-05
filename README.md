WebTools for GemStone/S 64 Bit
========

WebTools provides a HTML-based user interface for a suite of tools for GemStone/S. To use WebTools, clone the code, set an environment variable, start Topaz, login (you may need to edit the user, password, and stone name), and install the code:

    git clone https://github.com/jgfoster/webtools.git
    cd webtools
    cp topazini .topazini #edit as appropriate
    ./start

At this point you should be able to launch a web browser and start interacting with the tools. Note that connection will be on HTTPS with a privately-issued certificate. Depending on your browser and local configuration, the initial connection might be troublesome.


Components
----------

WebTools depends on various third-party components that are licensed by their respective owners.

- CodeMirror v3.18 [web](http://codemirror.net/) [license](http://codemirror.net/LICENSE)
- Flot v0.8.1 [web](http://www.flotcharts.org/) [license](https://github.com/flot/flot/blob/master/LICENSE.txt)
- jjmenu v1.1.2 [web](http://jursza.net/dev/jjmenu/) [license](http://www.opensource.org/licenses/mit-license.php)
- jQuery v1.10.2 [web](jquery.com) [license](jquery.org/license)
- jQuery UI v1.10.3 [web](http://jqueryui.com/) [license](https://github.com/jquery/jquery-ui/blob/master/MIT-LICENSE.txt)
- jsTree v1.0-rc3 [web](http://www.jstree.com/) [license](http://www.opensource.org/licenses/mit-license.php)

Supported Client Browsers
-------------------------

WebTools has been developed and tested on Chrome (version 22) on Macintosh OS X 10.8.4 with GemStone/S 64 Bit 3.1.0.4. WebTools requires the HTML canvas API to draw statmonitor graphics. This means that it does not work on IE8 or earlier. Other modern broswers support the canvas API so should work (but has not been tested).

Hosting
-------

WebTools is hosted on [GitHub](https://github.com/jgfoster/webtools). You are invited to fork the repository, make changes, and submit a pull request. You may also submit an [issue](https://github.com/jgfoster/webtools/issues).

License
-------

WebTools components that are not otherwise licensed (see above) are Copyright (c) 2011-2013 by GemTalk Systems LLC and have their own [license](https://github.com/jgfoster/webtools/blob/master/licenses/WebTools.license). 
