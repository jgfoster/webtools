/*
	webtools.js
	
	Copyright (c) 2011-2013 GemTalk Systems LLC. All Rights Reserved.
*/

GemStone = function() {	//	hide everything inside an anonymous function to isolate namespace
	var requestCount = 0,	//	keep a cumulative count of round-trips to server
		tools = {},
		$tabs,
		$statusBar,
		nextId = 0,
		scripts = {};
	
	$(document).ready(onDocumentReady);

	return {
		activateLastTab: 		activateLastTab,	//	adding a tab doesn't select it
		addTab:					addTab,				//	add a new tab panel
		ajax:					ajax,				//	wrapper for server call
		browseImplementorsOf: 	browseImplementorsOf,
		browseReferencesTo: 	browseReferencesTo,
		browseSendersOf:		browseSendersOf,
		loadCSS:				loadCSS,			//	add CSS link to head if not already present
		menu:					menu,				//	popup a dynamic context menu
		nextId:					getNextId,			//	unique HTML element identifier
		runJs:					runJs,				//	run a script
		runJsOnce:				runJsOnce,			//	run a script only if not yet run
		saveScript:				saveScript,			//	cache function to reuse without reloading
		scroll:					scroll,				//	add scrolling to a table
		encodeHTML:				encodeHTML,			//	escape for HTML display
		decodeHTML:				decodeHTML			//	unescape HTML code
	}
	
	function onDocumentReady() {
		$tabs = $('#tabs');
		$statusBar = $('#statusBar');
		$tabs.tabs({ activate: showTab });	//	create main tabs
		ajax('GET', 'tools', null, gotTools);
		$(window).resize(resizeWindow);		//	register to get resize events
		createConsole();
		$('body').append('<span class="ruler" style="display: inline-block;"></span>');
		return;
		
		function createConsole() {
			var alertFallback = true;
			if (typeof console === "undefined" || typeof console.log === "undefined") {
				console = {};
				if (alertFallback) {
					console.log = function(msg) {
						alert(msg);
					};
				} else {
					console.log = function() {};
				}
			}
		}

		function gotTools(json) { 
			var items = [];
			$.each(json.tools, function(){
				items.push('<tr class="clickable" title="' + this.file + '">');
				items.push('<td>' + this.name + '</td>');
				items.push('<td>' + this.description + '</td>');
				items.push('</tr>');
			});
			$('#homePanel tbody').empty().append(items.join(''));
			$('#homePanel tbody tr').click(onClick);
			return;

			function onClick(event) {
				event.preventDefault();
				ajax(
					'GET'
				,	$(this).attr('title')
				,	null
				,	function(html) { $('body').append("<div class='temp'>" + html + "</div>"); } 
				);
			}
		}
		
		function showTab(event, ui) { 
			var list = $('.scrollingTable', ui.newPanel),
				tool = ui.newPanel.data('tool');
			if (0 < list.length) {
				resizeTable(list[0]);
			}
			if ( tool && tool.onShow ) {
				tool.onShow($tab[0]);
			}
		}
	}

	function activateLastTab() {
		$tabs.tabs( "option", "active", -1 );
	}
	
	// addTab() is called by the home tab when the user clicks on a row
	// tool is an object with an id, label, title, and an onAdd() function
	// at this point a new div (class="temp") will have been added to to the body 
	// The new div should have a child with an id of tool.id
	function addTab(tool) {
		var destinationId = getNextId();		// each tab needs a unique id
		var sourceId = destinationId + 'a';
		// create the HTML for the tab and panel
		var tabHTML = "<li id='" + destinationId + "t'>" + 
						"<a href='#" + destinationId + "'>" + tool.label + "</a></li>";
		var panelHTML = "<div id='" + destinationId + "' class='hidden'></div>";
		// change the id of the new div
		$('#' + tool.id).attr('id', sourceId);
		// move any singleton divs in the recently-loaded code
		moveOnlyOnce($('#' + sourceId));
		$(tabHTML).appendTo($(".ui-tabs-nav", $tabs));
		$(panelHTML).appendTo("#tabs");
		$tabs.tabs("refresh");
		$('#' + destinationId).addClass($('#' + sourceId).attr('class'));
		$('#' + sourceId).children().each(function() {
			var clone = $(this).clone();
			$(clone).appendTo('#' + destinationId);
		});
		// remove the thing added by the click (having saved everything of value)
		$(".temp").remove();
		var $listItem = $('> ul > li:last', $tabs);
		$listItem.attr('title', tool.title);
		addCloseButton($listItem);
		$('#' + destinationId).data('tool', tool);
		// give time to fill the tab, then show it
		if (tool.onAdd) { tool.onAdd($('#' + destinationId)); }
		return;

		function addCloseButton($listItem) {
			$listItem.append('<a href="#" class="closeTab">X</a>');
			$('a.closeTab', $listItem).click(removeTab);
			return;
			
			function removeTab(event) {
				var list = $listItem.parent().children('li')
				,	index = list.index($listItem)
				;
				$('#' + destinationId).remove();
				$('#' + destinationId + "t").remove();
				$tabs.tabs( "option", "active", 0);
				$tabs.tabs("refresh");
				event.preventDefault();
			}
		};

		// There are some parts of a tab that we want to exist only once (e.g. css)
		// $tabPanel is the newly-loaded div that might have a singleton in it
		function moveOnlyOnce($tabPanel) {
			var	$div = $('.once', $tabPanel),	// find the singleton div (if any)
				newClass = $div.attr('id');		// use its id for its class
			$div.addClass(newClass);
			// if the singleton exists only once (i.e., is new), then save it
			if (1 === $('.' + newClass).length) {
				$div.attr('id', null);			// clear the id (we will use the class)
				// move the style element to the head
				$('style', 'head').append($('style', $div).html());
				$('style', $div).remove();
				$('body').append($div);
			}
		}
	}
	
	function ajax(type, url, args, callback) { 
		var startTime = new Date().getTime();
		$statusBar.text('Sent request #' + (++requestCount) + ' for ' + url);
		$.ajax({
			type: type,
			url: url,
			data: args,
			success: success
		});
/*
		console.log([type, url, args]);
		if (type === 'GET' && args) {
			var delimiter = '?'
			,	items = new Array();
			items.push(url);
			$.each(args, function(key, value) { 
				items.push(delimiter);
				items.push(key);
				items.push('=');
				items.push(value);
				delimiter = '&';
			});
			history.pushState(null, '', items.join(''));
		}
*/
		return;
		
		function success(json) {
			var serverTime = json._time;
			var networkTime = new Date().getTime() - startTime - serverTime;
			startTime = new Date().getTime();
			var error = json._error;
			if (error) {
				alert(error + ' (see console log for stack)');
				console.log(error + '\r\n' + json._stack);
			} else {
				if (callback) { (callback)(json); }
			};
			var elapsed = new Date().getTime() - startTime;
			var string = 'Request for ' + url + ' (roundtrip #' + requestCount + ') took ' +
				serverTime + ' ms on server, ' + networkTime + ' ms on the network, and ' 
				+ elapsed + ' ms on the client.'
			console.log(string);
			$statusBar.text(string);
		};
	}

	function browseMethods(data) {
		ajax(
			'GET',
			'MethodList.html', 
			null,
			function(html) {
				var string = html.replace('SEARCH_OBJECT', JSON.stringify(data));
				$('body').append(string);
			} 
		);
	}
	
	function browseImplementorsOf(anObject) {
		var name = anObject ? anObject : prompt('Browse implementors of?');
		if (!name) { return; }
		browseMethods({
			type: 'implementors',
			find: name,
			label: 'Implementors of ' + name
		});
	}
	
	function browseReferencesTo(dict, klass) {
		browseMethods({
			type: 'referencesToGlobal',
			dict: dict,
			find: klass,
			label: 'References to ' + klass
		});
	}

	function browseSendersOf(anObject) {
		var name = anObject ? anObject : prompt('Browse senders of?');
		if (!name) { return; }
		browseMethods({
			type: 'senders',
			find: name,
			label: 'Senders of ' + name
		});
	}
	
	function getNextId() {
		nextId = nextId + 1;
		return 'id' + nextId;
	}

	function loadCSS(href) {
		if ($('head link[href="' + href + '"]')[0]) { return; }
		$('head').append('<link rel="stylesheet" type="text/css" href="' + href + '"/>');
	}
	
	function menu(options) {
		options.selector.jjmenu(
			'rightClick',
			[ { 
				getByFunction: function() {
					var items = options.menu(triggerElement);
					$.each(items, function(index, each) {
						each.action = { 
							type: 'xfn'
						,	callback: each.action
						};
					});
					return items;
				}
			} ],
			null,	//	userData
			{ xposition: 'mouse', yposition: 'mouse' }
		);
	}

	function saveScript(src, aFunction) {
		scripts[src] = aFunction;
	}
	
	//	runJs(sourcePath)
	//	runJs(sourcePath, arguments)
	//	runJs(sourcePath, successFunction)
	//	runJs(sourcePath, arguments, successFunction)
	function runJs(src, arg1, arg2) {
		var args,
			done,
			result;
		if (typeof arg1 === 'function') {
			done = arg1;
		} else {
			args = arg1;
			done = arg2;
		}
		if (typeof scripts[src] === 'function') {
			result = scripts[src](args);
			if (done) { done(result); }
		} else {
			scripts[src] = null;
			runJsOnce(src, almostDone);
		}
		return;

		function almostDone() {
			if (typeof scripts[src] === 'function') {
				result = scripts[src](args);
			}
			if (done) { done(result); }
		}
	}
	
	function runJsOnce(src, done) {
		if (scripts[src]) { 
			if (done) { done(null); }
		} else {
			scripts[src] = true;
			$.getScript(src, function() { done(null); });
		}
	}
	
	function scroll($div) {
		if ( 1 === $('table', $div).length ) { 		// start with one table
			var table = $('table', $div).html();	// pull out the table HTML
			// create two tables, one to have header, other to have body
			// this is so the body can auto-size and the header can be fixed to it
			$div.empty().append(
				'<div class="tableHeader"><table>' + table + '</table></div>' +
				'<div class="tableBody"><table>' + table + '</table></div>'
				);
//			$('.tableHeader tbody', $div).remove();
			$('.tableBody caption', $div).remove();
			$div.addClass('scrollingTable');
		}
	}
	
	function encodeHTML(string) {
		return $('<div/>').text(string).html();
	}
	
	function decodeHTML(string) {
		return $('</div/>').html(string).text();
	}
	
	function resizeWindow() {
		var $tables = $('.ui-tabs-panel[aria-hidden=false] .scrollingTable');
		$.each($tables, function() { 
			resizeTable(this);
		});
	}
	
	function resizeTable($table) {
		var	$header       = $('.tableHeader', $table),
			$body         = $('.tableBody', $table),
			captionHeight = $('thead', $header).outerHeight(),
			headerHeight  = $('thead', $header).outerHeight() + 5;
		$header.css('height', headerHeight + captionHeight + 'px');
		$body.css('top', captionHeight + 2 + 'px');
		$('tbody', $header).html($('tbody', $body).html());
	}
}();
