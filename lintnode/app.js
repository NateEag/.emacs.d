/* HTTP interface to JSLint.

   Takes roughly half the time to jslint something with this than to
   start up a new rhino instance on every invocation.

   Invoke from bash script like:

     curl --form source="<${1}" --form filename="${1}" ${JSLINT_URL}
     
   or use the provided jslint.curl
   
     jslint.curl <file>

*/

/*global process, require */
var express = require("express");
var JSLINT = require('./fulljslint');
var fs = require('fs');
var _ = require('underscore');

var app = express.createServer();

app.configure(function () {
    app.use(express.errorHandler(
        { dumpExceptions: true, showStack: true }));
    app.use(express.bodyParser());
});

var jslint_port = 3003;

/* copied from jslint's rhino.js */
var jslint_options = {
    bitwise: true,
    eqeqeq: true,
    immed: true,
    newcap: true,
    nomen: true,
    onevar: true,
    plusplus: true,
    regexp: true,
    rhino: true,
    undef: true,
    white: true
};

var outputErrors = function (errors) {
    var e, i, output = [];
    // debug("Handling " + errors.length + "errors" + '\n');
    function write(s) {
        output.push(s + '\n');
    }
    /* This formatting is copied from JSLint's rhino.js, to be compatible with
       the command-line invocation. */
    for (i = 0; i < errors.length; i += 1) {
        e = errors[i];
        if (e) {
            write('Lint at line ' + e.line + ' character ' +
                        e.character + ': ' + e.reason);
            write((e.evidence || '').replace(/^\s*(\S*(\s+\S+)*)\s*$/, "$1"));
            write('');
        }
    }
    return output.join('');
};

app.get('/', function (req, res) {
    res.send('lintnode');
});

app.post('/jslint', function (req, res) {
    function doLint(sourcedata) {
        var passed, results;
        passed = JSLINT.JSLINT(sourcedata, jslint_options);
        if (passed) {
            results = "jslint: No problems found in " + req.body.filename + "\n";
        } else {
            results = outputErrors(JSLINT.JSLINT.errors);
        }
        return results;
    }
    res.send(doLint(req.body.source), {'Content-Type': 'text/plain'});
});

/* This action always return some JSLint problems. */
var exampleFunc = function (req, res) {
    JSLINT.JSLINT("a = function(){ return 7 + x }()",
        jslint_options);
    res.send(outputErrors(JSLINT.JSLINT.errors),
        {'Content-Type': 'text/plain'});
};

app.get('/example/errors', exampleFunc);
app.post('/example/errors', exampleFunc);

/* This action always returns JSLint's a-okay message. */
app.post('/example/ok', function (req, res) {
    res.send("jslint: No problems found in example.js\n",
        {'Content-Type': 'text/plain'});
});

function parseCommandLine() {
    var port_index, exclude_index, exclude_opts, include_index, include_opts, set_index, set_opts, set_pair, properties;
    port_index = process.argv.indexOf('--port');
    exclude_index = process.argv.indexOf('--exclude');
    include_index = process.argv.indexOf('--include');
    set_index = process.argv.indexOf('--set');
    if (port_index > -1) {
        jslint_port = process.argv[port_index + 1];
    }
    if (exclude_index > -1) {
        exclude_opts = process.argv[exclude_index + 1].split(",");
        if (exclude_opts.length > 0 && exclude_opts[0] !== '') {
            _.each(exclude_opts, function (opt) {
                jslint_options[opt] = false;
            });
        }
    }
    if (include_index > -1) {
        include_opts = process.argv[include_index + 1].split(",");
        if (include_opts.length > 0 && include_opts[0] !== '') {
            _.each(include_opts, function (opt) {
                jslint_options[opt] = true;
            });
        }
    }
    if (set_index > -1) {
        set_opts = process.argv[set_index + 1].split(",");
        if (set_opts.length > 0 && set_opts[0] !== '') {
            _.each(set_opts, function (opt) {
                if (opt.indexOf(":") > -1) {
                    set_pair = opt.split(":");
                    if (set_pair[1] === "true") {
                        set_pair[1] = true;
                    } else if (set_pair[1] === "false") {
                        set_pair[1] = false;
                    }
                    jslint_options[set_pair[0]] = set_pair[1];
                } else {
                    jslint_options[opt] = true;
                }
            });
        }
    }
    properties = "";
    _.each(jslint_options, function (value, opt) {
        properties = properties + opt + ": " + value + ", ";
    });
    return properties.substring(0, properties.length-2);
}

process.on('SIGINT', function () {
    console.log("\n[lintnode] received SIGINT, shutting down");
    process.exit(0);
});

console.log("[lintnode]", parseCommandLine());
app.listen(jslint_port, function () {
    console.log("[lintnode] server running on port", jslint_port);
});