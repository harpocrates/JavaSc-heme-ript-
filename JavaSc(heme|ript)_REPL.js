#!/usr/bin/env node

var fs          = require('fs')
    repl        = require('repl'),
    colors      = require('colors'),
    interpreter = require('./JavaSc(heme|ript).js');

interpreter.colors = true;

// Load standard functions and other files. A '!' at the end of the filename
// indicates the line by line output will not be displayed

var queued_files = ['/run/media/alec/FILES/alec\'s documents/School/14-15/Mine/JavaSc(heme|ript)/Standard_Functions.jscm!'].concat(process.argv.slice(2));
process.stdout.write('JavaSc(heme|ript) 2.2\n');
console.log('Loading '+queued_files.join(', ')+'...');
queued_files.forEach(function (filename) {
  show_output = filename.slice(-1) !== '!';
  if (!show_output)
    filename = filename.slice(0,-1);
  var file = fs.readFileSync(filename, 'utf8').split('\n'), parsed;
  for (var i in file)
    if ((parsed = interpreter.read_line(file[i])) != null) {
      result = interpreter.evaluate(parsed);
      if (show_output && result !== undefined)
        console.log(interpreter.to_string(result));
    }
});

// REPL

repl.start({
  prompt: '>',
  ignoreUndefined: true,
  eval: function (cmd, context, filename, callback) {
    var chunk = cmd.slice(1,-2), parsed;
    if (chunk !== "")
      if ((parsed = interpreter.read_line(chunk+' ')) !== null){
        parsed = interpreter.evaluate(parsed);
        callback(null,parsed);
      }
      else process.stdout.write('>'.dim);
  },
  writer: interpreter.to_string
});