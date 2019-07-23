#!/usr/bin/env node

const Module = require('../src/Cli.js')

const fs = require('fs')
const process = require('process')

const mainModule = process.argv[2]

var path = process.argv[3]

if (!path) {
	path = "./"
} else {
	path = "./" + path	
}

const mainModuleContent = fs.readFileSync(mainModule).toString()

var modules = [mainModuleContent];

filenames = fs.readdirSync(path);

filenames.forEach( (filename,index) => {
	// todo: recursion for directories	
	if (fs.lstatSync(path+filename).isFile() && filename !== mainModule && filename.endsWith('.elm')) {	
		let content = fs.readFileSync(path + filename, 'utf-8');
		if (content) {
			modules.push(content);
		}
	}	
});

run(modules);

function run (moduleList) {
  const flags = moduleList
  const generate = Module.Elm.Cli.init
  const process = generate({flags})
  process.ports.done.subscribe(x => console.log(x))
}
