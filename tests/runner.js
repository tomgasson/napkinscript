let path = require("path");
let fs = require("fs");
let cp = require("child_process");

let parser = path.join(process.cwd(), "./lib/napkinscript.exe");

function parseFile(filename, recover, env) {
  let args = ["-print", "ml"];
  if (recover) args.push("-recover");
  args.push(filename);
  return env ? cp.spawnSync(parser, args, {env}) : cp.spawnSync(parser, args);
}
// File "/home/travis/build/IwanKaramazow/napkinscript/tests/parsing/errors/scanner/oldDerefOp.js", line: 1, characters 4-5:
// test output contains the full path of the file
// this differs between multiple machines
// Just drop "/home/travis/build/IwanKaramazow" to make the file path machine independent
let makeReproducibleFilename = txt => {
  // "Parse error: "
  let lines = txt.split("\n");
  for (let i = 0; i < lines.length; i++) {
    let txt = lines[i];
    if (
      txt.indexOf("File") === -1 ||
      txt.indexOf("line") === -1 ||
      txt.indexOf("characters") === -1
    ) {
      continue;
    }
    let prefix = txt.substring(0, 6); // Keep `File "`-prefix
    let suffix = txt.substring(txt.indexOf("/napkinscript"), txt.length);
    lines[i] = prefix + suffix;
  }
  return lines.join("\n");
};

global.runParser = (dirname, recover = false, showError = false, env) => {
  fs.readdirSync(dirname).forEach(base => {
    let filename = path.join(dirname, base);
    if (!fs.lstatSync(filename).isFile() || base === "parse.spec.js") {
      return;
    }

    test(base, () => {
      let res = parseFile(filename, recover, env);
      let parsetree = res.stdout.toString();
      let output = "";
      if (showError) {
        output += `=====Parsetree==========================================\n`;
        output += `${parsetree}\n`;
        output += `=====Errors=============================================\n`;
        output += `${makeReproducibleFilename(res.stderr.toString())}\n`;
        output += `========================================================`;
      } else {
        output = parsetree;
      }

      expect(output).toMatchSnapshot();
    });
  });
};
