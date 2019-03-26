let path = require("path");
let fs = require("fs");
let cp = require("child_process");

let parser = path.join(process.cwd(), "./lib/napkinscript.exe");

function parseFile(filename, recover) {
  let args = [];
  if (recover) args.push("-recover");
  args.push(filename);
  return cp.spawnSync(parser, args);
}

global.runParser = (dirname, recover = false, showError = false) => {
  fs.readdirSync(dirname).forEach(base => {
    let filename = path.join(dirname, base);
    if (!fs.lstatSync(filename).isFile() || base === "parse.spec.js") {
      return;
    }

    test(base, () => {
      let res = parseFile(filename, recover);
      let parsetree = res.stdout.toString();
      let output = "";
      if (showError) {
        output += `=====Parsetree==========================================\n`;
        output += `${parsetree}\n`;
        output += `=====Errors=============================================\n`;
        output += `${res.stderr.toString()}\n`;
        output += `========================================================`;
      } else {
        output = parsetree;
      }

      expect(output).toMatchSnapshot();
    });
  });
};
