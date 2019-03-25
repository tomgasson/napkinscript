let path = require("path");
let fs = require("fs");
let cp = require("child_process");

let parser = path.join(process.cwd(), "./lib/napkinscript.exe");

function parseFile(filename, recover) {
  let args = [];
  if (recover) args.push("-recover");
  args.push(filename);
  return cp.spawnSync(parser, args).stdout.toString();
}

global.runParser = (dirname, recover = false) => {
  fs.readdirSync(dirname).forEach(base => {
    let filename = path.join(dirname, base);
    if (!fs.lstatSync(filename).isFile() || base === "parse.spec.js") {
      return;
    }

    test(base, () => {
      let parsetree = parseFile(filename, recover);
      expect(parsetree).toMatchSnapshot();
    });
  });
};
