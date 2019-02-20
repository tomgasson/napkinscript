let path = require("path");
let fs = require("fs");
let cp = require("child_process");

let parser = path.join(process.cwd(), "./_build/native.exe");

function parseFile(filename) {
  let cmd = `${parser} ${filename}\n`;
  return cp.execSync(cmd).toString();
}

global.runParser = dirname => {
  fs.readdirSync(dirname).forEach(base => {
    let filename = path.join(dirname, base);
    if (!fs.lstatSync(filename).isFile() || base === "parse.spec.js") {
      return;
    }

    test(base, () => {
      let parsetree = parseFile(filename);
      expect(parsetree).toMatchSnapshot();
    });
  });
};
