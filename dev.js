const chokidar = require('chokidar')
const { exec } = require('child_process')
const { join } = require('path')

chokidar.watch(join(__dirname, "./src/**/*.purs"), {
  awaitWriteFinish: true
}).on('change', (function () {
  let promise = Promise.resolve()
  let childProc = null
  return function () {
    if (childProc !== null) childProc.kill('SIGTERM')

    promise = promise.then(function () {
      return new Promise(function (resolve) {
        childProc = exec("npm run build")

        childProc.stdout.on('data', function (chunk) {
          global.process.stdout.write(chunk)
        })
        childProc.stderr.on('data', function (chunk) {
          global.process.stderr.write(chunk)
        })

        childProc.on('close', function () {
          childProc = null
          resolve()
        })
      })
    })
  }
})())

