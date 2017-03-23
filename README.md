
Got tired of TeamCity's broken search box, so I made my own in PureScript.


## Running tc-ui
```
$ npm i
$ bower i
$ npm run build

//or
$ npm run watch
```

## Running tc-proxy
```
$ npm i
$ bower i
$ pulp run
```

To host tc-proxy in IIS:

* Install [iisnode][0]
* Install [URL rewrite module for IIS][1]
* Create an IIS application pointing to the `tc-proxy\iis` folder
* Make sure you have a version of Node.JS that contains [this bug fix][3]; e.g. [this nightly][2].


 [0]: https://github.com/tjanczuk/iisnode#hosting-nodejs-applications-in-iis-on-windows
 [1]: https://www.iis.net/downloads/microsoft/url-rewrite
 [2]: https://nodejs.org/download/nightly/v8.0.0-nightly20170321ab2d49bcac/
 [3]: https://github.com/nodejs/node/issues/11656#event-1007393962

