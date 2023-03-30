//node .\build-service-worker.js
const {generateSW} = require('workbox-build');
const swDest = 'doc/service-worker.js';
generateSW({
  //swSrc: 'src-js/service-worker.js',
  swDest,
  globDirectory: 'doc',
  // Other configuration options...
}).then(({count, size}) => {
  console.log(`Generated ${swDest}, which will precache ${count} files, totaling ${size} bytes.`);
});