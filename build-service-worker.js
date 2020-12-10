//node .\build-service-worker.js
const {generateSW} = require('workbox-build');
const swDest = 'yetipad/service-worker.js';
generateSW({
  //swSrc: 'src-js/service-worker.js',
  swDest,
  globDirectory: 'yetipad',
  // Other configuration options...
}).then(({count, size}) => {
  console.log(`Generated ${swDest}, which will precache ${count} files, totaling ${size} bytes.`);
});