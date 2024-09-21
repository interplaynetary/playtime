const { spawn } = require('child_process');

const runningContexts = new Map();
let activeContext = null;

function start(context) {
  if (runningContexts.has(context)) {
    console.log(`Context ${context} is already running.`);
    return;
  }

  console.log(`Starting context: ${context}`);
  const contextProcess = spawn('guile', ['--fresh-auto-compile', '-s', 'playtime.scm', `contexts/${context}.play`]);

  contextProcess.stdout.setEncoding('utf8');
  contextProcess.stdout.on('data', (data) => {
    console.log(`[${context}] ${data.toString().trim()}`);
  });

  contextProcess.stderr.on('data', (data) => {
    console.error(`[${context}] stderr: ${data}`);
  });

  contextProcess.on('close', (code) => {
    console.log(`[${context}] process exited with code ${code}`);
    runningContexts.delete(context);
    if (activeContext === context) {
      activeContext = null;
    }
  });

  runningContexts.set(context, contextProcess);
  setActive(context);
}

function terminate(context) {
  if (runningContexts.has(context)) {
    console.log(`Terminating context: ${context}`);
    if (activeContext === context) {
      activeContext = null;
    }
    runningContexts.get(context).kill();
    runningContexts.delete(context);
  } else {
    console.log(`Context ${context} is not running`);
  }
}

function terminateAll() {
  console.log('Terminating all contexts');
  for (const [context, process] of runningContexts) {
    process.kill();
    console.log(`Terminated context: ${context}`);
  }
  runningContexts.clear();
}

function setActive(context) {
  if (runningContexts.has(context)) {
    activeContext = context;
    console.log(`Active context set to: ${context}`);
  } else {
    console.log(`Context ${context} is not running`);
  }
}

function getActive() {
  return activeContext;
}

function getRunning() {
  return runningContexts;
}

module.exports = {
  start,
  terminate,
  terminateAll,
  setActive,
  getRunning,
  getActive
};
