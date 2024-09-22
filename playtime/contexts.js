const { spawn } = require('child_process');

const runningContexts = new Map();
let activeContext = null;

function start(context) {
  return new Promise((resolve, reject) => {
    if (runningContexts.has(context)) {
      console.log(`Context ${context} is already running.`);
      resolve(runningContexts.get(context));
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

    contextProcess.on('error', (error) => {
      console.error(`[${context}] Failed to start process: ${error}`);
      reject(error);
    });

    // Set a timeout to check if the process is still running after a short delay
    setTimeout(() => {
      if (contextProcess.exitCode === null) {
        runningContexts.set(context, contextProcess);
        setActive(context);
        resolve(contextProcess);
      } else {
        reject(new Error(`Context ${context} failed to start`));
      }
    }, 1000); // Wait for 1 second to check if the process is still running

    contextProcess.on('close', (code) => {
      console.log(`[${context}] process exited with code ${code}`);
      runningContexts.delete(context);
      if (activeContext === context) {
        activeContext = null;
      }
    });
  });
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

const deliverPlayerMessage = async (contextName, username, message) => {
  try {
    let contextProcess = runningContexts.get(contextName);
    if (!contextProcess) {
      contextProcess = await start(contextName);
    }

    const formattedMessage = `${message} ${username}\n`;
    contextProcess.stdin.write(formattedMessage);

    // Listen for a response from the process
    return new Promise((resolve, reject) => {
      const onData = (data) => {
        contextProcess.stdout.removeListener('data', onData);
        resolve(data.toString().trim());
      };

      const onError = (error) => {
        contextProcess.stderr.removeListener('data', onError);
        reject(new Error(`[${contextName}] Error: ${error.toString().trim()}`));
      };

      contextProcess.stdout.on('data', onData);
      contextProcess.stderr.on('data', onError);
    });
  } catch (error) {
    throw new Error(`[${contextName}] Failed to deliver message. ${error.message}`);
  }
};

module.exports = {
  start,
  terminate,
  terminateAll,
  setActive,
  getRunning,
  getActive,
  deliverPlayerMessage
};
