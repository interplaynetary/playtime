const fs = require('fs');
const path = require('path');

const readline = require('readline');
const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

const init = (Contexts) => {
  rl.on('line', async (input) => {
    if (input.startsWith('/')) {
      // Command mode
      const [command, ...args] = input.slice(1).split(' ');
      switch (command) {
        case 'start':
          if (args.length > 0) {
            Contexts.start(args[0]);
          } else {
            console.log('Usage: /start <context>');
          }
          break;
        case 'stop':
          if (args.length > 0) {
            Contexts.terminate(args[0]);
          } else if (Contexts.getActive()) {
            Contexts.terminate(Contexts.getActive());
          } else {
            console.log('No active context to stop. Usage: /stop [context]');
          }
          break;
        case 'list':
          console.log('Running contexts:', Array.from(Contexts.getRunning().keys()));
          break;
        case 'active':
          if (args.length > 0) {
            Contexts.setActive(args[0]);
          } else {
            console.log('Active context:', Contexts.getActive() || 'None');
          }
          break;
        case 'index':
          const availableContexts = await listAvailableContexts();
          if (availableContexts.length > 0) {
            console.log('Available contexts:');
            availableContexts.forEach(context => console.log(`- ${context}`));
          } else {
            console.log('No available contexts found.');
          }
          break;
        case 'exit':
          console.log('Goodbye!');
          process.exit(0);
          break;
        default:
          console.log('Unknown command');
      }
    } else if (Contexts.getActive()) {
      // Send input to active context
      const contextProcess = Contexts.getRunning().get(Contexts.getActive());
      if (contextProcess) {
        contextProcess.stdin.write(input + '\n');
      }
    } else {
      console.log('No active context. Use /active <context> to set an active context.');
    }
  });
}

async function listAvailableContexts() {
  try {
    const files = await fs.promises.readdir('contexts');
    const playFiles = files.filter(file => file.endsWith('.play'));
    const contexts = playFiles.map(file => path.basename(file, '.play'));
    return contexts;
  } catch (error) {
    console.error('Error reading contexts directory:', error);
    return [];
  }
}

function printInstructions() {
  console.log('\n--- Playtime Context Manager ---');
  console.log('Available commands:');
  console.log('/index            - List all available context files');
  console.log('/start <context>  - Start a new context');
  console.log('/stop [context]   - Stop a running context (stops active context if no argument given)');
  console.log('/list             - List all running contexts');
  console.log('/active [context] - Set or display the active context');
  console.log('/exit             - Exit the Playtime Context Manager');
  console.log('');
  console.log('To interact with the active context, simply type your input.');
  console.log('-----------------------------------\n');
}

module.exports = {
  init,
  printInstructions
};
