const { Bot } = require("grammy");
const net = require('net');
const fs = require('fs');
require('dotenv').config();

const telegramApiToken = process.env.TELEGRAM_API_TOKEN;
const bot = new Bot(telegramApiToken);

let player = { id: 725085107, first_name: "Fronx", username: "fronx84" };
// bot.api.sendMessage(player.id, "Hey " + player.first_name + ", it's playtime!")
let awaitingResponse = false;

const SOCKET_PATH = '/tmp/guile_js_socket';

// Create server to listen for messages from Guile
const server = net.createServer((socket) => {
  console.log('Guile connected');

  socket.on('data', (data) => {
    const message = JSON.parse(data.toString());
    console.log('Received from Guile:', message);

    if (message.type === 'cue') {
      if (player) {
        bot.api.sendMessage(player.id, message.content);
      } else {
        console.log('No player connected yet');
      }
    } else if (message.type === 'request') {
      if (player) {
        awaitingResponse = true;
        bot.api.sendMessage(player.id, message.content);
      } else {
        console.log('No player connected yet');
      }
    }
  });
});

// Remove existing socket file if it exists
if (fs.existsSync(SOCKET_PATH)) {
  fs.unlinkSync(SOCKET_PATH);
}

server.listen(SOCKET_PATH, () => {
  console.log('Server listening on', SOCKET_PATH);
});

// Function to send messages to Guile
function sendToGuile(message) {
  const client = net.createConnection(SOCKET_PATH, () => {
    client.write(JSON.stringify(message));
    client.end();
  });
  client.on('error', (err) => {
    console.error('Failed to send to Guile:', err.message);
  });
}

bot.on("message:text", (ctx) => {
  if (!player) {
    player = ctx.update.message.from;
    bot.api.sendMessage(player.id, "Hey " + player.first_name + ", it's playtime!");
    sendToGuile({ type: 'player_connected', name: player.first_name });
  } else if (awaitingResponse) {
    sendToGuile({ type: 'response', content: ctx.message.text });
    awaitingResponse = false;
  } else {
    bot.api.sendMessage(player.id, "I'm waiting for the game to give me instructions.");
  }
});

// Start the bot (using long polling)
bot.start();








// {
//   "update": {
//     "update_id": 787119119,
//     "message": {
//       "message_id": 7,
//       "from": {
//         "id": 725085107,
//         "is_bot": false,
//         "first_name": "Fronx",
//         "username": "fronx84",
//         "language_code": "en"
//       },
//       "chat": {
//         "id": 725085107,
//         "first_name": "Fronx",
//         "username": "fronx84",
//         "type": "private"
//       },
//       "date": 1725484467,
//       "text": "whoop"
//     }
//   },
//   "api": {
//     "token": "6529984216:AAEJfa4UB2ge_rB2AzXMkz28q3uytPnQp5U",
//     "raw": { },
//     "config": { }
//   },
//   "me": {
//     "id": 6529984216,
//     "is_bot": true,
//     "first_name": "playtime",
//     "username": "the_playtime_bot",
//     "can_join_groups": true,
//     "can_read_all_group_messages": false,
//     "supports_inline_queries": true,
//     "can_connect_to_business": false,
//     "has_main_web_app": false
//   }
// }