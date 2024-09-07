const { Bot } = require("grammy");
const express = require('express');
const fs = require('fs');
const csv = require('csv-parser');
const fastcsv = require('fast-csv');
require('dotenv').config();

const telegramApiToken = process.env.TELEGRAM_API_TOKEN;
const bot = new Bot(telegramApiToken);
const app = express();
const port = 3000;

// This is a super awkward workaround to fix the duplicate
// Content-Type header issue caused by Guile's http-post.
// I've tried everything. We must set the Content-Type to
// application/json, but Guile always adds text/plain;charset=utf-8
// as a duplicate header.
const path = require('path');
const filterDuplicateContentType = require(path.join(__dirname, 'filterheaders.js'));
app.use(filterDuplicateContentType);

// This is necessary to read the body, otherwise it appears
//as empty.
app.use(express.json());

// Add this middleware to log all incoming requests
app.use((req, res, next) => {
  // console.log(`Received ${req.method} request to ${req.path}`);
  // console.log('Query parameters:', req.query);
  // console.log('Raw Headers:', req.rawHeaders);
  // console.log('Body:', req.body);
  // console.log(req);
  next();
});

// Add this new endpoint near the top of your Express routes
app.get('/hello', (req, res) => {
  console.log('Received GET request to /hello');
  res.status(200).send('Hello');
});

// Store pending requests
const pendingRequests = new Map();

// User database
const users = new Map();
const USER_DB_FILE = 'users.csv';

function ensureUserDBExists() {
  if (!fs.existsSync(USER_DB_FILE)) {
    fs.writeFileSync(USER_DB_FILE, 'username,id,name');
    console.log('Created new users.csv file');
  }
}

function loadUsers() {
  ensureUserDBExists();
  fs.createReadStream(USER_DB_FILE)
    .pipe(csv())
    .on('data', (row) => {
      users.set(row.username, { username: row.username, id: row.id, name: row.name });
    })
    .on('end', () => {
      console.log('Users loaded from CSV file');
    });
}

function saveUser(user) {
  const writer = fs.createWriteStream(USER_DB_FILE, { flags: 'a' });
  writer.write('\n'); // Add a new line before writing the new user
  fastcsv.write([user], { headers: false }).pipe(writer);
}

function checkAndAddUser(userId, name, username) {
  if (!username) {
    console.log('User has no username, skipping database entry');
    return;
  }
  if (!users.has(username)) {
    const newUser = { username, id: userId, name };
    users.set(username, newUser);
    saveUser(newUser);
    console.log('New user added:', newUser);
  }
}

loadUsers();

app.get('/find-user-by-username/:username', (req, res) => {
  const { username } = req.params;
  // console.log(`Searching for user with username: ${username}`);

  const user = users.get(username);
  if (user) {
    // console.log(`User found: ${JSON.stringify(user)}`);
    res.json(user);
  } else {
    console.log(`User not found for username: ${username}`);
    res.status(404).json({ error: 'User not found' });
  }
});

// Endpoint for Guile to send messages to players
app.post('/send-message', (req, res) => {
  console.log('Processing /send-message request');
  const { userId, text } = req.body;
  console.log('Sending to user:', userId, 'text:', text);
  bot.api.sendMessage(userId, text)
    .then(() => {
      console.log('Message sent successfully');
      res.status(200).json({ message: 'Message sent' });
    })
    .catch(error => {
      console.error('Error sending message:', error);
      res.status(500).json({ error: error.message });
    });
});

// Updated endpoint for Guile to request input from a player
app.post('/request-input', async (req, res) => {
  console.log('Processing /request-input request');
  const { userId, text } = req.body;

  if (!userId || !text) {
    return res.status(400).json({ error: 'Missing userId or text in request body' });
  }

  try {
    await bot.api.sendMessage(userId, text);
    console.log('Message sent, waiting for user response');

    const responsePromise = new Promise((resolve, reject) => {
      pendingRequests.set(userId, { resolve, reject });
    });

    const response = await responsePromise;
    console.log('Received user response:', response);
    res.json(response);
  } catch (error) {
    console.error('Error in /request-input:', error);
    res.status(500).json({ error: error.message });
  }
});

// Handle incoming messages from Telegram
bot.on("message:text", async (ctx) => {
  const userId = ctx.message.from.id.toString();
  const text = ctx.message.text;
  // TODO: handle photo/video/voice messages
  const name = `${ctx.message.from.first_name} ${ctx.message.from.last_name || ''}`.trim();
  const username = ctx.message.from.username || '';

  console.log('Received message from Telegram:', { userId, text, username });

  // Check and add user to the database
  checkAndAddUser(userId, name, username);

  if (pendingRequests.has(userId)) {
    const { resolve } = pendingRequests.get(userId);
    pendingRequests.delete(userId);
    // TODO: handle photo/video/voice messages
    resolve({ text: text });
    console.log('Resolved pending request for user:', userId);
  }
});

// Start the Express server
app.listen(port, () => {
  console.log(`Server listening at http://localhost:${port}`);
});

// Start the bot (using long polling)
bot.start();