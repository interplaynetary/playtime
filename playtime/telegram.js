const { Bot } = require("grammy");
const express = require('express');
require('dotenv').config();

const telegramApiToken = process.env.TELEGRAM_API_TOKEN;
const bot = new Bot(telegramApiToken);
const app = express();
const port = 3000;

// Add this middleware to log all incoming requests
app.use((req, res, next) => {
  console.log(`Received ${req.method} request to ${req.path}`);
  console.log('Query parameters:', req.query);
  console.log('Body:', req.body);
  next();
});

// Add this new endpoint near the top of your Express routes
app.get('/hello', (req, res) => {
  console.log('Received GET request to /hello');
  res.status(200).send('Hello');
});

// Store pending requests
const pendingRequests = new Map();

// Endpoint for Guile to send messages to players
app.post('/send-message', (req, res) => {
  console.log('Processing /send-message request');
  const { userId, content } = req.query;
  bot.api.sendMessage(userId, content)
    .then(() => {
      console.log('Message sent successfully');
      res.status(200).send('Message sent');
    })
    .catch(error => {
      console.error('Error sending message:', error);
      res.status(500).send('Error sending message: ' + error.message);
    });
});

// Endpoint for Guile to request input from a player
app.get('/request-input', async (req, res) => {
  console.log('Processing /request-input request');
  const { userId, content } = req.query;
  
  try {
    await bot.api.sendMessage(userId, content);
    console.log('Message sent, waiting for user response');
    
    const responsePromise = new Promise((resolve, reject) => {
      pendingRequests.set(userId, { resolve, reject });
      
      // Set a timeout for the request (e.g., 5 minutes)
      setTimeout(() => {
        if (pendingRequests.has(userId)) {
          pendingRequests.delete(userId);
          console.log('Request timed out for user:', userId);
          reject(new Error('Request timed out'));
        }
      }, 5 * 60 * 1000);
    });

    const response = await responsePromise;
    console.log('Received user response:', response);
    res.json(response);
  } catch (error) {
    console.error('Error in /request-input:', error);
    res.status(500).send('Error: ' + error.message);
  }
});

// Handle incoming messages from Telegram
bot.on("message:text", async (ctx) => {
  const userId = ctx.message.from.id.toString();
  const text = ctx.message.text;

  console.log('Received message from Telegram:', { userId, text });

  if (pendingRequests.has(userId)) {
    const { resolve } = pendingRequests.get(userId);
    pendingRequests.delete(userId);
    resolve({ content: text });
    console.log('Resolved pending request for user:', userId);
  }
});

// Start the Express server
app.listen(port, () => {
  console.log(`Server listening at http://localhost:${port}`);
});

// Start the bot (using long polling)
bot.start();