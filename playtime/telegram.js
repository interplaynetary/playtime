require('dotenv').config();

const { Bot, GrammyError, HttpError } = require("grammy");
const { hydrateFiles } = require("@grammyjs/files");
const { Menu } = require("@grammyjs/menu");
const { session } = require("grammy");
const express = require('express');

const path = require('path');
const requireRelative = (modulePath) => require(path.join(__dirname, modulePath));

const Users = requireRelative('users.js');
Users.load();

const Contexts = requireRelative('contexts.js');
const CLI = requireRelative('cli.js');
CLI.init(Contexts);

// --- Telegram bot -------------------------------------

const telegramApiToken = process.env.TELEGRAM_API_TOKEN;
const bot = new Bot(telegramApiToken);

const listContexts = async (ctx) => {
  await ctx.reply("Welcome to Playtime! Here are the available contexts:", { reply_markup: menuContexts })
}

const showContext = async (ctx, contextName) => {
  ctx.session = ctx.session || {};
  ctx.session.contextName = contextName;

  const code = await Contexts.getCode(contextName);
  await ctx.reply(code, { reply_markup: menuContextDetails });
}

const startContext = async (ctx, contextName) => {
  if (Contexts.getRunning().has(contextName)) {
    ctx.reply(`Context ${contextName} is already running.`);
    return;
  }
  const user = Users.register(ctx);
  try {
    let response = await Contexts.deliverPlayerMessage(
      contextName,
      `@${user.username}`, // the '@' is needed to indicate that it's a Telegram username
      'start');
    await ctx.reply(response);
  } catch (error) {
    console.error(error.message);
    await ctx.reply(error.message);
  }
}

const menuContexts = new Menu("contexts-menu");
const menuContextDetails = new Menu("context-details-menu");

Contexts.getAvailable().then((availableContexts) => {
  availableContexts.forEach((contextName) => {
    menuContexts.text(
      contextName.charAt(0).toUpperCase() + contextName.slice(1),
      (ctx) => showContext(ctx, contextName)
    ).row();
  });

  menuContextDetails
    .back("◀ Back", async (ctx) => listContexts(ctx))
    .text("Start ▶", (ctx) => startContext(ctx, ctx.session.contextName))
    .row();

  menuContexts.register(menuContextDetails);
});

bot.catch((err) => {
  const ctx = err.ctx;
  console.error(`Error while handling update ${ctx.update.update_id}:`);
  const e = err.error;
  if (e instanceof GrammyError) {
    console.error("Error in request:", e.description);
  } else if (e instanceof HttpError) {
    console.error("Could not contact Telegram:", e);
  } else {
    console.error("Unknown error:", e);
  }
});

bot.use(session());
bot.use(menuContexts);
bot.api.config.use(hydrateFiles(bot.token));

bot.command("menu", async (ctx) => {
  await ctx.reply("Pick a context:", { reply_markup: menuContexts });
});

bot.command("start", async (ctx) => {
  const args = ctx.message.text.split(' ');
  if (args.length == 2) {
    startContext(ctx, args[1]);
  } else {
    await ctx.reply("Usage: /start <context-name>");
  }
});

bot.on("message:voice", async (ctx) => {
  // TODO: transcribe voice message
})

// Handle incoming messages from Telegram
bot.on("message:text", async (ctx) => {
  const user = Users.register(ctx);
  const text = ctx.message.text;
  console.log('Received message from Telegram:', { userId: user.id, text, username: user.username });

  if (pendingRequests.has(user.id)) {
    const { resolve } = pendingRequests.get(user.id);
    pendingRequests.delete(user.id);
    // TODO: handle photo/video/voice messages
    resolve({ text: text });
    console.log('Resolved pending request for user:', user.id);
  } else {
    listContexts(ctx);
  }
});

bot.on("message:photo", async (ctx) => {
  const user = Users.register(ctx);
  const text = ctx.message.caption;
  console.log('Received photo message from Telegram:', { userId: user.id, text, username: user.username });
  const file = await ctx.getFile();
  const path = await file.download(`assets/photos/${file.file_id}.jpg`);

  if (pendingRequests.has(user.id)) {
    const { resolve } = pendingRequests.get(user.id);
    pendingRequests.delete(user.id);
    resolve({ text: text, path: path, photo_file_id: file.file_id });
    console.log('Resolved pending request for user:', user.id);
  }
});

// --- HTTP server -------------------------------------

const app = express();
const port = 3000;

// This is a super awkward workaround to fix the duplicate
// Content-Type header issue caused by Guile's http-post.
// I've tried everything. We must set the Content-Type to
// application/json, but Guile always adds text/plain;charset=utf-8
// as a duplicate header.
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

app.get('/find-user-by-username/:username', (req, res) => {
  const { username } = req.params;
  // console.log(`Searching for user with username: ${username}`);

  const user = Users.get(username);
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

// Update the endpoint to terminate a specific context or all contexts
app.post('/terminate-context', (req, res) => {
  const { context } = req.body;
  if (context) {
    Contexts.terminate(context);
    res.status(200).json({ message: `Context ${context} terminated` });
  } else {
    Contexts.terminateAll();
    res.status(200).json({ message: 'All contexts terminated' });
  }
});

// Add an endpoint to list running contexts
app.get('/list-contexts', (req, res) => {
  const contexts = Array.from(Contexts.getRunning().keys());
  res.status(200).json({ runningContexts: contexts });
});

// -- Main -----------------------------------------

// Start the Express server
app.listen(port, () => {
  console.log(`Server listening at http://localhost:${port}`);
  CLI.printInstructions();
});

// Start the bot (using long polling)
bot.start();