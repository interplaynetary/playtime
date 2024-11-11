const fs = require('fs');
const csv = require('csv-parser');
const fastcsv = require('fast-csv');

const users = new Map();
const USER_DB_FILE = 'users.csv';

function ensureUserDBExists() {
  if (!fs.existsSync(USER_DB_FILE)) {
    fs.writeFileSync(USER_DB_FILE, 'username,id,name');
    console.log('Created new users.csv file');
  }
}

function load() {
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

function checkAndAdd(userId, name, username) {
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

function register(ctx) {
  const from = ctx.update
    ? ctx.update.callback_query
      ? ctx.update.callback_query.from
      : ctx.update.message.from
    : ctx.message.from;
  const id = from.id.toString();
  const name = `${from.first_name} ${from.last_name || ''}`.trim();
  const username = from.username || '';
  checkAndAdd(id, name, username);
  return { id, name, username };
}

function get(username) {
  return users.get(username);
}

module.exports = {
  load,
  get,
  checkAndAdd,
  register
};
