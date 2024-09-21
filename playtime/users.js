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
  const writer = fcreateWriteStream(USER_DB_FILE, { flags: 'a' });
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

module.exports = {
  load,
  checkAndAdd
};
