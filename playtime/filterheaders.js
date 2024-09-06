function filterDuplicateContentType(req, res, next) {
  const rawHeaders = req.rawHeaders;

  let filteredHeaders = [];
  let lastContentType = null;

  // Iterate through the raw headers and keep the last 'Content-Type'
  for (let i = 0; i < rawHeaders.length; i += 2) {
    const headerName = rawHeaders[i].toLowerCase();
    const headerValue = rawHeaders[i + 1];

    if (headerName === 'content-type') {
      lastContentType = headerValue;  // Always overwrite with the last Content-Type
    } else {
      filteredHeaders.push(rawHeaders[i], rawHeaders[i + 1]);  // Keep other headers as they are
    }
  }

  // If a Content-Type was found, add it to the filtered headers
  if (lastContentType) {
    filteredHeaders.push('Content-Type', lastContentType);
    req.headers['content-type'] = lastContentType;
  }

  req.rawHeaders = filteredHeaders;

  next();
}

module.exports = filterDuplicateContentType;
