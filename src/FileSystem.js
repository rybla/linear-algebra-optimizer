export const read = (filename) => Bun.file(filename)

export const text_ = (file) => async () => { return await file.text() };

export const write_ = (filename) => (content) => async () => { return await Bun.write(filename, content) }

