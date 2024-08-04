export const spawn_ = (args) => async () => {
  const proc = Bun.spawn(args);
  const stderr = await new Response(proc.stderr).text();
  const stdout = await new Response(proc.stdout).text();
  await proc.exited;
  return { stderr, stdout }
}
