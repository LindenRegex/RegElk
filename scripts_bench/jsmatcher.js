// scripts_bench/jsfirstmatcher.js
// argv: [2] source, [3] flags, [4] max_groups, [5] input
const src = process.argv[2], flags = process.argv[3] || '';
const ngroups = parseInt(process.argv[4], 10) + 1;
const input = process.argv[5] ?? '';

try {
  const re = new RegExp(src, flags.replace(/[gd]/g, '') + 'd');
  const enc = new TextEncoder();
  const ascii = !/[^\x00-\x7f]/.test(input);
  const B = ascii ? i => i : i => enc.encode(input.slice(0, i)).length;

  let out = '';
  const m = input.match(re);

  if (m) {
    const regs = new Array(2 * ngroups).fill(-1);
    m.indices.forEach((p, g) => {
      if (p !== undefined && g < ngroups) { regs[2*g] = B(p[0]); regs[2*g+1] = B(p[1]); }
    });
    if (m.indices.length !== ngroups) out += 'Arity ' + m.indices.length + '\n';
    out += regs.join(' ') + '\n';
  }

  process.stdout.write(out + 'END\n');
} catch (e) {
  process.stdout.write('Error ' + String(e.message).replace(/\s+/g, ' ') + '\nEND\n');
}