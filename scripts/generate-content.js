const { spawnSync } = require('child_process');
const path = require('path');

const repoRoot = path.resolve(__dirname, '..');

const contentGeneration = spawnSync(
  process.platform === 'win32' ? 'npx.cmd' : 'npx',
  ['elm-frontmatter', '--elm-dir=./src', '-y'],
  {
    cwd: repoRoot,
    encoding: 'utf8'
  }
);

if (contentGeneration.stdout) {
  process.stdout.write(contentGeneration.stdout);
}

if (contentGeneration.stderr) {
  process.stderr.write(contentGeneration.stderr);
}

const combinedOutput = `${contentGeneration.stdout || ''}\n${contentGeneration.stderr || ''}`;

if (
  contentGeneration.status !== 0
  || combinedOutput.includes('🚨 Content generation terminated 🚨')
  || combinedOutput.includes('Error compiling Elm program')
) {
  process.exit(contentGeneration.status || 1);
}

const registryGeneration = spawnSync(
  process.execPath,
  [path.join(__dirname, 'generate-pages-registry.js')],
  {
    cwd: repoRoot,
    encoding: 'utf8'
  }
);

if (registryGeneration.stdout) {
  process.stdout.write(registryGeneration.stdout);
}

if (registryGeneration.stderr) {
  process.stderr.write(registryGeneration.stderr);
}

process.exit(registryGeneration.status || 0);
