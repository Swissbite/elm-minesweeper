const fs = require('fs');
const path = require('path');
const { spawnSync } = require('child_process');

const repoRoot = path.resolve(__dirname, '..');
const contentRoot = path.join(repoRoot, 'content', 'pages');
const outputPath = path.join(repoRoot, 'src', 'Content', 'Pages', 'Registry.elm');

function walkMarkdownFiles(dir) {
  if (!fs.existsSync(dir)) {
    return [];
  }

  return fs.readdirSync(dir, { withFileTypes: true }).flatMap((entry) => {
    const fullPath = path.join(dir, entry.name);

    if (entry.isDirectory()) {
      return walkMarkdownFiles(fullPath);
    }

    if (entry.isFile() && entry.name.endsWith('.md')) {
      return [fullPath];
    }

    return [];
  });
}

function toPascalCase(segment) {
  return segment
    .replace(/^\[|\]$/g, '')
    .split(/[^a-zA-Z0-9]+/)
    .filter(Boolean)
    .map((part) => part.charAt(0).toUpperCase() + part.slice(1))
    .join('');
}

function moduleNameFromFile(filePath) {
  const relativePath = path.relative(contentRoot, filePath);
  const parsed = path.parse(relativePath);
  const segments = parsed.dir === '' ? [] : parsed.dir.split(path.sep);
  const fileSegment = parsed.name === 'content' ? segments.pop() : parsed.name;

  return ['Content', 'Pages', ...segments.map(toPascalCase), toPascalCase(fileSegment)].join('.');
}

function slugFromFile(filePath) {
  const source = fs.readFileSync(filePath, 'utf8');
  const frontmatterMatch = source.match(/^---\r?\n([\s\S]*?)\r?\n---/);

  if (!frontmatterMatch) {
    throw new Error(`Missing frontmatter in ${path.relative(repoRoot, filePath)}`);
  }

  const slugLine = frontmatterMatch[1]
    .split(/\r?\n/)
    .find((line) => line.trim().startsWith('slug:'));

  if (!slugLine) {
    throw new Error(`Missing slug in ${path.relative(repoRoot, filePath)}`);
  }

  return slugLine
    .split(':')
    .slice(1)
    .join(':')
    .trim()
    .replace(/^['"]|['"]$/g, '');
}

function registryEntry(aliasName) {
  return `    { title = ${aliasName}.content.title
    , slug = ${aliasName}.content.slug
    , order = ${aliasName}.content.order
    , published = ${aliasName}.content.published
    , description = ${aliasName}.content.description
    , lang = ${aliasName}.content.lang
    , updatedAt = ${aliasName}.content.updatedAt
    , body = ${aliasName}.content.body
    }`;
}

const markdownFiles = walkMarkdownFiles(contentRoot).sort();
const seenSlugs = new Map();

markdownFiles.forEach((filePath) => {
  const slug = slugFromFile(filePath);
  const existing = seenSlugs.get(slug);

  if (existing) {
    throw new Error(`Duplicate slug "${slug}" in ${path.relative(repoRoot, existing)} and ${path.relative(repoRoot, filePath)}`);
  }

  seenSlugs.set(slug, filePath);
});

const imports = markdownFiles.map((filePath, index) => `import ${moduleNameFromFile(filePath)} as Page${index + 1}`);
const entries = markdownFiles.map((_, index) => registryEntry(`Page${index + 1}`));

const output = `{-
   This file is part of Elm Minesweeper.

   Elm Minesweeper is free software: you can redistribute it and/or modify it under
   the terms of the GNU Affero General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later version.

   Elm Minesweeper is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
   PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License along with
   Elm Minesweeper. If not, see <https://www.gnu.org/licenses/>.

-}


module Content.Pages.Registry exposing (all)

${imports.join('\n')}


all =
${entries.length === 0 ? '    []\n' : `    [\n${entries.join('\n    , ')}\n    ]\n`}`;

fs.mkdirSync(path.dirname(outputPath), { recursive: true });
fs.writeFileSync(outputPath, output);

const elmFormatCommand = process.platform === 'win32' ? 'npx.cmd' : 'npx';
const elmFormat = spawnSync(
  elmFormatCommand,
  ['elm-format', '--elm-version=0.19', '--yes', outputPath],
  {
    cwd: repoRoot,
    encoding: 'utf8'
  }
);

if (elmFormat.status !== 0 && elmFormat.error && elmFormat.error.code !== 'ENOENT') {
  throw elmFormat.error;
}
