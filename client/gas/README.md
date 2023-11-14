# HOW TO

## use `clasp`

```bash
npx clasp --version
npx clasp --help
```

## setup project

```bash
echo -n "<SCRIPT ID>" | xargs -0 printf "{\"scriptId\":\"%s\",\"rootDir\":\"$PWD\"}\n" | jq -c > ./.clasp.json
```

## prettier

```bash
npx prettier --write target.js
```

## push

```bash
npx clasp push
```
