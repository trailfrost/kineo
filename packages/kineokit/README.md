# KineoKit

Migration and schema manager for Kineo.

```sh
kineokit init # Initializes by creating a configuration file.
kineokit push # Pushes schema to database, skipping migrations. Warns you for possible breaking changes.
kineokit pull # Pulls schema from database.
kineokit migrate # Generates migrations.
kineokit status # Gets migration status for each migration file.
kineokit deploy # Deploys migrations.
```

## Installation

```sh
npm install --save-dev kineokit # OR
pnpm add --save-dev kineokit    # OR
bun add --development kineokit  # OR
yarn add --dev kineokit         # OR
```
