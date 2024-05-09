# rvn
Package manager for Ravenports


## How to populate the submodules

First time (from root)

```
git submodule update --init
```

Subsequent times (from root)

```
git submodule update --recursive --remote
```

## Incomplete commands

### Version

- Implement remote repository fetch and import
- Query which packages are installed
- Compare installed packages to snapshot/remote/release packages
- filter by <, >, =, ?, !
- match against pattern [cegix]
- match against package name

### Info

- Implement query on installed package database