## Getting started (R + renv)

These steps install the exact **R version recorded in `renv.lock`**, then restore all packages with **{renv}**. Run them **from the repo root** (the folder that contains `renv.lock`).

### macOS

**Option A — Homebrew (recommended)**

```bash
# 1) Install rig (R version manager)
brew tap r-lib/rig
brew install --cask rig

# 2) From the repo root, detect the R version in renv.lock (requires Python 3)
R_VERSION=$(python3 - <<'PY'
import json,sys; print(json.load(open("renv.lock"))["R"]["Version"])
PY
)

# If python3 is not available, use jq instead:
# R_VERSION=$(jq -r '.R.Version' renv.lock)

# 3) Install that R and set it as default
rig add "$R_VERSION"
rig default "$R_VERSION"
rig list  # confirms your default

# 4) Restore packages (fast CRAN via Posit Public Package Manager)
R -q -e "options(repos=c(CRAN='https://packagemanager.posit.co/cran/latest')); if(!'renv' %in% rownames(installed.packages())) install.packages('renv'); renv::restore(); renv::status()"
```

**Option B — Download installer (if you don’t use Homebrew)**
Install **rig** from the releases page, then continue at step 2 above with the same commands.

---


### Windows (PowerShell)

```powershell
# 1) Install rig (R version manager)
winget install -e --id Posit.rig

# 2) From the repo root, detect the R version in renv.lock
$R_VERSION = (Get-Content renv.lock | ConvertFrom-Json).R.Version

# 3) Install that R and set it as default
rig add $R_VERSION
rig default $R_VERSION
rig list   # confirms your default

# 4) Restore packages (fast CRAN via Posit Public Package Manager)
R -q -e "options(repos=c(CRAN='https://packagemanager.posit.co/cran/latest')); if(!'renv' %in% rownames(installed.packages())) install.packages('renv'); renv::restore(); renv::status()"
```

> If `R` isn’t on your PATH after installing rig, open a new PowerShell window (rig updates PATH) or run `rig r R -q -e "<code>"` instead of `R -q -e "<code>"`.

---


### Linux (Debian/Ubuntu)

```bash
# 1) Install rig (R version manager)
curl -L https://rig.r-pkg.org/deb/rig.gpg | sudo tee /etc/apt/trusted.gpg.d/rig.gpg >/dev/null
echo "deb http://rig.r-pkg.org/deb rig main" | sudo tee /etc/apt/sources.list.d/rig.list
sudo apt-get update && sudo apt-get install -y rig

# 2) From the repo root, detect the R version in renv.lock (requires Python 3)
R_VERSION=$(python3 - <<'PY'
import json,sys; print(json.load(open("renv.lock"))["R"]["Version"])
PY
)

# If python3 is not available, use jq instead:
# sudo apt-get install -y jq
# R_VERSION=$(jq -r '.R.Version' renv.lock)

# 3) Install that R and set it as default
rig add "$R_VERSION"
rig default "$R_VERSION"
rig list  # confirms your default

# 4) Restore packages (fast CRAN via Posit Public Package Manager)
R -q -e "options(repos=c(CRAN='https://packagemanager.posit.co/cran/latest')); if(!'renv' %in% rownames(installed.packages())) install.packages('renv'); renv::restore(); renv::status()"
```

---


### Verify everything worked

```bash
R -q -e "cat(R.version.string, '\n'); renv::status()"
```

You should see the **R version** from `renv.lock` and `renv::status()` reporting a synchronized project library. If you hit a network or permissions error, re‑run the restore command once more; transient network hiccups are common. If it persists, post the error message on the course forum.
