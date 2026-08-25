#!/usr/bin/env bash
#
# install.sh - Set up a Mac to render the dashboards in this repo.
#
# Installs the system libraries the R graphics stack needs, R at the version
# renv.lock pins, Quarto, the AWS CLI, a project-local Python venv holding the
# pinned boto3 that RAthena drives through reticulate, and the R packages.
#
# Safe to re-run. Anything already present is reported and skipped, including
# tools installed outside Homebrew.
#
# Usage:
#   ./install.sh [--dry-run]
#
# --dry-run prints what would happen and changes nothing.
#
# Afterwards:
#   1. Fill in .env (the script copies .env.example if you have no .env yet).
#   2. aws sso login --profile cl-data-admin
#   3. quarto render dashboards/experience-monitoring

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

dry_run=false
while [[ $# -gt 0 ]]; do
  case "$1" in
    --dry-run) dry_run=true; shift ;;
    # Print the header block only: skip the shebang, stop at the first
    # non-comment line.
    -h|--help) awk 'NR>1 && !/^#/{exit} NR>1{sub(/^# ?/,""); print}' "$0"; exit 0 ;;
    *) echo "Unknown option: $1" >&2; exit 1 ;;
  esac
done

# The Quarto the render workflow pins.
QUARTO_PINNED="1.9.37"

# What magick, ragg, systemfonts and textshaping link against, plus the usual
# dependencies of R packages that build from source.
BREW_FORMULAE=(
  pkg-config
  imagemagick      # magick, for the logo overlay in common/branding.R
  freetype         # systemfonts / ragg
  fontconfig       # systemfonts / ragg
  harfbuzz         # textshaping
  fribidi          # textshaping
  libpng           # ragg
  libtiff          # ragg
  jpeg-turbo       # ragg
  webp             # ragg
  libxml2          # xml2
  openssl@3        # openssl, curl
)

step()  { printf '\n\033[1m==> %s\033[0m\n' "$1"; }
info()  { printf '    %s\n' "$1"; }
warn()  { printf '    \033[33mwarning:\033[0m %s\n' "$1"; }
run()   { if $dry_run; then printf '    [dry run] %s\n' "$*"; else "$@"; fi; }

# ---------------------------------------------------------------------------

step "Checking prerequisites"

if [[ "$(uname -s)" != "Darwin" ]]; then
  echo "This script is for macOS. On Linux, follow the apt list in the render workflow." >&2
  exit 1
fi

if ! command -v brew >/dev/null 2>&1; then
  cat >&2 <<'EOF'
Homebrew is not installed, and this script will not install it for you: it
changes ownership of directories outside this project and deserves a deliberate
decision. Install it from https://brew.sh, then re-run this script.
EOF
  exit 1
fi
info "Homebrew $(brew --version | head -1 | awk '{print $2}')"

# R's version is pinned in renv.lock, and renv warns when the running R differs.
r_pinned="$(/usr/bin/python3 -c 'import json;print(json.load(open("renv.lock"))["R"]["Version"])')"
info "renv.lock pins R ${r_pinned}"

# ---------------------------------------------------------------------------

step "System libraries (Homebrew)"

# One query, rather than one `brew list` per formula.
installed="$(brew list --formula)"
for formula in "${BREW_FORMULAE[@]}"; do
  if grep -qx "$formula" <<<"$installed"; then
    info "$formula already installed"
  else
    run brew install "$formula"
  fi
done

# ---------------------------------------------------------------------------

step "R"

if command -v R >/dev/null 2>&1; then
  r_have="$(R --version | head -1 | awk '{print $3}')"
  info "R ${r_have} already installed at $(command -v R)"
  if [[ "$r_have" != "$r_pinned" ]]; then
    warn "R ${r_have} does not match the ${r_pinned} in renv.lock."
    warn "renv will still restore, but package builds may differ. To match exactly:"
    warn "  brew install --cask r-lib/rig/rig && rig add ${r_pinned}"
  fi
else
  # rig installs a named R version; brew's r formula only gives the newest.
  info "R not found; installing rig to get the pinned version"
  run brew install --cask r-lib/rig/rig
  run rig add "$r_pinned"
fi

# ---------------------------------------------------------------------------

step "Quarto"

if command -v quarto >/dev/null 2>&1; then
  q_have="$(quarto --version)"
  info "Quarto ${q_have} already installed at $(command -v quarto)"
  if [[ "$q_have" != "$QUARTO_PINNED" ]]; then
    warn "Quarto ${q_have} differs from the ${QUARTO_PINNED} this repo renders against."
  fi
else
  run brew install --cask quarto
fi

# ---------------------------------------------------------------------------

step "AWS CLI"

if command -v aws >/dev/null 2>&1; then
  info "$(aws --version 2>&1 | awk '{print $1}') already installed at $(command -v aws)"
else
  run brew install awscli
fi

# ---------------------------------------------------------------------------

step "Python environment for RAthena"

# Project-local, because requirements.txt pins boto3 exactly and a venv shared
# across projects cannot honour that pin.
if [[ ! -d .venv ]]; then
  run /usr/bin/python3 -m venv .venv
else
  info ".venv already exists"
fi

run ./.venv/bin/python -m pip install --quiet --upgrade pip
run ./.venv/bin/python -m pip install --quiet -r requirements.txt
$dry_run || info "boto3 $(./.venv/bin/python -c 'import boto3;print(boto3.__version__)')"

$dry_run || info "common/setup.R points reticulate at this venv at render time"

# ---------------------------------------------------------------------------

step "R packages (renv)"

if $dry_run; then
  info "[dry run] would run renv::restore()"
else
  # renv bootstraps itself from renv/activate.R on startup.
  Rscript -e 'renv::restore(prompt = FALSE)'
fi

# ---------------------------------------------------------------------------

step "Connection config"

if [[ -f .env ]]; then
  info ".env already exists, leaving it alone"
elif $dry_run; then
  info "[dry run] would copy .env.example to .env"
else
  cp .env.example .env
  info "copied .env.example to .env - fill in ATHENA_S3_STAGING_DIR before rendering"
fi

# ---------------------------------------------------------------------------

step "Done"
cat <<EOF
    Next:
      1. Check .env has the right ATHENA_S3_STAGING_DIR.
      2. aws sso login --profile cl-data-admin
      3. quarto render dashboards/experience-monitoring
EOF
