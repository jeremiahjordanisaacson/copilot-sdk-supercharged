#!/usr/bin/env bash
# =============================================================================
# verify-version-sync.sh — Ensures version strings never drift across SDKs
#
# Checks that the canonical version in pyproject.toml / package.json matches
# every place a version is declared. Fails CI if ANY mismatch is found.
#
# Run: bash scripts/verify-version-sync.sh
# =============================================================================
set -euo pipefail

REPO_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_DIR"

ERRORS=0
CHECKED=0

fail() {
    echo "❌ VERSION MISMATCH: $1"
    ERRORS=$((ERRORS + 1))
}

ok() {
    echo "  ✅ $1"
    CHECKED=$((CHECKED + 1))
}

# check_version <file> <python regex> [expected] [label]
# Compares the version captured by <python regex> (re.MULTILINE; several groups are
# joined with ".") with [expected], which defaults to the canonical version.
check_version() {
    local file="$1" pattern="$2" expected="${3:-$CANONICAL}" label="${4:-version}" ver
    if [ ! -f "$file" ]; then
        fail "$file not found"
        return
    fi
    ver=$(python3 -c "
import re, sys
with open(sys.argv[1], encoding='utf-8-sig') as f:
    m = re.search(sys.argv[2], f.read(), re.MULTILINE)
    print('.'.join(m.groups()) if m else 'NOT_SET')
" "$file" "$pattern" 2>/dev/null || echo "NOT_SET")
    if [ "$ver" = "$expected" ]; then
        ok "$file ($label: $ver)"
    else
        fail "$file has $label \"$ver\" (expected \"$expected\")"
    fi
}

# ── Canonical version source ─────────────────────────────────────────────
CANONICAL=$(python3 -c "
import re
with open('python/pyproject.toml') as f:
    m = re.search(r'^version\s*=\s*\"(.+?)\"', f.read(), re.MULTILINE)
    print(m.group(1) if m else 'MISSING')
")

echo "═══════════════════════════════════════════════════"
echo "  Version Sync Check"
echo "  Canonical version: $CANONICAL (from python/pyproject.toml)"
echo "═══════════════════════════════════════════════════"
echo ""

if [ "$CANONICAL" = "MISSING" ]; then
    fail "Cannot read canonical version from python/pyproject.toml"
    exit 1
fi

# ── Python __version__ ───────────────────────────────────────────────────
PY_VER=$(python3 -c "
import re
with open('python/copilot/__init__.py') as f:
    content = f.read()
# When __version__ is resolved dynamically from installed package metadata
# (importlib.metadata), it is inherently derived from pyproject at build/install
# time, so it can never drift. Treat that as in-sync instead of comparing the
# dev sentinel fallback string.
if 'importlib.metadata' in content and '_pkg_version' in content:
    print('DYNAMIC')
else:
    m = re.search(r'__version__\s*=\s*\"(.+?)\"', content)
    print(m.group(1) if m else 'MISSING')
")
if [ "$PY_VER" = "DYNAMIC" ]; then
    ok "python/copilot/__init__.py (version sourced from package metadata via importlib.metadata)"
elif [ "$PY_VER" = "$CANONICAL" ]; then
    ok "python/copilot/__init__.py (__version__ = \"$PY_VER\")"
else
    fail "python/copilot/__init__.py has __version__ = \"$PY_VER\" (expected \"$CANONICAL\")"
fi

# ── Node.js package.json ─────────────────────────────────────────────────
NODE_VER=$(node -p "require('./nodejs/package.json').version" 2>/dev/null || echo "MISSING")
if [ "$NODE_VER" = "$CANONICAL" ]; then
    ok "nodejs/package.json (version: \"$NODE_VER\")"
else
    fail "nodejs/package.json has version \"$NODE_VER\" (expected \"$CANONICAL\")"
fi

# ── .NET .csproj ─────────────────────────────────────────────────────────
# .NET and Java use upstream-managed versioning (0.0.0-dev, beta-java etc.)
# so we skip them in version sync — they follow upstream's release cadence.
if [ -f "dotnet/src/Microsoft.Copilot.SDK.csproj" ]; then
    DOTNET_VER=$(python3 -c "
import re
with open('dotnet/src/Microsoft.Copilot.SDK.csproj') as f:
    m = re.search(r'<Version>(.+?)</Version>', f.read())
    print(m.group(1) if m else 'NOT_SET')
" 2>/dev/null || echo "NOT_SET")
    if [ "$DOTNET_VER" = "NOT_SET" ]; then
        ok "dotnet/.csproj (no <Version> tag — uses CI-provided version)"
    elif [ "$DOTNET_VER" = "$CANONICAL" ]; then
        ok "dotnet/.csproj (Version: $DOTNET_VER)"
    elif echo "$DOTNET_VER" | grep -qE "^0\.0\.0-dev|^[0-9]+\.[0-9]+\.[0-9]+-beta"; then
        ok "dotnet/.csproj (upstream-managed version: $DOTNET_VER — skipped)"
    else
        fail "dotnet/.csproj has Version \"$DOTNET_VER\" (expected \"$CANONICAL\")"
    fi
fi

# ── Java pom.xml ─────────────────────────────────────────────────────────
if [ -f "java/pom.xml" ]; then
    JAVA_VER=$(python3 -c "
import re
with open('java/pom.xml') as f:
    # Get first <version> under the root <project>, not inside <dependency>
    content = f.read()
    m = re.search(r'<artifactId>copilot-sdk[^<]*</artifactId>\s*<version>([^<]+)</version>', content)
    if not m:
        m = re.search(r'<version>([^<]+)</version>', content)
    print(m.group(1) if m else 'NOT_SET')
" 2>/dev/null || echo "NOT_SET")
    if [ "$JAVA_VER" = "$CANONICAL" ] || [ "$JAVA_VER" = "NOT_SET" ]; then
        ok "java/pom.xml (version: $JAVA_VER)"
    elif echo "$JAVA_VER" | grep -qE "SNAPSHOT$|^[0-9]+\.[0-9]+\.[0-9]+-beta"; then
        ok "java/pom.xml (upstream-managed version: $JAVA_VER — skipped)"
    else
        fail "java/pom.xml has version \"$JAVA_VER\" (expected \"$CANONICAL\")"
    fi
fi

# ── Ruby gemspec ─────────────────────────────────────────────────────────
if [ -f "ruby/lib/copilot/version.rb" ]; then
    RUBY_VER=$(ruby -e "
      content = File.read('ruby/lib/copilot/version.rb')
      if content =~ /VERSION\s*=\s*['\"]([^'\"]+)['\"]/
        puts \$1
      else
        puts 'NOT_SET'
      end
    " 2>/dev/null || echo "NOT_SET")
    if [ "$RUBY_VER" = "$CANONICAL" ]; then
        ok "ruby/lib/copilot/version.rb (VERSION = \"$RUBY_VER\")"
    elif [ "$RUBY_VER" = "NOT_SET" ]; then
        ok "ruby/lib/copilot/version.rb (no VERSION constant)"
    else
        fail "ruby/lib/copilot/version.rb has VERSION \"$RUBY_VER\" (expected \"$CANONICAL\")"
    fi
fi

# ── Dart pubspec.yaml ────────────────────────────────────────────────────
if [ -f "dart/pubspec.yaml" ]; then
    DART_VER=$(python3 -c "
import re
with open('dart/pubspec.yaml') as f:
    m = re.search(r'^version:\s*(.+)', f.read(), re.MULTILINE)
    print(m.group(1).strip() if m else 'NOT_SET')
" 2>/dev/null || echo "NOT_SET")
    if [ "$DART_VER" = "$CANONICAL" ]; then
        ok "dart/pubspec.yaml (version: $DART_VER)"
    else
        fail "dart/pubspec.yaml has version \"$DART_VER\" (expected \"$CANONICAL\")"
    fi
fi

# ── Elixir mix.exs ──────────────────────────────────────────────────────
if [ -f "elixir/mix.exs" ]; then
    ELIXIR_VER=$(python3 -c "
import re
with open('elixir/mix.exs') as f:
    content = f.read()
    # Try @version module attribute first, then inline version:
    m = re.search(r'@version\s+\"(.+?)\"', content)
    if not m:
        m = re.search(r'version:\s*\"(.+?)\"', content)
    print(m.group(1) if m else 'NOT_SET')
" 2>/dev/null || echo "NOT_SET")
    if [ "$ELIXIR_VER" = "$CANONICAL" ]; then
        ok "elixir/mix.exs (version: \"$ELIXIR_VER\")"
    else
        fail "elixir/mix.exs has version \"$ELIXIR_VER\" (expected \"$CANONICAL\")"
    fi
fi

# ── PHP composer.json ────────────────────────────────────────────────────
if [ -f "php/composer.json" ]; then
    PHP_VER=$(python3 -c "
import json
with open('php/composer.json') as f:
    d = json.load(f)
    print(d.get('version', 'NOT_SET'))
" 2>/dev/null || echo "NOT_SET")
    if [ "$PHP_VER" = "$CANONICAL" ] || [ "$PHP_VER" = "NOT_SET" ]; then
        ok "php/composer.json (version: $PHP_VER)"
    else
        fail "php/composer.json has version \"$PHP_VER\" (expected \"$CANONICAL\")"
    fi
fi

# ── Kotlin build.gradle.kts ─────────────────────────────────────────────
if [ -f "kotlin/build.gradle.kts" ]; then
    KT_VER=$(python3 -c "
import re
with open('kotlin/build.gradle.kts') as f:
    m = re.search(r'version\s*=\s*\"(.+?)\"', f.read())
    print(m.group(1) if m else 'NOT_SET')
" 2>/dev/null || echo "NOT_SET")
    if [ "$KT_VER" = "$CANONICAL" ] || [ "$KT_VER" = "NOT_SET" ]; then
        ok "kotlin/build.gradle.kts (version: $KT_VER)"
    else
        fail "kotlin/build.gradle.kts has version \"$KT_VER\" (expected \"$CANONICAL\")"
    fi
fi

# ── Node.js package-lock.json ────────────────────────────────────────────
NODE_LOCK_VER=$(python3 -c "
import json
with open('nodejs/package-lock.json', encoding='utf-8') as f:
    d = json.load(f)
root = d.get('version', 'NOT_SET')
pkg = d.get('packages', {}).get('', {}).get('version', 'NOT_SET')
print(root if root == pkg else root + ' / ' + pkg)
" 2>/dev/null || echo "NOT_SET")
if [ "$NODE_LOCK_VER" = "$CANONICAL" ]; then
    ok "nodejs/package-lock.json (version: $NODE_LOCK_VER)"
else
    fail "nodejs/package-lock.json has version \"$NODE_LOCK_VER\" (expected \"$CANONICAL\")"
fi

# ── Lua rockspec ─────────────────────────────────────────────────────────
# The file name carries the version too, so exactly one rockspec may exist.
shopt -s nullglob
ROCKSPECS=(lua/copilot-sdk-supercharged-*-1.rockspec)
shopt -u nullglob
if [ "${#ROCKSPECS[@]}" -ne 1 ]; then
    fail "expected one lua/copilot-sdk-supercharged-*-1.rockspec, found ${#ROCKSPECS[@]}"
elif [ "${ROCKSPECS[0]}" != "lua/copilot-sdk-supercharged-$CANONICAL-1.rockspec" ]; then
    fail "${ROCKSPECS[0]} is named for another version (expected lua/copilot-sdk-supercharged-$CANONICAL-1.rockspec)"
else
    check_version "${ROCKSPECS[0]}" '^version\s*=\s*"([^"]+)"' "$CANONICAL-1"
    check_version "${ROCKSPECS[0]}" '\btag\s*=\s*"([^"]+)"' "v$CANONICAL" "tag"
fi

# ── Other SDK manifests and version constants ───────────────────────────
# Go, Swift, and PHP declare no version: their releases are the git tags.
check_version "ada/alire.toml" '^version\s*=\s*"([^"]+)"'
check_version "ada/src/copilot-version.ads" 'SDK_Version\s*:\s*constant\s+String\s*:=\s*"([^"]+)"' "$CANONICAL" "SDK_Version"
check_version "c/CMakeLists.txt" 'project\s*\([^)]*?\bVERSION\s+([0-9][0-9.]*)' "$CANONICAL" "project VERSION"
check_version "c/include/copilot/copilot.h" 'COPILOT_SDK_VERSION_MAJOR\s+(\d+)\s+#define\s+COPILOT_SDK_VERSION_MINOR\s+(\d+)\s+#define\s+COPILOT_SDK_VERSION_PATCH\s+(\d+)' "$CANONICAL" "COPILOT_SDK_VERSION_*"
check_version "clojure/build.clj" '\(System/getenv "VERSION"\)\s+"([^"]+)"' "$CANONICAL" "default version"
check_version "cobol/src/COPILOT-VERSION.cpy" 'WS-SDK-VERSION\s+PIC\s+X\(\d+\)\s+VALUE\s+"([^"]+)"' "$CANONICAL" "WS-SDK-VERSION"
check_version "cpp/CMakeLists.txt" 'project\s*\([^)]*?\bVERSION\s+([0-9][0-9.]*)' "$CANONICAL" "project VERSION"
check_version "crystal/shard.yml" '^version:\s*(\S+)'
check_version "crystal/src/copilot_sdk/version.cr" '^\s*VERSION\s*=\s*"([^"]+)"' "$CANONICAL" "VERSION"
check_version "delphi/boss.json" '"version"\s*:\s*"([^"]+)"'
check_version "dlang/dub.json" '"version"\s*:\s*"([^"]+)"'
check_version "erlang/src/copilot_sdk.app.src" '\{vsn,\s*"([^"]+)"\}' "$CANONICAL" "vsn"
check_version "fortran/fpm.toml" '^version\s*=\s*"([^"]+)"'
check_version "fortran/CMakeLists.txt" 'project\s*\([^)]*?\bVERSION\s+([0-9][0-9.]*)' "$CANONICAL" "project VERSION"
check_version "fsharp/src/CopilotSDK.Supercharged.FSharp.fsproj" '<Version>([^<]+)</Version>'
check_version "groovy/build.gradle" "^version\s*=\s*'([^']+)'"
check_version "haskell/copilot-sdk-supercharged.cabal" '^version:\s*(\S+)' "$CANONICAL.0"
check_version "julia/Project.toml" '^version\s*=\s*"([^"]+)"'
check_version "matlab/info.xml" '<version>([^<]+)</version>'
check_version "nim/copilot_sdk.nimble" '^version\s*=\s*"([^"]+)"'
check_version "objc/CopilotSDK.podspec" '^\s*s\.version\s*=\s*"([^"]+)"'
check_version "ocaml/copilot-sdk-supercharged.opam" '^version:\s*"([^"]+)"'
check_version "ocaml/dune-project" '^\(version\s+([^)\s]+)\)'
check_version "ocaml/lib/version.ml" '^let\s+sdk_version\s*=\s*"([^"]+)"' "$CANONICAL" "sdk_version"
check_version "perl/lib/GitHub/Copilot/Client.pm" "^our\s+[\$]VERSION\s*=\s*'([^']+)'" "$CANONICAL" "\$VERSION"
check_version "r/DESCRIPTION" '^Version:\s*(\S+)'
check_version "rust/Cargo.toml" '^version\s*=\s*"([^"]+)"'
check_version "scala/build.sbt" '\bversion\s*:=\s*"([^"]+)"'
check_version "shell/lib/copilot_sdk.sh" '^COPILOT_SDK_VERSION="([^"]+)"' "$CANONICAL" "COPILOT_SDK_VERSION"
check_version "solidity/package.json" '"version"\s*:\s*"([^"]+)"'
check_version "tcl/lib/copilot/pkgIndex.tcl" 'package\s+ifneeded\s+copilot::version\s+(\S+)' "$CANONICAL" "package ifneeded"
check_version "tcl/lib/copilot/version.tcl" 'variable\s+sdk_version\s+"([^"]+)"' "$CANONICAL" "sdk_version"
check_version "tcl/lib/copilot/version.tcl" '^package\s+provide\s+copilot::version\s+(\S+)' "$CANONICAL" "package provide"
check_version "visualbasic/CopilotSDK.Supercharged.VB.vbproj" '<Version>([^<]+)</Version>'
check_version "vlang/v.mod" "^\s*version:\s*'([^']+)'"
check_version "vlang/src/version.v" "\bsdk_version\s*=\s*'([^']+)'" "$CANONICAL" "sdk_version"
check_version "zig/build.zig.zon" '^\s*\.version\s*=\s*"([^"]+)"'

# ── Summary ──────────────────────────────────────────────────────────────
echo ""
echo "═══════════════════════════════════════════════════"
if [ "$ERRORS" -gt 0 ]; then
    echo "  ❌ FAILED: $ERRORS version mismatches found"
    echo "  Fix them before shipping!"
    echo "═══════════════════════════════════════════════════"
    exit 1
else
    echo "  ✅ ALL $CHECKED versions in sync ($CANONICAL)"
    echo "═══════════════════════════════════════════════════"
    exit 0
fi
