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
    m = re.search(r'__version__\s*=\s*\"(.+?)\"', f.read())
    print(m.group(1) if m else 'MISSING')
")
if [ "$PY_VER" = "$CANONICAL" ]; then
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
if [ -f "dotnet/src/GitHub.Copilot.SDK.csproj" ]; then
    DOTNET_VER=$(python3 -c "
import re
with open('dotnet/src/GitHub.Copilot.SDK.csproj') as f:
    m = re.search(r'<Version>(.+?)</Version>', f.read())
    print(m.group(1) if m else 'NOT_SET')
" 2>/dev/null || echo "NOT_SET")
    if [ "$DOTNET_VER" = "NOT_SET" ]; then
        ok "dotnet/.csproj (no <Version> tag — uses CI-provided version)"
    elif [ "$DOTNET_VER" = "$CANONICAL" ]; then
        ok "dotnet/.csproj (Version: $DOTNET_VER)"
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
