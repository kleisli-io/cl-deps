# Package a buildLisp.program into a self-contained, relocatable directory tree.
# On Linux the only host contract is the Linux syscall ABI; on macOS it is the
# system dynamic linker at /usr/lib/dyld -- a stable OS path present on every Mac.
#
# A save-lisp-and-die image carries its Lisp core appended to the executable,
# which the runtime locates by scanning itself; rewriting the executable
# (patchelf on ELF, install_name_tool on Mach-O) shifts that and breaks core
# lookup -- and on macOS also invalidates the code signature the kernel demands.
# So the image is shipped byte-for-byte on both platforms and its baked
# /nix/store references are left intact; the generated launcher overrides library
# resolution at runtime instead, with the bundle's lib/ on the search path.
#
#   Linux: the image's PT_INTERP is an absolute /nix/store loader the kernel
#   resolves literally (ignoring LD_LIBRARY_PATH), so with /nix/store absent
#   execve itself fails. The bundled loader is invoked explicitly to sidestep it.
#
#   macOS: dyld lives at /usr/lib/dyld regardless of /nix/store, so there is no
#   interpreter problem. The image's LC_LOAD_DYLIB entries are absolute /nix/store
#   paths, but dyld searches DYLD_LIBRARY_PATH by leaf name -- before the
#   load-command path -- so a bundled lib/ shadows the dead store references with
#   no rewriting (and thus no broken core, no invalidated signature).
#
# The complete runtime closure travels with the artifact:
#
#   bin/<launcher>          /bin/sh trampoline (no store path)
#   bin/.<name>-wrapped     (Linux only) the raw dumped image (untouched)
#   libexec/<launcher>      the exec'd entry: on Linux a copy of the dynamic
#                           loader, on macOS the raw dumped image itself
#   lib/<loader>            (Linux only) the image's own dynamic loader
#   lib/<deps...>           transitively-linked shared objects (ldd / otool -L)
#   lib/<dlopen sonames>    libs the image reopens by SONAME at boot (see below)
#   share/...               resource roots (KLI_DATA_DIR points here)
#
# The kernel names a process (comm, cmdline[0] -- what ps/top/tmux show) after
# the execve pathname, so the exec'd entry lives in libexec/ under the
# launcher's name. The loader still passes the image path as the application's
# argv[0], so runtime core lookup is unaffected.
#
# The dlopen'd set is NOT in the load commands (the image loads it via dlopen),
# so ldd/otool cannot see it. Rather than hand-list sonames -- which rots when
# the closure moves -- the set is read from the image itself: `dlopenProbe` is a
# sibling program that prints `blessed-sonames: a, b, c`, the same source of
# truth the runtime reopen hook uses. Each soname is then resolved against the
# program's own Nix closure, so the shipped file is exactly the one the image
# loaded.

{ pkgs, lib, validateSpec }:

{ name
, program
, share ? null
, dataDir ? null
, dlopenProbe ? null
, launcherName ? name
}:

let
  _validated = validateSpec {
    kind = "relocatableBundle";
    inherit name;
    spec = { inherit name program share dataDir dlopenProbe launcherName; };
  };

  inherit (pkgs) runCommand;
  isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
  imageName = ".${name}-wrapped";

  launcher = pkgs.writeText "${name}-launcher" (''
    #!/bin/sh
    _dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
    _root=$(CDPATH= cd -- "$_dir/.." && pwd)
  '' + lib.optionalString (dataDir != null) ''
    export KLI_DATA_DIR="''${KLI_DATA_DIR:-$_root/${dataDir}}"
  '' + (if isDarwin then ''
    export DYLD_LIBRARY_PATH="$_root/lib''${DYLD_LIBRARY_PATH:+:$DYLD_LIBRARY_PATH}"
    exec "$_root/libexec/${launcherName}" -- "$@"
  '' else ''
    exec "$_root/libexec/${launcherName}" --library-path "$_root/lib" "$_dir/${imageName}" -- "$@"
  ''));
in
builtins.seq _validated (runCommand "${name}-relocatable"
  ({
    __structuredAttrs = true;
    exportReferencesGraph.closure =
      [ program ] ++ lib.optional (dlopenProbe != null) dlopenProbe.drv;
    nativeBuildInputs = [ pkgs.jq ]
      ++ (if isDarwin then [ pkgs.darwin.cctools ]
          else [ pkgs.patchelf pkgs.glibc.bin ]);
    storeDir = builtins.storeDir;
    inherit imageName launcherName;
    passthru = { inherit program; };
  })
  (''
    set -euo pipefail
    mkdir -p "$out/bin" "$out/lib" "$out/libexec" "$out/share"

    img=${program}/bin/${imageName}

  '' + (if isDarwin then ''
    cp "$img" "$out/libexec/$launcherName"
    chmod u+w "$out/libexec/$launcherName"
    chmod +x "$out/libexec/$launcherName"

    # 1+2. Transitive LC_LOAD_DYLIB closure, keyed by leaf name. otool -L lists
    #      only DIRECT load commands (unlike ldd, which is transitive), so walk
    #      to a fixpoint. Only /nix/store dylibs are bundled; /usr/lib and
    #      /System dylibs live in dyld's shared cache and must never be shipped.
    #      No loader is bundled -- dyld at /usr/lib/dyld is always present.
    queue=$(otool -L "$out/libexec/$launcherName" | awk 'NR>1 {print $1}')
    seen=" "
    while [ -n "$queue" ]; do
      next=""
      for path in $queue; do
        case "$path" in "$storeDir"/*) ;; *) continue ;; esac
        leaf=$(basename "$path")
        case "$seen" in *" $leaf "*) continue ;; esac
        seen="$seen$leaf "
        cp -L "$path" "$out/lib/$leaf"
        next="$next $(otool -L "$path" | awk 'NR>1 {print $1}')"
      done
      queue="$next"
    done
  '' else ''
    cp "$img" "$out/bin/$imageName"
    chmod u+w "$out/bin/$imageName"
    chmod +x "$out/bin/$imageName"

    # 1. The image's own dynamic loader (its PT_INTERP target), invoked
    #    explicitly rather than rewriting the image's interpreter. libexec/
    #    holds the exec'd copy; the lib/ copy satisfies glibc's DT_NEEDED.
    interp=$(patchelf --print-interpreter "$img")
    loader=$(basename "$interp")
    cp -L "$interp" "$out/lib/$loader"
    cp -L "$interp" "$out/libexec/$launcherName"

    # 2. Transitive DT_NEEDED, resolved against the build closure (ldd works here
    #    because /nix/store is present at build time). Keyed by SONAME.
    ldd "$img" | awk '/=>/ && $3 ~ /^\// { print $1, $3 }' | while read -r soname path; do
      case "$soname" in */*) continue ;; esac
      cp -L "$path" "$out/lib/$soname"
    done
  '') + ''

    # 3. dlopen'd libs the image reopens by SONAME at boot. Sonames come from the
    #    image's own probe (the runtime source of truth); each is resolved from
    #    the program's Nix closure so the shipped file matches what was loaded.
    ${lib.optionalString (dlopenProbe != null) ''
      mapfile -t closure < <(jq -r '.closure | map(.path) | .[]' "$NIX_ATTRS_JSON_FILE")
      sonames=$(${dlopenProbe.drv}/bin/${dlopenProbe.bin} 2>/dev/null \
                  | sed -n 's/^blessed-sonames:[[:space:]]*//p' | tr ',' ' ' || true)
      for soname in $sonames; do
        soname=$(printf '%s' "$soname" | tr -d '[:space:]')
        [ -n "$soname" ] || continue
        [ -e "$out/lib/$soname" ] && continue
        found=$(find "''${closure[@]}" -name "$soname" -print -quit 2>/dev/null || true)
        if [ -z "$found" ]; then
          echo "mkRelocatableBundle: blessed soname '$soname' not found in closure" >&2
          exit 1
        fi
        cp -L "$found" "$out/lib/$soname"
      done
    ''}

    ${lib.optionalString isDarwin ''
      # Transitively close the bundled set. The libs gathered above (the image's
      # own load commands and the dlopen'd blessed libs) may themselves pull
      # further /nix/store dylibs by absolute path (e.g. sqlite -> libz); ship
      # those too, keyed by leaf, so DYLD_LIBRARY_PATH resolves every sibling to
      # the bundle rather than back into /nix/store.
      changed=1
      while [ "$changed" = 1 ]; do
        changed=0
        for dylib in "$out"/lib/*; do
          [ -e "$dylib" ] || continue
          for path in $(otool -L "$dylib" | awk 'NR>1 {print $1}'); do
            case "$path" in "$storeDir"/*) ;; *) continue ;; esac
            leaf=$(basename "$path")
            [ -e "$out/lib/$leaf" ] && continue
            cp -L "$path" "$out/lib/$leaf"
            changed=1
          done
        done
      done
    ''}

    ${lib.optionalString (!isDarwin) ''
      # Close DT_NEEDED over lib/ to a fixpoint: dlopen'd blessed libs may
      # need sonames the image itself never links (e.g. sqlite -> libz),
      # invisible to step 2's ldd of the image. Unresolved sonames fail.
      changed=1
      while [ "$changed" = 1 ]; do
        changed=0
        for so in "$out"/lib/*; do
          needed=$(patchelf --print-needed "$so" 2>/dev/null) || continue
          resolved=$(ldd "$so" 2>/dev/null | awk '/=>/ && $3 ~ /^\// { print $1, $3 }' || true)
          for soname in $needed; do
            case "$soname" in */*) continue ;; esac
            [ -e "$out/lib/$soname" ] && continue
            path=$(printf '%s\n' "$resolved" | awk -v s="$soname" '$1 == s { print $2 }')
            if [ -z "$path" ]; then
              echo "mkRelocatableBundle: cannot resolve DT_NEEDED '$soname' of $so" >&2
              exit 1
            fi
            cp -L "$path" "$out/lib/$soname"
            changed=1
          done
        done
      done
    ''}

    # 4. Resources.
    ${lib.optionalString (share != null) ''
      cp -r ${share}/share/. "$out/share/"
    ''}

    # 5. Launcher.
    cp ${launcher} "$out/bin/$launcherName"
    chmod u+w "$out/bin/$launcherName"
    chmod +x "$out/bin/$launcherName"
  ''))
